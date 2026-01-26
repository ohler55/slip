// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"errors"
	"fmt"
	"io"
	"net"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/swank"
)

// Connection represents a single SLY connection with channel support.
type Connection struct {
	id         int64
	conn       net.Conn
	server     *Server
	scope      *slip.Scope
	currentPkg *slip.Package
	writeMu    sync.Mutex

	// Channel support for multiple REPLs
	channels    map[int64]*Channel
	channelsMu  sync.RWMutex
	nextChanID  atomic.Int64
	defaultChan *Channel

	// Output capture stream
	outputStream *SlynkOutputStream
}

// NewConnection creates a connection for an accepted socket.
func NewConnection(id int64, conn net.Conn, server *Server) *Connection {
	// Create a child scope for this connection
	scope := server.baseScope.NewScope()

	c := &Connection{
		id:         id,
		conn:       conn,
		server:     server,
		scope:      scope,
		currentPkg: slip.FindPackage("cl-user"),
		channels:   make(map[int64]*Channel),
	}

	// Create output stream that sends to SLY
	c.outputStream = NewSlynkOutputStream(c)

	// Bind standard output to our capture stream
	scope.Let(slip.Symbol("*standard-output*"), c.outputStream)

	// Create a default channel for non-mrepl operations
	c.defaultChan = c.CreateChannel("default")

	return c
}

// Run starts the connection read loop.
func (c *Connection) Run() {
	defer func() {
		c.Close()
		c.server.removeConnection(c.id)
	}()

	for {
		msg, err := swank.ReadWireMessage(c.conn, c.scope)
		if err != nil {
			if !errors.Is(err, io.EOF) {
				LogError("connection %d read error: %v", c.id, err)
			}
			return
		}

		if msg == nil {
			continue
		}

		LogWire("<-", msg)
		c.dispatch(msg)
	}
}

// Close closes the connection and all its channels.
func (c *Connection) Close() {
	c.channelsMu.Lock()
	for _, ch := range c.channels {
		ch.Close()
	}
	c.channelsMu.Unlock()

	c.conn.Close()
}

// Send writes a message to the connection.
func (c *Connection) Send(msg slip.Object) error {
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	LogWire("->", msg)
	return swank.WriteWireMessage(c.conn, msg)
}

// SendChannelMessage sends a message on a specific channel.
func (c *Connection) SendChannelMessage(chanID int64, msg slip.Object) error {
	wrapped := slip.List{
		slip.Symbol(":channel-send"),
		slip.Fixnum(chanID),
		msg,
	}
	return c.Send(wrapped)
}

// CreateChannel creates a new channel for this connection.
func (c *Connection) CreateChannel(name string) *Channel {
	id := c.nextChanID.Add(1)
	ch := NewChannel(id, c, name)

	c.channelsMu.Lock()
	c.channels[id] = ch
	c.channelsMu.Unlock()

	return ch
}

// GetChannel retrieves a channel by ID.
func (c *Connection) GetChannel(id int64) *Channel {
	c.channelsMu.RLock()
	defer c.channelsMu.RUnlock()
	return c.channels[id]
}

// CloseChannel closes and removes a channel.
func (c *Connection) CloseChannel(id int64) {
	c.channelsMu.Lock()
	if ch, ok := c.channels[id]; ok {
		ch.Close()
		delete(c.channels, id)
	}
	c.channelsMu.Unlock()
}

// DefaultChannel returns the default channel.
func (c *Connection) DefaultChannel() *Channel {
	return c.defaultChan
}

// dispatch routes a message to the appropriate handler.
func (c *Connection) dispatch(msg slip.Object) {
	list, ok := msg.(slip.List)
	if !ok || len(list) == 0 {
		return
	}

	msgType, ok := list[0].(slip.Symbol)
	if !ok {
		return
	}

	switch msgType {
	case ":emacs-rex":
		c.handleEmacsRex(list)
	case ":emacs-channel-send":
		c.handleChannelSend(list)
	case ":emacs-interrupt":
		c.handleEmacsInterrupt(list)
	default:
		LogError("unknown message type: %s", msgType)
	}
}

// handleEmacsRex processes an :emacs-rex RPC request.
// Format: (:emacs-rex form package thread continuation)
func (c *Connection) handleEmacsRex(msg slip.List) {
	if len(msg) < 5 {
		return
	}

	form := msg[1]
	pkgName := ""
	if s, ok := msg[2].(slip.String); ok {
		pkgName = string(s)
	} else if s, ok := msg[2].(slip.Symbol); ok {
		pkgName = string(s)
	}
	// thread is msg[3] - we currently ignore it
	cont := slip.Fixnum(0)
	if cn, ok := msg[4].(slip.Fixnum); ok {
		cont = cn
	}

	// Set current package if specified
	if pkgName != "" && pkgName != "nil" {
		if pkg := slip.FindPackage(pkgName); pkg != nil {
			c.currentPkg = pkg
			c.defaultChan.SetPackage(pkg)
		}
	}

	// Extract handler name from form
	formList, ok := form.(slip.List)
	if !ok || len(formList) == 0 {
		c.sendAbort(cont, "invalid form")
		return
	}

	handlerName, ok := formList[0].(slip.Symbol)
	if !ok {
		c.sendAbort(cont, "invalid handler name")
		return
	}

	// Look up and execute handler
	handler := GetHandler(string(handlerName))
	if handler == nil {
		LogError("unknown handler: %s", handlerName)
		c.sendAbort(cont, fmt.Sprintf("unknown handler: %s", handlerName))
		return
	}

	LogDispatch(string(handlerName), formList[1:])

	// Execute handler with panic recovery
	var result slip.Object
	func() {
		defer func() {
			if r := recover(); r != nil {
				if p, ok := r.(*slip.Panic); ok {
					c.sendAbort(cont, p.Message)
				} else {
					c.sendAbort(cont, fmt.Sprintf("%v", r))
				}
				result = nil
			}
		}()
		result = handler(c, formList[1:])
	}()

	if result != nil || recover() == nil {
		c.sendReturn(cont, result)
	}
}

// handleChannelSend handles a message sent to a specific channel.
// Format: (:emacs-channel-send channel-id message)
func (c *Connection) handleChannelSend(msg slip.List) {
	if len(msg) < 3 {
		return
	}

	chanID, ok := msg[1].(slip.Fixnum)
	if !ok {
		return
	}

	ch := c.GetChannel(int64(chanID))
	if ch == nil {
		return
	}

	// The message is typically another list with an operation
	innerMsg := msg[2]
	innerList, ok := innerMsg.(slip.List)
	if !ok || len(innerList) == 0 {
		return
	}

	// Handle channel-specific operations
	op, ok := innerList[0].(slip.Symbol)
	if !ok {
		return
	}

	switch op {
	case ":process":
		// Evaluate code in the channel
		if len(innerList) >= 2 {
			if code, ok := innerList[1].(slip.String); ok {
				c.evalInChannel(ch, string(code))
			}
		}
	case ":teardown":
		c.CloseChannel(int64(chanID))
	}
}

// handleEmacsInterrupt handles interrupt requests.
func (c *Connection) handleEmacsInterrupt(msg slip.List) {
	// TODO: implement interrupt handling
}

// sendReturn sends a successful return message.
// Format: (:return (:ok value) continuation)
func (c *Connection) sendReturn(cont slip.Fixnum, value slip.Object) {
	msg := slip.List{
		slip.Symbol(":return"),
		slip.List{slip.Symbol(":ok"), value},
		cont,
	}
	_ = c.Send(msg)
}

// sendAbort sends an abort message.
// Format: (:return (:abort reason) continuation)
func (c *Connection) sendAbort(cont slip.Fixnum, reason string) {
	msg := slip.List{
		slip.Symbol(":return"),
		slip.List{slip.Symbol(":abort"), slip.String(reason)},
		cont,
	}
	_ = c.Send(msg)
}

// WriteString sends a :write-string message to SLY.
func (c *Connection) WriteString(text string) {
	msg := slip.List{
		slip.Symbol(":write-string"),
		slip.String(text),
	}
	_ = c.Send(msg)
}

// WriteResult sends a :write-string message with :repl-result flag.
func (c *Connection) WriteResult(text string) {
	msg := slip.List{
		slip.Symbol(":write-string"),
		slip.String(text + "\n"),
		slip.Symbol(":repl-result"),
	}
	_ = c.Send(msg)
}

// NewPackage sends a :new-package message to SLY.
func (c *Connection) NewPackage(name, prompt string) {
	msg := slip.List{
		slip.Symbol(":new-package"),
		slip.String(name),
		slip.String(prompt),
	}
	_ = c.Send(msg)
}

// evalInChannel evaluates code in a specific channel's context.
func (c *Connection) evalInChannel(ch *Channel, source string) {
	defer func() {
		if r := recover(); r != nil {
			if p, ok := r.(*slip.Panic); ok {
				_ = ch.Send(slip.List{
					slip.Symbol(":write-string"),
					slip.String(fmt.Sprintf("Error: %s\n", p.Message)),
				})
			} else {
				_ = ch.Send(slip.List{
					slip.Symbol(":write-string"),
					slip.String(fmt.Sprintf("Error: %v\n", r)),
				})
			}
		}
	}()

	// Parse the source
	code := slip.Read([]byte(source), ch.Scope())
	if len(code) == 0 {
		return
	}

	// Compile
	code.Compile()

	// Track the form for history
	var form slip.Object
	if len(code) > 0 {
		form = code[0]
	}

	// Evaluate
	result := code.Eval(ch.Scope(), nil)

	// Log evaluation if verbose
	LogEval(form, result)

	// Update history
	ch.UpdateREPLHistory(result, form)

	// Get history index for display
	histIdx := ch.HistoryLength()

	// Send result back on channel
	_ = ch.Send(slip.List{
		slip.Symbol(":write-values"),
		slip.List{
			slip.List{
				slip.String(slip.ObjectString(result)),
				slip.Fixnum(histIdx),
				slip.String(slip.ObjectString(result)),
			},
		},
	})

	// Send prompt
	_ = ch.Send(slip.List{
		slip.Symbol(":prompt"),
		slip.String(ch.Package().Name),
		slip.String(ch.Package().Name),
		slip.Fixnum(0),
		slip.Fixnum(histIdx),
	})
}
