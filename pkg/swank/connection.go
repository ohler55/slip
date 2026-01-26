// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"errors"
	"fmt"
	"io"
	"net"
	"sync"

	"github.com/ohler55/slip"
)

// Connection represents a single SLIME connection.
type Connection struct {
	id         int64
	conn       net.Conn
	server     *Server
	scope      *slip.Scope
	currentPkg *slip.Package
	writeMu    sync.Mutex

	// Output capture stream
	outputStream *SwankOutputStream

	// History tracking
	lastValues [3]slip.Object // *, **, ***
	lastForms  [3]slip.Object // +, ++, +++

	// Inspector state
	inspector *Inspector
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
	}

	// Create output stream that sends to SLIME
	c.outputStream = NewSwankOutputStream(c)

	// Bind standard output to our capture stream
	scope.Let(slip.Symbol("*standard-output*"), c.outputStream)

	return c
}

// Run starts the connection read loop.
func (c *Connection) Run() {
	defer func() {
		c.Close()
		c.server.removeConnection(c.id)
	}()

	for {
		msg, err := ReadWireMessage(c.conn, c.scope)
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

// Close closes the connection.
func (c *Connection) Close() {
	c.conn.Close()
}

// Send writes a message to the connection.
func (c *Connection) Send(msg slip.Object) error {
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	LogWire("->", msg)
	return WriteWireMessage(c.conn, msg)
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
	if c, ok := msg[4].(slip.Fixnum); ok {
		cont = c
	}

	// Set current package if specified
	if pkgName != "" && pkgName != "nil" {
		if pkg := slip.FindPackage(pkgName); pkg != nil {
			c.currentPkg = pkg
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

// WriteString sends a :write-string message to SLIME.
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

// NewPackage sends a :new-package message to SLIME.
func (c *Connection) NewPackage(name, prompt string) {
	msg := slip.List{
		slip.Symbol(":new-package"),
		slip.String(name),
		slip.String(prompt),
	}
	_ = c.Send(msg)
}

// UpdateHistory updates the REPL history variables.
func (c *Connection) UpdateHistory(value, form slip.Object) {
	// Shift values
	c.lastValues[2] = c.lastValues[1]
	c.lastValues[1] = c.lastValues[0]
	c.lastValues[0] = value

	c.lastForms[2] = c.lastForms[1]
	c.lastForms[1] = c.lastForms[0]
	c.lastForms[0] = form

	// Update scope bindings
	c.scope.Set(slip.Symbol("*"), value)
	c.scope.Set(slip.Symbol("**"), c.lastValues[1])
	c.scope.Set(slip.Symbol("***"), c.lastValues[2])
	c.scope.Set(slip.Symbol("+"), form)
	c.scope.Set(slip.Symbol("++"), c.lastForms[1])
	c.scope.Set(slip.Symbol("+++"), c.lastForms[2])
}
