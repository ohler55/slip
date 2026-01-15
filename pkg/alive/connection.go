// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"net"
	"sync"

	"github.com/ohler55/slip"
)

// Connection represents a single Alive LSP connection.
type Connection struct {
	id         int64
	conn       net.Conn
	reader     *bufio.Reader
	server     *Server
	scope      *slip.Scope
	currentPkg *slip.Package
	writeMu    sync.Mutex

	// LSP state
	initialized bool
	shutdownReq bool

	// Output capture stream
	outputStream *AliveOutputStream

	// History tracking
	lastValues [3]slip.Object // *, **, ***
	lastForms  [3]slip.Object // +, ++, +++
}

// NewConnection creates a connection for an accepted socket.
func NewConnection(id int64, conn net.Conn, server *Server) *Connection {
	// Create a child scope for this connection
	scope := server.baseScope.NewScope()

	c := &Connection{
		id:         id,
		conn:       conn,
		reader:     bufio.NewReader(conn),
		server:     server,
		scope:      scope,
		currentPkg: slip.FindPackage("cl-user"),
	}

	// Create output stream
	c.outputStream = NewAliveOutputStream(c)

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
		msg, err := ReadMessage(c.reader)
		if err != nil {
			if !errors.Is(err, io.EOF) {
				fmt.Printf("alive: connection %d read error: %v\n", c.id, err)
			}
			return
		}

		if msg == nil {
			continue
		}

		c.dispatch(msg)

		// Exit after shutdown acknowledgment
		if c.shutdownReq && msg.Method == "exit" {
			return
		}
	}
}

// Close closes the connection.
func (c *Connection) Close() {
	c.conn.Close()
}

// Send writes a JSON-RPC message to the connection.
func (c *Connection) Send(msg *JSONRPCMessage) error {
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	return WriteMessage(c.conn, msg)
}

// SendResponse sends a response for a request ID.
func (c *Connection) SendResponse(id interface{}, result interface{}) error {
	return c.Send(NewResponse(id, result))
}

// SendError sends an error response for a request ID.
func (c *Connection) SendError(id interface{}, code int, message string) error {
	return c.Send(NewErrorResponse(id, code, message, nil))
}

// SendNotification sends a notification (no response expected).
func (c *Connection) SendNotification(method string, params interface{}) error {
	return c.Send(NewNotification(method, params))
}

// dispatch routes a JSON-RPC message to the appropriate handler.
func (c *Connection) dispatch(msg *JSONRPCMessage) {
	// Handle notifications (no ID)
	if msg.ID == nil {
		c.handleNotification(msg)
		return
	}

	// Handle requests
	handler := GetHandler(msg.Method)
	if handler == nil {
		c.SendError(msg.ID, MethodNotFound, fmt.Sprintf("method not found: %s", msg.Method))
		return
	}

	// Check initialization state
	if !c.initialized && msg.Method != "initialize" {
		c.SendError(msg.ID, ServerNotInitialized, "server not initialized")
		return
	}

	// Execute handler with panic recovery
	result, err := func() (res interface{}, err error) {
		defer func() {
			if r := recover(); r != nil {
				if p, ok := r.(*slip.Panic); ok {
					err = fmt.Errorf("%s", p.Message)
				} else {
					err = fmt.Errorf("%v", r)
				}
			}
		}()
		return handler(c, msg.Params)
	}()

	if err != nil {
		c.SendError(msg.ID, InternalError, err.Error())
		return
	}

	c.SendResponse(msg.ID, result)
}

// handleNotification handles notifications (messages without ID).
func (c *Connection) handleNotification(msg *JSONRPCMessage) {
	switch msg.Method {
	case "initialized":
		// Client acknowledges initialization complete
		c.initialized = true
	case "exit":
		// Client requests exit
		c.shutdownReq = true
	case "textDocument/didOpen", "textDocument/didClose", "textDocument/didChange", "textDocument/didSave":
		// Document notifications - dispatch to handler if registered
		if handler := GetHandler(msg.Method); handler != nil {
			handler(c, msg.Params)
		}
	default:
		// Ignore unknown notifications per LSP spec
	}
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

// Scope returns the connection's evaluation scope.
func (c *Connection) Scope() *slip.Scope {
	return c.scope
}

// Package returns the current package.
func (c *Connection) Package() *slip.Package {
	return c.currentPkg
}

// SetPackage sets the current package.
func (c *Connection) SetPackage(pkg *slip.Package) {
	c.currentPkg = pkg
}
