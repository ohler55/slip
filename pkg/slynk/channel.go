// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
)

// Channel represents a bidirectional communication channel for mrepl.
// Each channel has its own evaluation scope, package context, and history.
type Channel struct {
	id         int64
	conn       *Connection
	scope      *slip.Scope
	currentPkg *slip.Package
	name       string

	// History for #v backreferences
	history   []slip.Object
	historyMu sync.Mutex

	// REPL history variables
	lastValues [3]slip.Object // *, **, ***
	lastForms  [3]slip.Object // +, ++, +++

	closed atomic.Bool
}

// NewChannel creates a new channel for the connection.
func NewChannel(id int64, conn *Connection, name string) *Channel {
	// Create isolated scope for this channel
	scope := conn.server.baseScope.NewScope()

	ch := &Channel{
		id:         id,
		conn:       conn,
		scope:      scope,
		currentPkg: slip.FindPackage("cl-user"),
		name:       name,
		history:    make([]slip.Object, 0, 100),
	}

	// Initialize history variables
	scope.Let(slip.Symbol("*"), nil)
	scope.Let(slip.Symbol("**"), nil)
	scope.Let(slip.Symbol("***"), nil)
	scope.Let(slip.Symbol("+"), nil)
	scope.Let(slip.Symbol("++"), nil)
	scope.Let(slip.Symbol("+++"), nil)
	scope.Let(slip.Symbol("/"), nil)
	scope.Let(slip.Symbol("//"), nil)
	scope.Let(slip.Symbol("///"), nil)

	return ch
}

// ID returns the channel's unique identifier.
func (ch *Channel) ID() int64 {
	return ch.id
}

// Name returns the channel's name.
func (ch *Channel) Name() string {
	return ch.name
}

// Scope returns the channel's evaluation scope.
func (ch *Channel) Scope() *slip.Scope {
	return ch.scope
}

// Package returns the channel's current package.
func (ch *Channel) Package() *slip.Package {
	return ch.currentPkg
}

// SetPackage sets the channel's current package.
func (ch *Channel) SetPackage(pkg *slip.Package) {
	ch.currentPkg = pkg
}

// Close marks the channel as closed.
func (ch *Channel) Close() {
	ch.closed.Store(true)
}

// IsClosed returns true if the channel is closed.
func (ch *Channel) IsClosed() bool {
	return ch.closed.Load()
}

// AddToHistory adds a value to the channel's history for #v references.
func (ch *Channel) AddToHistory(value slip.Object) int {
	ch.historyMu.Lock()
	defer ch.historyMu.Unlock()

	ch.history = append(ch.history, value)
	return len(ch.history)
}

// GetFromHistory retrieves a value from history by 1-based index.
func (ch *Channel) GetFromHistory(index int) (slip.Object, error) {
	ch.historyMu.Lock()
	defer ch.historyMu.Unlock()

	if index < 1 || index > len(ch.history) {
		return nil, fmt.Errorf("history index %d out of range (1-%d)", index, len(ch.history))
	}
	return ch.history[index-1], nil
}

// HistoryLength returns the number of items in history.
func (ch *Channel) HistoryLength() int {
	ch.historyMu.Lock()
	defer ch.historyMu.Unlock()
	return len(ch.history)
}

// UpdateREPLHistory updates the standard REPL history variables.
func (ch *Channel) UpdateREPLHistory(value, form slip.Object) {
	// Shift values
	ch.lastValues[2] = ch.lastValues[1]
	ch.lastValues[1] = ch.lastValues[0]
	ch.lastValues[0] = value

	ch.lastForms[2] = ch.lastForms[1]
	ch.lastForms[1] = ch.lastForms[0]
	ch.lastForms[0] = form

	// Update scope bindings
	ch.scope.Set(slip.Symbol("*"), value)
	ch.scope.Set(slip.Symbol("**"), ch.lastValues[1])
	ch.scope.Set(slip.Symbol("***"), ch.lastValues[2])
	ch.scope.Set(slip.Symbol("+"), form)
	ch.scope.Set(slip.Symbol("++"), ch.lastForms[1])
	ch.scope.Set(slip.Symbol("+++"), ch.lastForms[2])

	// Also add to indexed history for #v
	ch.AddToHistory(value)
}

// Send sends a message on this channel.
func (ch *Channel) Send(msg slip.Object) error {
	return ch.conn.SendChannelMessage(ch.id, msg)
}

// String implements slip.Object.
func (ch *Channel) String() string {
	return fmt.Sprintf("#<slynk-channel %d %q>", ch.id, ch.name)
}

// Append implements slip.Object.
func (ch *Channel) Append(b []byte) []byte {
	return append(b, ch.String()...)
}

// Simplify implements slip.Object.
func (ch *Channel) Simplify() any {
	return ch.String()
}

// Equal implements slip.Object.
func (ch *Channel) Equal(other slip.Object) bool {
	if o, ok := other.(*Channel); ok {
		return ch.id == o.id && ch.conn == o.conn
	}
	return false
}

// Hierarchy implements slip.Object.
func (ch *Channel) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"slynk-channel", slip.TrueSymbol}
}

// Eval implements slip.Object.
func (ch *Channel) Eval(scope *slip.Scope, depth int) slip.Object {
	return ch
}
