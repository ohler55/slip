// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	// Channel management
	RegisterHandler("slynk:mrepl-new", handleMREPLNew)
	RegisterHandler("slynk:mrepl-close", handleMREPLClose)

	// History access
	RegisterHandler("slynk:mrepl-get-history-value", handleGetHistoryValue)

	// Channel listing
	RegisterHandler("slynk:list-channels", handleListChannels)
}

// handleMREPLNew creates a new MREPL channel.
// Returns: channel-id
func handleMREPLNew(c *Connection, args slip.List) slip.Object {
	ch := c.CreateChannel("mrepl")

	// Send initial prompt on the new channel
	_ = ch.Send(slip.List{
		slip.Symbol(":prompt"),
		slip.String(ch.Package().Name),
		slip.String(ch.Package().Name),
		slip.Fixnum(0),
		slip.Fixnum(0),
	})

	return slip.Fixnum(ch.ID())
}

// handleMREPLClose closes an MREPL channel.
func handleMREPLClose(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return nil
	}

	chanID, ok := args[0].(slip.Fixnum)
	if !ok {
		return nil
	}

	c.CloseChannel(int64(chanID))
	return slip.TrueSymbol
}

// handleGetHistoryValue retrieves a value from channel history by index.
// This enables the #v syntax in SLY (e.g., #v1, #v2).
func handleGetHistoryValue(c *Connection, args slip.List) slip.Object {
	if len(args) < 2 {
		return nil
	}

	chanID, ok := args[0].(slip.Fixnum)
	if !ok {
		return nil
	}

	index, ok := args[1].(slip.Fixnum)
	if !ok {
		return nil
	}

	ch := c.GetChannel(int64(chanID))
	if ch == nil {
		return nil
	}

	value, err := ch.GetFromHistory(int(index))
	if err != nil {
		return nil
	}

	return value
}

// handleListChannels returns information about all active channels.
func handleListChannels(c *Connection, args slip.List) slip.Object {
	var channels slip.List

	c.channelsMu.RLock()
	defer c.channelsMu.RUnlock()

	for id, ch := range c.channels {
		channels = append(channels, slip.List{
			slip.Symbol(":id"), slip.Fixnum(id),
			slip.Symbol(":name"), slip.String(ch.Name()),
			slip.Symbol(":package"), slip.String(ch.Package().Name),
			slip.Symbol(":history-count"), slip.Fixnum(ch.HistoryLength()),
		})
	}

	return channels
}

// ChannelOutput represents a writer that sends output to a specific channel.
type ChannelOutput struct {
	ch *Channel
}

// NewChannelOutput creates an output writer for a channel.
func NewChannelOutput(ch *Channel) *ChannelOutput {
	return &ChannelOutput{ch: ch}
}

// Write implements io.Writer, sending output to the channel.
func (co *ChannelOutput) Write(p []byte) (n int, err error) {
	if co.ch.IsClosed() {
		return 0, fmt.Errorf("channel closed")
	}

	_ = co.ch.Send(slip.List{
		slip.Symbol(":write-string"),
		slip.String(string(p)),
	})

	return len(p), nil
}

// String implements slip.Object.
func (co *ChannelOutput) String() string {
	return fmt.Sprintf("#<channel-output %d>", co.ch.ID())
}

// Append implements slip.Object.
func (co *ChannelOutput) Append(b []byte) []byte {
	return append(b, co.String()...)
}

// Simplify implements slip.Object.
func (co *ChannelOutput) Simplify() any {
	return co.String()
}

// Equal implements slip.Object.
func (co *ChannelOutput) Equal(other slip.Object) bool {
	if o, ok := other.(*ChannelOutput); ok {
		return co.ch == o.ch
	}
	return false
}

// Hierarchy implements slip.Object.
func (co *ChannelOutput) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"channel-output", slip.StreamSymbol, slip.TrueSymbol}
}

// Eval implements slip.Object.
func (co *ChannelOutput) Eval(scope *slip.Scope, depth int) slip.Object {
	return co
}
