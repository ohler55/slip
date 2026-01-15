// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"sync"

	"github.com/ohler55/slip"
)

// AliveOutputStream captures output and can send it to the LSP client.
type AliveOutputStream struct {
	conn   *Connection
	buffer []byte
	mu     sync.Mutex
}

// NewAliveOutputStream creates a new output stream for the connection.
func NewAliveOutputStream(conn *Connection) *AliveOutputStream {
	return &AliveOutputStream{
		conn: conn,
	}
}

// Write implements io.Writer.
func (s *AliveOutputStream) Write(p []byte) (n int, err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.buffer = append(s.buffer, p...)
	return len(p), nil
}

// Flush returns the buffered content and clears the buffer.
func (s *AliveOutputStream) Flush() string {
	s.mu.Lock()
	defer s.mu.Unlock()
	result := string(s.buffer)
	s.buffer = nil
	return result
}

// FlushToClient sends buffered content to the LSP client as a notification.
func (s *AliveOutputStream) FlushToClient() {
	content := s.Flush()
	if content != "" {
		// Send as window/logMessage notification
		s.conn.SendNotification("window/logMessage", map[string]interface{}{
			"type":    3, // Info
			"message": content,
		})
	}
}

// Close implements io.Closer.
func (s *AliveOutputStream) Close() error {
	s.FlushToClient()
	return nil
}

// slip.Object interface implementation

func (s *AliveOutputStream) String() string {
	return "#<alive-output-stream>"
}

func (s *AliveOutputStream) Append(b []byte) []byte {
	return append(b, s.String()...)
}

func (s *AliveOutputStream) Simplify() any {
	return s.String()
}

func (s *AliveOutputStream) Equal(other slip.Object) bool {
	if o, ok := other.(*AliveOutputStream); ok {
		return s == o
	}
	return false
}

func (s *AliveOutputStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"alive-output-stream", slip.StreamSymbol, slip.TrueSymbol}
}

func (s *AliveOutputStream) Eval(scope *slip.Scope, depth int) slip.Object {
	return s
}

// IsOpen returns true if the stream is open.
func (s *AliveOutputStream) IsOpen() bool {
	return s.conn != nil
}

// LastByte returns the last byte written, or 0 if empty.
func (s *AliveOutputStream) LastByte() byte {
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.buffer) > 0 {
		return s.buffer[len(s.buffer)-1]
	}
	return 0
}
