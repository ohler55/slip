// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"sync"

	"github.com/ohler55/slip"
)

// SlynkOutputStream captures output and forwards it to SLY via :write-string.
// It implements io.Writer and slip.Object so it can be bound to *standard-output*.
type SlynkOutputStream struct {
	conn   *Connection
	buffer []byte
	mu     sync.Mutex
}

// NewSlynkOutputStream creates a new output stream for the connection.
func NewSlynkOutputStream(conn *Connection) *SlynkOutputStream {
	return &SlynkOutputStream{
		conn: conn,
	}
}

// Write implements io.Writer. It buffers the output.
func (s *SlynkOutputStream) Write(p []byte) (n int, err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.buffer = append(s.buffer, p...)
	return len(p), nil
}

// Flush sends buffered output to SLY and clears the buffer.
func (s *SlynkOutputStream) Flush() string {
	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.buffer) == 0 {
		return ""
	}

	result := string(s.buffer)
	s.buffer = s.buffer[:0]
	return result
}

// FlushToSly sends buffered output directly to SLY.
func (s *SlynkOutputStream) FlushToSly() {
	output := s.Flush()
	if output != "" {
		s.conn.WriteString(output)
	}
}

// Close implements io.Closer.
func (s *SlynkOutputStream) Close() error {
	s.FlushToSly()
	return nil
}

// String implements slip.Object.
func (s *SlynkOutputStream) String() string {
	return "#<slynk-output-stream>"
}

// Append implements slip.Object.
func (s *SlynkOutputStream) Append(b []byte) []byte {
	return append(b, s.String()...)
}

// Simplify implements slip.Object.
func (s *SlynkOutputStream) Simplify() any {
	return s.String()
}

// Equal implements slip.Object.
func (s *SlynkOutputStream) Equal(other slip.Object) bool {
	if o, ok := other.(*SlynkOutputStream); ok {
		return s == o
	}
	return false
}

// Hierarchy implements slip.Object.
func (s *SlynkOutputStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"slynk-output-stream", slip.StreamSymbol, slip.TrueSymbol}
}

// Eval implements slip.Object.
func (s *SlynkOutputStream) Eval(scope *slip.Scope, depth int) slip.Object {
	return s
}

// IsOpen returns true if the stream is open (connection is active).
func (s *SlynkOutputStream) IsOpen() bool {
	return s.conn != nil
}

// LastByte returns the last byte written, or 0 if the buffer is empty.
func (s *SlynkOutputStream) LastByte() byte {
	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.buffer) == 0 {
		return 0
	}
	return s.buffer[len(s.buffer)-1]
}
