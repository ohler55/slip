// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"sync"

	"github.com/ohler55/slip"
)

// SwankOutputStream captures output and forwards it to SLIME via :write-string.
// It implements io.Writer and slip.Object so it can be bound to *standard-output*.
type SwankOutputStream struct {
	conn   *Connection
	buffer []byte
	mu     sync.Mutex
}

// NewSwankOutputStream creates a new output stream for the connection.
func NewSwankOutputStream(conn *Connection) *SwankOutputStream {
	return &SwankOutputStream{
		conn: conn,
	}
}

// Write implements io.Writer. It buffers the output.
func (s *SwankOutputStream) Write(p []byte) (n int, err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.buffer = append(s.buffer, p...)
	return len(p), nil
}

// Flush sends buffered output to SLIME and clears the buffer.
func (s *SwankOutputStream) Flush() string {
	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.buffer) == 0 {
		return ""
	}

	result := string(s.buffer)
	s.buffer = s.buffer[:0]
	return result
}

// FlushToSlime sends buffered output directly to SLIME.
func (s *SwankOutputStream) FlushToSlime() {
	output := s.Flush()
	if output != "" {
		s.conn.WriteString(output)
	}
}

// Close implements io.Closer.
func (s *SwankOutputStream) Close() error {
	s.FlushToSlime()
	return nil
}

// String implements slip.Object.
func (s *SwankOutputStream) String() string {
	return "#<swank-output-stream>"
}

// Append implements slip.Object.
func (s *SwankOutputStream) Append(b []byte) []byte {
	return append(b, s.String()...)
}

// Simplify implements slip.Object.
func (s *SwankOutputStream) Simplify() any {
	return s.String()
}

// Equal implements slip.Object.
func (s *SwankOutputStream) Equal(other slip.Object) bool {
	if o, ok := other.(*SwankOutputStream); ok {
		return s == o
	}
	return false
}

// Hierarchy implements slip.Object.
func (s *SwankOutputStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"swank-output-stream", slip.StreamSymbol, slip.TrueSymbol}
}

// Eval implements slip.Object.
func (s *SwankOutputStream) Eval(scope *slip.Scope, depth int) slip.Object {
	return s
}

// IsOpen returns true if the stream is open (connection is active).
func (s *SwankOutputStream) IsOpen() bool {
	return s.conn != nil
}

// LastByte returns the last byte written, or 0 if the buffer is empty.
func (s *SwankOutputStream) LastByte() byte {
	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.buffer) == 0 {
		return 0
	}
	return s.buffer[len(s.buffer)-1]
}

// stdOutMu serializes mutation of slip.StandardOutput across concurrent
// swank evals. Required because CL primitives like princ, terpri, write,
// and printer.Print read slip.StandardOutput as a Go-level global rather
// than resolving the scope binding *standard-output*. Without this mutex,
// goroutines spawned by handleEmacsRex cross-route output and can
// permanently corrupt the global when defer-restores interleave.
var stdOutMu sync.Mutex

// withStandardOutput runs fn with slip.StandardOutput set to stream,
// serialized across goroutines.
func withStandardOutput(stream slip.Object, fn func()) {
	stdOutMu.Lock()
	defer stdOutMu.Unlock()
	prev := slip.StandardOutput
	slip.StandardOutput = stream
	defer func() { slip.StandardOutput = prev }()
	fn()
}
