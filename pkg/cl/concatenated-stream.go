// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"errors"
	"io"

	"github.com/ohler55/slip"
)

// ConcatenatedStreamSymbol is the symbol with a value of "concatenated-stream".
const ConcatenatedStreamSymbol = slip.Symbol("concatenated-stream")

func init() {
	slip.DefConstant(ConcatenatedStreamSymbol, ConcatenatedStreamSymbol,
		`A _concatenated-stream_ stream reads from component streams.`)
}

// ConcatenatedStream is slice of output streams or streams that are also
// io.Readers
type ConcatenatedStream []slip.Stream

func NewConcatenatedStream(args ...slip.Object) ConcatenatedStream {
	cs := make(ConcatenatedStream, len(args)+1)
	cs[0] = nil
	for i, a := range args {
		if is, ok := a.(slip.Stream); ok {
			if _, ok = is.(io.Reader); ok {
				cs[i+1] = is
				continue
			}
		}
		slip.PanicType("input-stream", a, "input-stream")
	}
	return cs
}

// String representation of the Object.
func (obj ConcatenatedStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj ConcatenatedStream) Append(b []byte) []byte {
	return append(b, "#<CONCATENATED-STREAM>"...)
}

// Simplify the Object into an combination of the current concatenated and a position.
func (obj ConcatenatedStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj ConcatenatedStream) Equal(other slip.Object) (eq bool) {
	if ob, ok := other.(ConcatenatedStream); ok && len(obj) == len(ob) {
		eq = true
		for i, s := range obj {
			os := ob[i]
			if s != os || (s != nil && !s.Equal(os)) {
				eq = false
				break
			}
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj ConcatenatedStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{ConcatenatedStreamSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'concatenatedStream.
func (obj ConcatenatedStream) StreamType() slip.Symbol {
	return ConcatenatedStreamSymbol
}

// Eval returns self.
func (obj ConcatenatedStream) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Read each component streams until the last one returns EOF.
func (obj ConcatenatedStream) Read(b []byte) (n int, err error) {
	if obj[0] != nil {
		err = slip.NewStreamError(obj, "closed")
		return
	}
	var read bool
	for i, s := range obj {
		if s == nil {
			continue
		}
		read = true
		sn, se := s.(io.Reader).Read(b)
		if se != nil {
			obj[i] = nil
			if !errors.Is(se, io.EOF) {
				err = se
				return
			}
		}
		b = b[sn:]
		n += sn
		if len(b) == 0 {
			break
		}
	}
	if !read {
		err = io.EOF
	}
	return
}

// Close the stream but not the component streams.
func (obj ConcatenatedStream) Close() error {
	obj[0] = &slip.StringStream{}
	return nil
}

// IsOpen return true if the stream is open or false if not.
func (obj ConcatenatedStream) IsOpen() bool {
	return obj[0] == nil
}
