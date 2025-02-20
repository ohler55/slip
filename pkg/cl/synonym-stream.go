// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

// SynonymStreamSymbol is the symbol with a value of "synonym-stream".
const SynonymStreamSymbol = slip.Symbol("synonym-stream")

func init() {
	slip.DefConstant(SynonymStreamSymbol, SynonymStreamSymbol,
		`A _synonym-stream_ uses the stream bound to a global symbol for reading and writing.`)
}

// SynonymStream is a stream that looks up the stream bound to a symbol and
// uses that for read, write, close, and open state.
type SynonymStream struct {
	symbol slip.Symbol
	closed bool
}

// NewSynonymStream creates a new SynonymStream.
func NewSynonymStream(sym slip.Symbol) *SynonymStream {
	return &SynonymStream{symbol: sym}
}

// String representation of the Object.
func (obj *SynonymStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *SynonymStream) Append(b []byte) []byte {
	b = append(b, "#<SYNONYM-STREAM :symbol "...)
	b = append(b, []byte(obj.symbol)...)

	return append(b, '>')
}

// Simplify the Object into a string.
func (obj *SynonymStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *SynonymStream) Equal(other slip.Object) (eq bool) {
	if ss, ok := other.(*SynonymStream); ok && ss.symbol == obj.symbol && ss.closed == obj.closed {
		eq = true
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *SynonymStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{SynonymStreamSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'synonym-stream.
func (obj *SynonymStream) StreamType() slip.Symbol {
	return SynonymStreamSymbol
}

// Eval returns self.
func (obj *SynonymStream) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Close the stream but not the input or output streams.
func (obj *SynonymStream) Close() error {
	obj.closed = true

	return nil
}

// IsOpen return true if the stream is open or false if not.
func (obj *SynonymStream) IsOpen() bool {
	return !obj.closed
}

// Read from the stream bound to the symbol.
func (obj *SynonymStream) Read(p []byte) (int, error) {
	if obj.closed {
		slip.PanicStream(obj, "closed")
	}
	value, has := slip.CurrentPackage.Get(string(obj.symbol))
	if !has {
		slip.PanicUnboundVariable(obj.symbol, "unbound")
	}
	var r io.Reader
	switch tv := value.(type) {
	case io.Reader:
		r = tv
	case nil:
		r = slip.CurrentPackage.JustGet("*standard-input*").(io.Reader)
	default:
		if slip.True != tv {
			slip.PanicType(string(obj.symbol), tv, "input-stream")
		}
		r = slip.CurrentPackage.JustGet("*standard-input*").(io.Reader)
	}
	return r.Read(p)
}

// Write to the stream bound to the symbol.
func (obj *SynonymStream) Write(b []byte) (n int, err error) {
	if obj.closed {
		slip.PanicStream(obj, "closed")
	}
	value, has := slip.CurrentPackage.Get(string(obj.symbol))
	if !has {
		slip.PanicUnboundVariable(obj.symbol, "unbound")
	}
	var w io.Writer
	switch tv := value.(type) {
	case io.Writer:
		w = tv
	case nil:
		w = slip.CurrentPackage.JustGet("*standard-output*").(io.Writer)
	default:
		if slip.True != tv {
			slip.PanicType(string(obj.symbol), tv, "output-stream")
		}
		w = slip.CurrentPackage.JustGet("*standard-output*").(io.Writer)
	}
	return w.Write(b)
}
