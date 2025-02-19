// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

// SymbolStreamSymbol is the symbol with a value of "symbol-stream".
const SymbolStreamSymbol = slip.Symbol("symbol-stream")

func init() {
	slip.DefConstant(SymbolStreamSymbol, SymbolStreamSymbol,
		`A _symbol-stream_ uses the stream bound to a global symbol for reading and writing.`)
}

// SymbolStream is a stream that looks up the stream bound to a symbol and
// uses that for read, write, close, and open state.
type SymbolStream struct {
	symbol slip.Symbol
	closed bool
}

// NewSymbolStream creates a new SymbolStream.
func NewSymbolStream(sym slip.Symbol) *SymbolStream {
	return &SymbolStream{symbol: sym}
}

// String representation of the Object.
func (obj *SymbolStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *SymbolStream) Append(b []byte) []byte {
	b = append(b, "#<SYMBOL-STREAM :symbol "...)
	b = append(b, []byte(obj.symbol)...)

	return append(b, '>')
}

// Simplify the Object into a string.
func (obj *SymbolStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *SymbolStream) Equal(other slip.Object) (eq bool) {
	if ss, ok := other.(*SymbolStream); ok && ss.symbol == obj.symbol && ss.closed == obj.closed {
		eq = true
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *SymbolStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{SymbolStreamSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'symbol-stream.
func (obj *SymbolStream) StreamType() slip.Symbol {
	return SymbolStreamSymbol
}

// Eval returns self.
func (obj *SymbolStream) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Close the stream but not the input or output streams.
func (obj *SymbolStream) Close() error {
	obj.closed = true

	return nil
}

// IsOpen return true if the stream is open or false if not.
func (obj *SymbolStream) IsOpen() bool {
	return !obj.closed
}

// Read from the stream bound to the symbol.
func (obj *SymbolStream) Read(p []byte) (int, error) {
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
func (obj *SymbolStream) Write(b []byte) (n int, err error) {
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
