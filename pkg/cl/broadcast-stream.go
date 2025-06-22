// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

// BroadcastStreamSymbol is the symbol with a value of "broadcast-stream".
const BroadcastStreamSymbol = slip.Symbol("broadcast-stream")

func init() {
	slip.CLPkg.Locked = false // a bit of a cheat
	slip.CLPkg.DefConst(string(BroadcastStreamSymbol), BroadcastStreamSymbol,
		`A _broadcast-stream_ stream that forward output to component streams.`)
	slip.CLPkg.Locked = true
	slip.CLPkg.Export(string(BroadcastStreamSymbol))
}

// BroadcastStream is slice of output streams or streams that are also
// io.Writers.
type BroadcastStream []slip.Stream

// NewBroadcastStream creates a new broadcast-stream.
func NewBroadcastStream(args ...slip.Object) BroadcastStream {
	bs := make(BroadcastStream, len(args)+1)
	bs[0] = nil
	for i, a := range args {
		if os, ok := a.(slip.Stream); ok {
			if _, ok = os.(io.Writer); ok {
				bs[i+1] = os
				continue
			}
		}
		slip.PanicType("output-stream", a, "output-stream")
	}
	return bs
}

// String representation of the Object.
func (obj BroadcastStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj BroadcastStream) Append(b []byte) []byte {
	return append(b, "#<BROADCAST-STREAM>"...)
}

// Simplify the Object into an combination of the current broadcast and a position.
func (obj BroadcastStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj BroadcastStream) Equal(other slip.Object) (eq bool) {
	if ob, ok := other.(BroadcastStream); ok && len(obj) == len(ob) {
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
func (obj BroadcastStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{BroadcastStreamSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'broadcastStream.
func (obj BroadcastStream) StreamType() slip.Symbol {
	return BroadcastStreamSymbol
}

// Eval returns self.
func (obj BroadcastStream) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Write to all the component streams. If any one of the writes fails a panic
// is called.
func (obj BroadcastStream) Write(b []byte) (n int, err error) {
	if obj[0] != nil {
		return 0, slip.NewStreamError(obj, "closed")
	}
	for _, s := range obj {
		if s != nil {
			if n, err = s.(io.Writer).Write(b); err != nil {
				break
			}
		}
	}
	return
}

// Seek does not move the position in any of the streams. It is used to
// determine the position of the last component stream only.
func (obj BroadcastStream) Seek(offset int64, whence int) (n int64, err error) {
	if obj[0] != nil {
		return 0, slip.NewStreamError(obj, "closed")
	}
	if 1 < len(obj) {
		if seeker, ok := obj[len(obj)-1].(io.Seeker); ok {
			n, err = seeker.Seek(offset, whence)
		}
	}
	return
}

// Close the stream but not the component streams.
func (obj BroadcastStream) Close() error {
	obj[0] = &slip.StringStream{}
	return nil
}

// IsOpen return true if the stream is open or false if not.
func (obj BroadcastStream) IsOpen() bool {
	return obj[0] == nil
}

// LastByte returns the last byte written or zero if nothing has been written.
func (obj BroadcastStream) LastByte() (b byte) {
	if obj[0] != nil {
		return 0
	}
	if 1 < len(obj) {
		if peeker, ok := obj[len(obj)-1].(slip.LastBytePeeker); ok {
			b = peeker.LastByte()
		}
	}
	return
}

// FileLength return the length of a file.
func (obj BroadcastStream) FileLength() (length slip.Object) {
	if 1 < len(obj) {
		if hfl, ok := obj[len(obj)-1].(hasFileLength); ok {
			length = hfl.FileLength()
		}
	}
	return
}
