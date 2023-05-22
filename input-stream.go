// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"io"
	"unicode/utf8"
)

// InputStreamSymbol is the symbol with a value of "input-stream".
const InputStreamSymbol = Symbol("input-stream")

func init() {
	DefConstant(InputStreamSymbol, InputStreamSymbol, `A _input-stream_ stream backed by a io.Readr.`)
}

// InputStream is a *os.Input.
type InputStream struct {
	Reader   io.Reader
	lastRune rune
	useLast  bool
}

// String representation of the Object.
func (obj *InputStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *InputStream) Append(b []byte) []byte {
	return append(b, "#<INPUT-STREAM>"...)
}

// Simplify the Object into an int64.
func (obj *InputStream) Simplify() interface{} {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *InputStream) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *InputStream) Hierarchy() []Symbol {
	return []Symbol{InputStreamSymbol, StreamSymbol, TrueSymbol}
}

// StreamType returns 'inputStream.
func (obj *InputStream) StreamType() Symbol {
	return InputStreamSymbol
}

// Eval returns self.
func (obj *InputStream) Eval(s *Scope, depth int) Object {
	return obj
}

// Read made visible since os.Input functions are not automatically visible.
func (obj *InputStream) Read(b []byte) (int, error) {
	return obj.Reader.Read(b)
}

// Close the reader if it is a io.Closer
func (obj *InputStream) Close() (err error) {
	if closer, ok := obj.Reader.(io.Closer); ok {
		err = closer.Close()
	}
	return
}

// ReadRune reads a rune.
func (obj *InputStream) ReadRune() (r rune, size int, err error) {
	if rr, ok := obj.Reader.(io.RuneReader); ok {
		r, size, err = rr.ReadRune()
		obj.lastRune = r
	} else if obj.useLast {
		r = obj.lastRune
		size = utf8.RuneLen(r)
	} else {
		// TBD read 1 byte, check hi bit, read more as needed
	}
	return
}

// UnreadRune reads a rune.
func (obj *InputStream) UnreadRune() (err error) {
	if ur, ok := obj.Reader.(io.RuneScanner); ok {
		err = ur.UnreadRune()
	} else if obj.useLast || obj.lastRune == 0 {
		// TBD panic
	} else {
		obj.useLast = true
	}
	return
}

// TBD ReadByte() (byte, error)
//  if useLast and 1 byte rune then ok, else panic
//
