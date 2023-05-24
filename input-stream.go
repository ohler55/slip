// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
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
	if obj.useLast {
		r = obj.lastRune
		size = utf8.RuneLen(r)
		obj.useLast = false
	} else if rr, ok := obj.Reader.(io.RuneReader); ok {
		r, size, err = rr.ReadRune()
		obj.lastRune = r
	} else {
		buf := make([]byte, 4)
		var cnt int
		if cnt, err = obj.Read(buf[:1]); cnt == 1 && err == nil {
			switch {
			case buf[0] < 0x80:
				r = rune(buf[0])
				size = 1
			case (buf[0] & 0xf8) == 0xf0: // 11110xxx
				// 4 byte rune
				if cnt, err = obj.Read(buf[1:]); cnt == 3 && err == nil {
					r, size = utf8.DecodeRune(buf)
				}
			case (buf[0] & 0xf0) == 0xe0: // 1110xxxx
				// 3 byte rune
				if cnt, err = obj.Read(buf[1:3]); cnt == 2 && err == nil {
					r, size = utf8.DecodeRune(buf)
				}
			case (buf[0] & 0xe0) == 0xc0: // 110xxxxx
				// 2 byte rune
				if cnt, err = obj.Read(buf[1:2]); cnt == 1 && err == nil {
					r, size = utf8.DecodeRune(buf)
				}
			}
		}
		if size == 0 && err == nil {
			err = fmt.Errorf("invalid UTF8 character")
		}
		obj.lastRune = r
	}
	return
}

// UnreadRune reads a rune.
func (obj *InputStream) UnreadRune() (err error) {
	if ur, ok := obj.Reader.(io.RuneScanner); ok {
		err = ur.UnreadRune()
		obj.useLast = false
	} else if obj.useLast || obj.lastRune == 0 {
		err = fmt.Errorf("cannot unread a character more than once before a read")
	} else {
		obj.useLast = true
	}
	return
}

// ReadByte reads a byte.
func (obj *InputStream) ReadByte() (b byte, err error) {
	if obj.useLast {
		if 0x80 <= obj.lastRune {
			fmt.Errorf("cannot read a byte from a multiple byte character that was unread, %s",
				string([]rune{obj.lastRune}))
		}
		b = byte(obj.lastRune)
		obj.useLast = false
	} else if br, ok := obj.Reader.(io.ByteReader); ok {
		b, err = br.ReadByte()
		obj.lastRune = rune(b)
	} else {
		buf := []byte{0}
		var cnt int
		if cnt, err = obj.Read(buf); cnt == 1 && err == nil {
			b = buf[0]
		}
	}
	return
}
