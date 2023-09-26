// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"errors"
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
func (obj *InputStream) Simplify() any {
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
func (obj *InputStream) Read(b []byte) (cnt int, err error) {
	var plus int
	if obj.useLast && obj.lastRune != 0 {
		plus = utf8.RuneLen(obj.lastRune)
		if len(b) < plus {
			return 0, fmt.Errorf("read buffer to short for unread character %s", string([]rune{obj.lastRune}))
		}
		_ = utf8.EncodeRune(b, obj.lastRune)
		obj.useLast = false
	}
	if cnt, err = obj.Reader.Read(b[plus:]); err == nil {
		// Save the last rune written by searching backwards until the start
		// of a rune.
	top:
		for i := cnt - 1; 0 < i; i-- {
			bi := b[i]
			switch {
			case bi < 0x80:
				obj.lastRune = rune(bi)
				break top
			case bi&0xc0 == 0x80:
				// not the first byte of a rune so keep going
			case bi&0xc0 == 0xc0:
				obj.lastRune, _ = utf8.DecodeRune(b[i:cnt])
				break top
			}
		}
	} else if errors.Is(err, io.EOF) && 0 < plus {
		err = nil
	}
	cnt += plus
	return
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

// UnreadRune unreads a rune.
func (obj *InputStream) UnreadRune() (err error) {
	// The go RuneScanners require a previous ReadRune before an
	// UnreadRune. Since we would like to allow UnreadRune for regular reads
	// as well the direct approach is used instead.
	if obj.useLast || obj.lastRune == 0 {
		err = fmt.Errorf("cannot unread a character more than once before a read")
	} else {
		obj.useLast = true
	}
	return
}

// PushRune makes the rune argument the next rune to be read.
func (obj *InputStream) PushRune(r rune) {
	// The go RuneScanners require a previous ReadRune before an
	// UnreadRune. Since we would like to allow UnreadRune for regular reads
	// as well the direct approach is used instead.
	if obj.useLast || obj.lastRune == 0 {
		PanicStream(obj, "cannot unread a character more than once before a read")
	}
	obj.useLast = true
	obj.lastRune = r
}

// ReadByte reads a byte.
func (obj *InputStream) ReadByte() (b byte, err error) {
	if obj.useLast {
		if 0x80 <= obj.lastRune {
			return 0, fmt.Errorf("cannot read a byte from a multiple byte character that was unread, %s",
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
			obj.lastRune = rune(b)
		}
	}
	return
}
