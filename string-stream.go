// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"io"
	"unicode/utf8"
)

// StringStreamSymbol is the symbol with a value of "string-stream".
const StringStreamSymbol = Symbol("string-stream")

func init() {
	DefConstant(StringStreamSymbol, StringStreamSymbol, `A _string-stream_ stream backed by character vector.`)
}

// StringStream is an input and output stream with seeker support.
type StringStream struct {
	buf []byte
	pos int
}

// NewStringStream initializes a new string-stream with the provided content.
func NewStringStream(content []byte) *StringStream {
	return &StringStream{buf: content}
}

// String representation of the Object.
func (obj *StringStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *StringStream) Append(b []byte) []byte {
	return append(b, "#<STRING-STREAM>"...)
}

// Simplify the Object into string.
func (obj *StringStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *StringStream) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *StringStream) Hierarchy() []Symbol {
	return []Symbol{StringStreamSymbol, StreamSymbol, TrueSymbol}
}

// StreamType returns 'stringStream.
func (obj *StringStream) StreamType() Symbol {
	return StringStreamSymbol
}

// Eval returns self.
func (obj *StringStream) Eval(s *Scope, depth int) Object {
	return obj
}

// Write to the current position in the buf. This is part of the io.Writer interface.
func (obj *StringStream) Write(b []byte) (int, error) {
	if obj.pos < len(obj.buf) {
		copy(obj.buf[obj.pos:], b)
		if len(obj.buf)-obj.pos < len(b) {
			obj.buf = append(obj.buf, b[len(obj.buf)-obj.pos:]...)
		}
	} else {
		obj.buf = append(obj.buf, b...)
	}
	obj.pos = len(obj.buf)

	return len(b), nil
}

// WriteAt to the offset in the buf. The position in the buf is not
// changed. This is part of the io.WriterAt interface.
func (obj *StringStream) WriteAt(b []byte, off int64) (int, error) {
	if off < int64(len(obj.buf)) {
		copy(obj.buf[off:], b)
		if int64(len(obj.buf))-off < int64(len(b)) {
			obj.buf = append(obj.buf, b[len(obj.buf)-int(off):]...)
		}
	} else {
		obj.buf = append(obj.buf, b...)
	}
	return len(b), nil
}

// Read from the current position in the buf. This is part of the io.Reader interface.
func (obj *StringStream) Read(p []byte) (n int, err error) {
	if len(obj.buf) <= obj.pos {
		if 0 < len(p) {
			err = io.EOF
		}
		return
	}
	n = copy(p, obj.buf[obj.pos:])
	obj.pos += n

	return
}

// ReadAt from the off in the buf and does not advance pos. This is part of
// the io.ReaderAt interface.
func (obj *StringStream) ReadAt(p []byte, off int64) (n int, err error) {
	if off <= 0 {
		off = 0 // maybe non-standard
	}
	if int64(len(obj.buf)) <= off {
		if 0 < len(p) {
			err = io.EOF
		}
		return
	}
	n = copy(p, obj.buf[off:])

	return
}

// ReadRune resd the next run in buf from the current position. This is part of
// the io.RuneReader interface.
func (obj *StringStream) ReadRune() (r rune, size int, err error) {
	if len(obj.buf) <= obj.pos {
		err = io.EOF
		return
	}
	r, size = utf8.DecodeRune(obj.buf[obj.pos:])
	obj.pos += size

	return
}

// Seek moves the pos in buf. This is part of the io.Seeker interface.
func (obj *StringStream) Seek(offset int64, whence int) (n int64, err error) {
	switch whence {
	case 0:
		// leave as it, from start
	case 1: // from current
		offset += int64(obj.pos)
	case 2: // from end
		offset += int64(len(obj.buf))
	}
	if offset < 0 || int64(len(obj.buf)) < offset {
		err = io.ErrShortBuffer
	} else {
		obj.pos = int(offset)
	}
	n = int64(obj.pos)

	return
}

// IsOpen return true if the stream is open or false if not.
func (obj *StringStream) IsOpen() bool {
	return true
}

// LastByte returns the last byte written or zero if nothing has been written.
func (obj *StringStream) LastByte() (b byte) {
	if 0 < len(obj.buf) && 0 < obj.pos {
		b = obj.buf[obj.pos-1]
	}
	return
}

// Content returns the current buffer as a string.
func (obj *StringStream) Content() string {
	return string(obj.buf)
}
