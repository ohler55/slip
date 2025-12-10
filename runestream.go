// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"io"
	"unicode/utf8"
)

// RuneStream defines functions to support reading streams of runes.
type RuneStream interface {
	io.Reader
	io.Closer
	io.RuneScanner
	io.ByteReader

	// IsOpen return true if the stream is open or false if not. Note that EOF
	// also indicates closed.
	IsOpen() bool
}

// RuneFromReader read a rune from an io.Reader.
func RuneFromReader(reader io.Reader) (r rune, size int, err error) {
	buf := make([]byte, 4)
	var cnt int
	if cnt, err = reader.Read(buf[:1]); cnt == 1 && err == nil {
		switch {
		case buf[0] < 0x80:
			r = rune(buf[0])
			size = 1
		case (buf[0] & 0xf8) == 0xf0: // 11110xxx
			// 4 byte rune
			if cnt, err = reader.Read(buf[1:]); cnt == 3 && err == nil {
				r, size = utf8.DecodeRune(buf)
			}
		case (buf[0] & 0xf0) == 0xe0: // 1110xxxx
			// 3 byte rune
			if cnt, err = reader.Read(buf[1:3]); cnt == 2 && err == nil {
				r, size = utf8.DecodeRune(buf)
			}
		case (buf[0] & 0xe0) == 0xc0: // 110xxxxx
			// 2 byte rune
			if cnt, err = reader.Read(buf[1:2]); cnt == 1 && err == nil {
				r, size = utf8.DecodeRune(buf)
			}
		}
	}
	if size == 0 && err == nil {
		err = io.EOF
	}
	return
}

// ByteFromReader reads a byte.
func ByteFromReader(r io.Reader) (b byte, err error) {
	buf := []byte{0}
	var cnt int
	if cnt, err = r.Read(buf); err != nil || cnt == 0 {
		return
	}
	b = buf[0]

	return
}
