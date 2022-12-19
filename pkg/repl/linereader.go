// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"io"
)

// LineReader is an io.Reader that is optimized for reading lines terminated
// by a newline character.
type LineReader struct {
	r    io.Reader
	cnt  int
	buf  []byte
	line []byte
}

// NewLineReader creates a new LineReader.
func NewLineReader(r io.Reader, size uint) *LineReader {
	if size < 16 { // don't allow something too small
		size = 16
	}
	return &LineReader{r: r, buf: make([]byte, size)}
}

// Read into the provided buffer, p.
func (lr *LineReader) Read(p []byte) (n int, err error) {
	return lr.r.Read(p)
}

// ReadLine reads a line. The returned line is the internal buffer so should
// be copied if it needs to be preserved past the next call to the reader.
func (lr *LineReader) ReadLine() ([]byte, error) {
	lr.line = lr.line[:0]
	var err error
	for {
		for i := 0; i < lr.cnt; i++ {
			if lr.buf[i] == '\n' {
				lr.line = append(lr.line, lr.buf[:i]...)
				i++
				copy(lr.buf, lr.buf[i:])
				lr.cnt -= i
				return lr.line, nil
			}
		}
		lr.line = append(lr.line, lr.buf[:lr.cnt]...)
		if lr.cnt, err = lr.r.Read(lr.buf); err != nil {
			break
		}
	}
	return lr.line, err
}
