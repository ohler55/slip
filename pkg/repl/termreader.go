// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"io"

	"github.com/ohler55/slip"
)

type termReader struct {
	line  []byte
	buf   []byte
	depth int
}

func (r *termReader) initialize() {
}

func (r *termReader) stop() {
}

func (r *termReader) setDepth(d int) {
	r.depth = d
}

func (r *termReader) reset() {
	r.buf = r.buf[:0]
	r.depth = 0
}

func (r *termReader) read() []byte {
	w := scope.Get(slip.Symbol(stdOutput)).(io.Writer)
	if 0 < len(r.buf) {
		_, _ = w.Write([]byte("  "))
	} else {
		_, _ = w.Write([]byte(prompt))
	}
	if len(r.line) == 0 {
		r.line = make([]byte, 1024)
	}
	for {
		n, err := scope.Get(slip.Symbol(stdInput)).(io.Reader).Read(r.line)
		if err != nil {
			panic(err)
		}
		r.buf = append(r.buf, r.line[:n]...)
		if 0 < len(r.buf) && r.buf[len(r.buf)-1] == '\n' {
			break
		}
	}
	return r.buf
}
