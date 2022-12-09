// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"fmt"
	"io"
	"os"

	"github.com/ohler55/slip"
	"golang.org/x/term"
)

type die string

type editor struct {
	lines     [][]byte
	v0        int // first terminal line of display
	buf       []byte
	uni       []byte
	mode      []bindFunc
	line      int
	pos       int
	foff      int // form offset (right after prompt)
	in        *os.File
	fd        int // in fd
	out       io.Writer
	depth     int
	origState *term.State
}

func (ed *editor) initialize() {
	if ed.origState != nil {
		return
	}
	var err error
	fs, ok := scope.Get(slip.Symbol(stdInput)).(*slip.FileStream)
	if !ok {
		panic("*repl-editor* can only be set to true when the *standard-input* is a file-stream")
	}
	ed.in = (*os.File)(fs)
	ed.fd = int(((*os.File)(fs)).Fd())
	ed.out = scope.Get(slip.Symbol(stdOutput)).(io.Writer)
	if ed.origState, err = term.MakeRaw(ed.fd); err != nil {
		panic(err)
	}
	ed.buf = make([]byte, 16)
	ed.mode = topMode
}

func (ed *editor) stop() {
	if ed.origState != nil {
		_ = term.Restore(ed.fd, ed.origState)
		ed.origState = nil
	}
}

func (ed *editor) setDepth(d int) {
	ed.depth = d
}

func (ed *editor) reset() {
	for i := range ed.lines {
		ed.lines[i] = nil
	}
	ed.lines = ed.lines[:0]
	ed.line = 0
	ed.pos = 0
}

// TBD functions to get cursor position and also compare to screen size from term.GetSize()
// setCursor(), getCursor(), (save and restore also possible)

// TBD on read check .buf len. if 0 then get cursor, else move to start location
//  print prompt then each line followed by a clear to end of line
//    record location after printing prompt as poff (prompt offset or maybe foff for form offset)

func (ed *editor) display() {
	ed.setCursor(ed.v0, 0)
	ed.clearLine()
	_, _ = ed.out.Write([]byte(prompt))
	_, ed.foff = ed.getCursor()
	for i, line := range ed.lines {
		ed.setCursor(ed.v0+i, ed.foff)
		if 0 < i {
			ed.clearLine()
		}
		_, _ = ed.out.Write(line)
	}
	ed.setCursor(ed.v0+ed.line, ed.foff)
}

func (ed *editor) displayChar(line, pos int) {
	// TBD
}

func (ed *editor) read() []byte {
	if len(ed.lines) == 0 {
		ed.v0, _ = ed.getCursor()
		ed.setCursor(ed.v0, 0)
		ed.clearLine()
		_, _ = ed.out.Write([]byte(prompt))
		_, ed.foff = ed.getCursor()
	} else {
		ed.setCursor(ed.v0+ed.line, ed.foff)
	}
	rbuf := make([]byte, 8) // large enough for a character
top:
	for {
		cnt, err := ed.in.Read(rbuf)
		if err != nil {
			panic(err)
		} else if cnt == 0 {
			continue
		}
		for i := 0; i < cnt; i++ {
			b := rbuf[i]
			ed.mode[b](ed, b)
			if b == 0x0d {
				break top
			}
		}
	}
	_, _ = ed.out.Write([]byte{'\n', '\r'})
	return bytes.Join(ed.lines, []byte{'\n'})
}

// ANSI sequences
func (ed *editor) getCursor() (v, h int) {
	if _, err := ed.out.Write([]byte("\x1b[6n")); err != nil {
		panic(die(err.Error()))
	}
	if _, err := fmt.Fscanf(ed.in, "\x1b[%d;%dR", &v, &h); err != nil {
		panic(die(err.Error()))
	}
	return
}

func (ed *editor) setCursor(v, h int) {
	_, _ = fmt.Fprintf(ed.out, "\x1b[%d;%dH", v, h)
}

func (ed *editor) clearScreen() {
	_, _ = ed.out.Write([]byte(("\x1b[2J")))
}

func (ed *editor) clearLine() {
	_, _ = ed.out.Write([]byte(("\x1b[2K")))
}

func (ed *editor) clearToEnd() {
	_, _ = ed.out.Write([]byte(("\x1b[0K")))
}

func (ed *editor) clearToStart() {
	_, _ = ed.out.Write([]byte(("\x1b[1K")))
}

func (ed *editor) home() {
	_, _ = ed.out.Write([]byte(("\x1b[H")))
}

func (ed *editor) iUp(n int) {
	_, _ = fmt.Fprintf(ed.out, "\x1b[%dA", n)
}

func (ed *editor) iDown(n int) {
	_, _ = fmt.Fprintf(ed.out, "\x1b[%dB", n)
}

func (ed *editor) iRight(n int) {
	_, _ = fmt.Fprintf(ed.out, "\x1b[%dC", n)
}

func (ed *editor) iLeft(n int) {
	_, _ = fmt.Fprintf(ed.out, "\x1b[%dD", n)
}
