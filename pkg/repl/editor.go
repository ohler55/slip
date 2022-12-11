// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"io"
	"os"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl/term"
)

type die string

type editor struct {
	lines     [][]rune
	v0        int // first terminal line of display
	uni       []byte
	msg       string
	mode      []bindFunc
	line      int
	pos       int
	foff      int // form offset (right after prompt)
	dirty     int // number of lines below form to delete on next key stroke unless a tab
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
	fs, ok := scope.Get(slip.Symbol(stdInput)).(*slip.FileStream)
	if !ok {
		panic("*repl-editor* can only be set to true when the *standard-input* is a file-stream")
	}
	ed.in = (*os.File)(fs)
	ed.fd = int(((*os.File)(fs)).Fd())
	ed.out = scope.Get(slip.Symbol(stdOutput)).(io.Writer)
	ed.origState = term.MakeRaw(ed.fd)
	ed.mode = topMode
	_, _ = ed.out.Write([]byte("Entering the SLIP REPL editor. Type ctrl-h for help and key bindings.\n"))
}

func (ed *editor) stop() {
	if ed.origState != nil {
		term.Restore(ed.fd, ed.origState)
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
		_, _ = ed.out.Write([]byte(string(line)))
	}
	ed.setCursor(ed.v0+ed.line, ed.foff)
}

func (ed *editor) displayRune(v, h int, r rune) {
	ed.setCursor(v, h)
	_, _ = ed.out.Write([]byte(string([]rune{r})))
}

func (ed *editor) read() (out []byte) {
	if len(ed.lines) == 0 {
		ed.v0, _ = ed.getCursor()
		ed.setCursor(ed.v0, 0)
		ed.clearLine()
		_, _ = ed.out.Write([]byte(prompt))
		_, ed.foff = ed.getCursor()
		ed.lines = [][]rune{{}}
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
		if 0 < ed.dirty && rbuf[0] != 0x09 {
			start := ed.v0 + len(ed.lines) - 1
			for ; 0 < ed.dirty; ed.dirty-- {
				ed.setCursor(start+ed.dirty, 0)
				ed.clearLine()
			}
			ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
		}
		for i := 0; i < cnt; i++ {
			b := rbuf[i]
			ed.mode[b](ed, b)
			if b == 0x0d {
				break top
			}
		}
	}
	_, _ = ed.out.Write([]byte{'\n'})
	for _, line := range ed.lines {
		out = append(out, string(line)...)
		out = append(out, '\n')
	}
	return
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

func (ed *editor) scroll(n int) {
	if 0 < n {
		_, _ = fmt.Fprintf(ed.out, "\x1b[%dS", n)
	} else if n < 0 {
		_, _ = fmt.Fprintf(ed.out, "\x1b[%dT", n)
	}
}
