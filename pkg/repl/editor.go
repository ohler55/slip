// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"unicode/utf8"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl/term"
)

type die string

type editor struct {
	lines     [][]rune
	v0        int // first terminal line of display
	key       []byte
	uni       []byte
	msg       string
	mode      []bindFunc
	kcnt      int
	line      int
	pos       int
	foff      int // form offset (right after prompt)
	dirty     dirty
	match     point // matching parens that needs to be redrawn on move
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
	ed.key = make([]byte, 8)
	ed.match.line = -1
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

func (ed *editor) displayRune(line, pos int) {
	if 0 <= line && line < len(ed.lines) && 0 <= pos && pos < len(ed.lines[line]) {
		ed.setCursor(ed.v0+line, ed.foff+pos)
		if ed.match.line == line && ed.match.pos == pos {
			_, _ = ed.out.Write([]byte(matchColor))
			_, _ = ed.out.Write([]byte(string(ed.lines[line][pos : pos+1])))
			if 0 < len(matchColor) {
				_, _ = ed.out.Write([]byte{'\x1b', '[', 'm'})
			}
		} else {
			_, _ = ed.out.Write([]byte(string(ed.lines[line][pos : pos+1])))
		}
	}
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
	var err error
top:
	for {
		if ed.kcnt, err = ed.in.Read(ed.key); err != nil {
			panic(err)
		} else if ed.kcnt == 0 {
			continue
		}
		// dirty and not tab and not shift-tab
		if 0 < ed.dirty.cnt &&
			ed.key[0] != 0x09 && !(ed.key[0] == 0x1b && ed.key[1] == 0x5b && ed.key[2] == 0x5a) {
			start := ed.v0 + len(ed.lines) - 1
			for ; 0 < ed.dirty.cnt; ed.dirty.cnt-- {
				ed.setCursor(start+ed.dirty.cnt, 0)
				ed.clearLine()
			}
			ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
			ed.dirty.lines = nil
		}
		if 0 <= ed.match.line {
			n := ed.match.line
			ed.match.line = -1
			ed.displayRune(n, ed.match.pos)
			ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
		}
		for i := 0; i < ed.kcnt; i++ {
			b := ed.key[i]
			ed.mode[b](ed, b)
			if b == 0x0d {
				break top
			}
		}
	}
	ed.setCursor(ed.v0+len(ed.lines), 0)
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

func (ed *editor) box(top, left, h, w, barTop, barBottom int) {
	// Save cursor position and then turn the cursor invisible.
	_, _ = ed.out.Write([]byte{'\x1b', '[', '7', '\x1b', '[', '?', '2', '5', 'l'})

	ed.setCursor(top, left)
	line := utf8.AppendRune(nil, '┌')
	line = append(line, bytes.Repeat(utf8.AppendRune(nil, '─'), w-2)...)
	line = utf8.AppendRune(line, '┒')
	_, _ = ed.out.Write(line)

	leftEdge := utf8.AppendRune(nil, '│')
	rightEdge := utf8.AppendRune(nil, '┃')
	barEdge := utf8.AppendRune(nil, '▊')
	// barEdge := utf8.AppendRune(nil, '█')

	barTop += top
	barBottom += top
	for i := top + 1; i < top+h; i++ {
		ed.setCursor(i, left)
		_, _ = ed.out.Write(leftEdge)
		ed.setCursor(i, left+w-1)
		if barTop <= i && i < barBottom {
			_, _ = ed.out.Write(barEdge)
		} else {
			_, _ = ed.out.Write(rightEdge)
		}
	}
	ed.setCursor(top+h, left)
	line = utf8.AppendRune(nil, '┕')
	line = append(line, bytes.Repeat(utf8.AppendRune(nil, '━'), w-2)...)
	line = utf8.AppendRune(line, '┛')
	_, _ = ed.out.Write(line)

	// Restore the cursor position then Make the cursor visible.
	_, _ = ed.out.Write([]byte{'\x1b', '[', '8', '\x1b', '[', '?', '2', '5', 'h'})
}

func (ed *editor) displayMessage(msg []byte) {
	w, h := term.GetSize(0)
	bottom := ed.v0 + len(ed.lines)
	cnt := 1
	ed.dirty.cnt = cnt + 1
	if h <= bottom+cnt {
		diff := bottom + cnt - h
		ed.scroll(diff)
		ed.v0 -= diff
	}

	ed.setCursor(ed.v0+len(ed.lines), 1)
	_, _ = ed.out.Write([]byte{'\x1b', '[', '7', 'm'})
	_, _ = ed.out.Write(bytes.Repeat([]byte{' '}, ed.foff-1))
	msg = append(msg, bytes.Repeat([]byte{' '}, w-len(msg)-ed.foff)...)
	msg = append(msg, "\x1b[m"...)
	_, _ = ed.out.Write(msg)
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
}

func (ed *editor) displayHelp(doc []byte) {
	w, h := term.GetSize(0)
	bottom := ed.v0 + len(ed.lines)
	box := scope.Get("*repl-help-box*") != nil
	indent := 0
	pad := 0
	if box {
		w -= 6
		indent = 3
		pad = 2
	}
	cnt := bytes.Count(doc, []byte{'\n'})
	if h <= bottom+cnt+pad {
		diff := bottom + cnt - h + pad
		if h <= len(ed.lines)+pad+cnt { // not all will fit in window
			ed.dirty.lines = bytes.Split(doc, []byte{'\n'})
			ed.dirty.top = 0
			ed.dirty.box = box
			cnt = h - len(ed.lines) - pad - 2
			diff = bottom + cnt - h + pad
			pos := 0
			for i := cnt; 0 < i; i-- {
				p := bytes.IndexByte(doc[pos+1:], '\n')
				pos += p + 1
			}
			doc = doc[:pos]
		}
		ed.scroll(diff)
		ed.v0 -= diff
	}
	ed.dirty.cnt = cnt + 1 + pad
	ed.setCursor(ed.v0+len(ed.lines)+pad/2, indent+1)
	_, _ = ed.out.Write(doc)
	if box {
		if ed.dirty.lines != nil {
			ed.box(ed.v0+len(ed.lines), 2, cnt+2/pad, w+pad, 0, cnt*cnt/len(ed.dirty.lines))
		} else {
			ed.box(ed.v0+len(ed.lines), 2, cnt+2/pad, w+pad, -1, -1)
		}
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

// dir can be -1, 0, or 1
func (ed *editor) updateDirty(dir int) {
	switch {
	case 0 < dir:
		if ed.dirty.top+ed.dirty.cnt-3 < len(ed.dirty.lines) {
			ed.dirty.top += ed.dirty.cnt - 5
			if len(ed.dirty.lines) <= ed.dirty.top-ed.dirty.cnt+3 {
				ed.dirty.top = len(ed.dirty.lines) - ed.dirty.cnt + 3
			}
		}
	case dir < 0:
		ed.dirty.top -= ed.dirty.cnt - 3
		if ed.dirty.top < 0 {
			ed.dirty.top = 0
		}
	}
	last := ed.dirty.top + ed.dirty.cnt - 3
	if len(ed.dirty.lines) < last {
		last = len(ed.dirty.lines)
	}
	doc := bytes.Join(ed.dirty.lines[ed.dirty.top:last], []byte{'\n'})
	for i := 0; i < ed.dirty.cnt-2; i++ {
		ed.setCursor(ed.v0+len(ed.lines)+i, 4)
		ed.clearToEnd()
	}
	ed.setCursor(ed.v0+len(ed.lines)+1, 4)
	_, _ = ed.out.Write(doc)
	if ed.dirty.box {
		w, _ := term.GetSize(0)
		cnt := ed.dirty.cnt - 3
		barTop := cnt * ed.dirty.top / len(ed.dirty.lines)
		bar := cnt * cnt / len(ed.dirty.lines)
		ed.box(ed.v0+len(ed.lines), 2, ed.dirty.cnt-2, w-4, barTop, barTop+bar)
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func (ed *editor) findOpenParen() *point {
	var depth int
	pos := ed.pos - 1
	for i := ed.line; 0 <= i; i-- {
		line := ed.lines[i]
		if pos < 0 {
			pos = len(line) - 1
		}
		for ; 0 <= pos; pos-- {
			r := line[pos]
			switch r {
			case '(':
				depth--
				if depth == 0 {
					return &point{line: i, pos: pos}
				}
			case ')':
				depth++
			}
		}
	}
	return nil
}

func (ed *editor) findCloseParen() *point {
	var depth int
	pos := ed.pos
	for i := ed.line; i < len(ed.lines); i++ {
		line := ed.lines[i]
		for ; pos < len(line); pos++ {
			r := line[pos]
			switch r {
			case '(':
				depth++
			case ')':
				depth--
				if depth == 0 {
					return &point{line: i, pos: pos + 1}
				}
			}
		}
		pos = 0
	}
	return nil
}
