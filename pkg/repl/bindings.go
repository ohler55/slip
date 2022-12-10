// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"fmt"
	"io"
	"unicode/utf8"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"golang.org/x/term"
)

type bindFunc func(ed *editor, b byte)

var (
	//   0123456789abcdef0123456789abcdef
	sepMap = "" +
		".........xx..x.................." + // 0x00
		"x.......xx......................" + // 0x20
		"................................." + // 0x40
		"................................" + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................" //  0xe0

	topMode  []bindFunc
	rootMode = []bindFunc{
		bad, lineBegin, back, done, delForward, lineEnd, forward, bad, // 0x00
		help, tab, bad, delLineEnd, bad, addReturn, down, bad, // 0x08
		up, bad, searchBack, searchForward, swapChar, bad, historyForward, nlAfter, // 0x10
		bad, bad, bad, esc, bad, bad, bad, bad, // 0x18
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x20
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x28
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x30
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x38
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x40
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x48
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x50
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x58
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x60
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x68
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, addByte, // 0x70
		addByte, addByte, addByte, addByte, addByte, addByte, addByte, delBack, // 0x78
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0x80
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0x88
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0x90
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0x98
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xa0
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xa8
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xb0
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xb8
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xc0
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xc8
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xd0
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xd8
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xe0
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xe8
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xf0
		topUni, topUni, topUni, topUni, topUni, topUni, topUni, topUni, // 0xf8
	}
	escMode = []bindFunc{
		bad, bad, matchClose, bad, bad, bad, matchOpen, bad, // 0x00
		bad, bad, bad, bad, bad, nl, bad, bad, // 0x08
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x10
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, describe, // 0x20
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x30
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x40
		bad, bad, bad, bad, bad, enterU8, bad, bad, // 0x50
		bad, bad, bad, esc5b, collapse, bad, bad, bad, // 0x58
		bad, bad, backWord, bad, delForwardWord, bad, forwardWord, bad, // 0x60
		bad, bad, bad, bad, bad, bad, bad, bad, // 0x68
		bad, bad, bad, bad, swapWord, enterU4, historyBack, bad, // 0x70
		bad, bad, bad, bad, bad, bad, bad, delBackWord, // 0x78
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x80
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x90
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xa0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xb0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xc0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xd0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xe0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xf0
	}
	esc5bMode = []bindFunc{
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x00
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x10
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x20
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x30
		bad, up, down, forward, back, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x40
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x50
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x60
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x70
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x80
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x90
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xa0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xb0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xc0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xd0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xe0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xf0
	}
	unicodeMode = []bindFunc{
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x00
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x10
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x20
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x30
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x40
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x50
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x60
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x70
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0x80
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0x88
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0x90
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0x98
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xa0
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xa8
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xb0
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xb8
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xc0
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xc8
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xd0
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xd8
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xe0
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xe8
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xf0
		addUni, addUni, addUni, addUni, addUni, addUni, addUni, addUni, // 0xf8
	}
)

func init() {
	topMode = rootMode
}

func bad(ed *editor, b byte) {
	fmt.Printf("*** %02x\n", b)
	// TBD if a status line then indicate and error there
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func done(ed *editor, b byte) {
	panic(io.EOF)
}

func topUni(ed *editor, b byte) {
	ed.uni = ed.uni[:0]
	ed.uni = append(ed.uni, b)
	ed.mode = unicodeMode
}

func addUni(ed *editor, b byte) {
	ed.uni = append(ed.uni, b)
	if utf8.Valid(ed.uni) {
		ed.mode = topMode
		if _, err := ed.out.Write(ed.uni); err != nil {
			panic(err)
		}
		r, _ := utf8.DecodeRune(ed.uni)
		ed.lines[ed.line] = append(ed.lines[ed.line], r)
		ed.pos++
	}
}

func addReturn(ed *editor, _ byte) {
	ed.line++
	ed.pos = 0
	for len(ed.lines) <= ed.line {
		ed.lines = append(ed.lines, nil)
	}
}

func addByte(ed *editor, b byte) {
	// TBD handle new char not at end
	// update line that changes
	// handle wraps
	// handle end of window
	if _, err := ed.out.Write([]byte{b}); err != nil {
		panic(err)
	}
	ed.lines[ed.line] = append(ed.lines[ed.line], rune(b))
	ed.pos++
}

func esc(ed *editor, _ byte) {
	ed.mode = escMode
}

func esc5b(ed *editor, _ byte) {
	ed.mode = esc5bMode
}

func back(ed *editor, _ byte) {
	ed.pos--
	if ed.pos < 0 {
		if 0 < ed.line {
			ed.line--
			ed.pos = len(ed.lines[ed.line])
		} else {
			ed.pos = 0
		}
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func forward(ed *editor, _ byte) {
	ed.pos++
	if len(ed.lines[ed.line]) < ed.pos {
		if ed.line+1 < len(ed.lines) {
			ed.line++
			ed.pos = 0
		} else {
			ed.pos = len(ed.lines[ed.line])
		}
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func up(ed *editor, _ byte) {
	if 0 < ed.line {
		ed.line--
		if len(ed.lines[ed.line]) < ed.pos {
			ed.pos = len(ed.lines[ed.line])
		}
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func down(ed *editor, _ byte) {
	if ed.line+1 < len(ed.lines) {
		ed.line++
		if len(ed.lines[ed.line]) < ed.pos {
			ed.pos = len(ed.lines[ed.line])
		}
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func backWord(ed *editor, _ byte) {
	ed.pos--
	// Skip until a work character (non-separator) is encountered.
	first := true
lineLoop:
	for ; 0 <= ed.line; ed.line-- {
		line := ed.lines[ed.line]
		if first {
			first = false
		} else {
			ed.pos = len(line) - 1
		}
		for ; 0 <= ed.pos; ed.pos-- {
			if sepMap[line[ed.pos]] != 'x' {
				break lineLoop
			}
		}
	}
	if ed.line < 0 {
		ed.line = 0
		ed.pos = -1
	} else {
		line := ed.lines[ed.line]
		for ; 0 <= ed.pos; ed.pos-- {
			if sepMap[line[ed.pos]] == 'x' {
				break
			}
		}
	}
	ed.pos++
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func forwardWord(ed *editor, _ byte) {
	ed.pos++
	first := true
lineLoop:
	for ; ed.line < len(ed.lines); ed.line++ {
		line := ed.lines[ed.line]
		if first {
			first = false
		} else {
			ed.pos = 0
		}
		for ; ed.pos < len(line); ed.pos++ {
			if sepMap[line[ed.pos]] != 'x' {
				break lineLoop
			}
		}
	}
	if len(ed.lines) <= ed.line {
		ed.line = len(ed.lines) - 1
		ed.pos = len(ed.lines[ed.line])
	} else {
		line := ed.lines[ed.line]
		for ; ed.pos < len(line); ed.pos++ {
			if sepMap[line[ed.pos]] == 'x' {
				break
			}
		}
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func lineBegin(ed *editor, _ byte) {
	ed.pos = 0
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
}

func lineEnd(ed *editor, _ byte) {
	ed.pos = len(ed.lines[ed.line])
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
}

func matchClose(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func matchOpen(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func historyBack(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func historyForward(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func searchBack(ed *editor, _ byte) {
	// TBD search history
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func searchForward(ed *editor, _ byte) {
	// TBD search history
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func delForward(ed *editor, _ byte) {
	line := ed.lines[ed.line]
	if ed.pos < len(line) {
		line = append(line[:ed.pos], line[ed.pos+1:]...)
		ed.lines[ed.line] = line
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
		ed.clearToEnd()
		if _, err := ed.out.Write([]byte(string(line[ed.pos:]))); err != nil {
			panic(err)
		}
	} else if ed.line < len(ed.lines)-1 {
		line = ed.lines[ed.line+1]
		ed.lines = append(ed.lines[:ed.line+1], ed.lines[ed.line+2:]...)
		ed.lines[ed.line] = append(ed.lines[ed.line], line...)
		ed.setCursor(ed.v0+len(ed.lines), 0)
		ed.clearLine()
		ed.display()
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func delBack(ed *editor, _ byte) {
	line := ed.lines[ed.line]
	if 0 < ed.pos {
		ed.pos--
		line = append(line[:ed.pos], line[ed.pos+1:]...)
		ed.lines[ed.line] = line
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
		ed.clearToEnd()
		if _, err := ed.out.Write([]byte(string(line[ed.pos:]))); err != nil {
			panic(err)
		}
	} else if 0 < ed.line {
		ed.line--
		ed.pos = len(ed.lines[ed.line])
		ed.lines = append(ed.lines[:ed.line+1], ed.lines[ed.line+2:]...)
		ed.lines[ed.line] = append(ed.lines[ed.line], line...)
		ed.setCursor(ed.v0+len(ed.lines), 0)
		ed.clearLine()
		ed.display()
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func delForwardWord(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func delBackWord(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func delLineEnd(ed *editor, _ byte) {
	line := ed.lines[ed.line]
	if ed.pos < len(line) {
		line = line[:ed.pos]
		ed.lines[ed.line] = line
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
		ed.clearToEnd()
	} else if ed.line < len(ed.lines)-1 {
		line = ed.lines[ed.line+1]
		ed.lines = append(ed.lines[:ed.line+1], ed.lines[ed.line+2:]...)
		ed.lines[ed.line] = append(ed.lines[ed.line], line...)
		ed.setCursor(ed.v0+len(ed.lines), 0)
		ed.clearLine()
		ed.display()
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func swapChar(ed *editor, _ byte) {
	if 0 < ed.pos && ed.pos < len(ed.lines[ed.line]) {
		r0 := ed.lines[ed.line][ed.pos-1]
		r := ed.lines[ed.line][ed.pos]
		ed.lines[ed.line][ed.pos] = r0
		ed.lines[ed.line][ed.pos-1] = r
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos-1)
		ed.out.Write([]byte(string([]rune{r, r0})))
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	}
	ed.mode = topMode
}

func swapWord(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func collapse(ed *editor, _ byte) {
	// TBD collapse space
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func nlAfter(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func nl(ed *editor, _ byte) {
	// TBD split line
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func enterU4(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func enterU8(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func tab(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func help(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func describe(ed *editor, _ byte) {
	var (
		start int
		end   int
	)
	line := ed.lines[ed.line]
	for start = ed.pos - 1; 0 <= start; start-- {
		if sepMap[line[start]] == 'x' {
			start++
			break
		}
	}
	for end = ed.pos; end < len(line); end++ {
		if sepMap[line[end]] == 'x' {
			break
		}
	}
	if start < 0 {
		return
	}
	word := string(line[start:end])
	w, h, _ := term.GetSize(0)
	bottom := ed.v0 + len(ed.lines)

	buf := cl.AppendDescribe(nil, slip.Symbol(word), scope, w, true)
	buf = bytes.ReplaceAll(buf, []byte{'\n'}, []byte{'\n', '\r'})
	cnt := bytes.Count(buf, []byte{'\n'})
	ed.dirty = cnt + 1
	if h <= bottom+cnt {
		diff := bottom + cnt - h
		ed.scroll(diff)
		ed.v0 -= diff
	}
	ed.setCursor(ed.v0+len(ed.lines), ed.foff)
	ed.out.Write(buf)
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}
