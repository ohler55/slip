// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"fmt"
	"io"
	"unicode/utf8"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/repl/term"
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
		help, tab, nl, delLineEnd, bad, enter, down, nlAfter, // 0x08
		up, bad, searchBack, searchForward, swapChar, bad, historyForward, bad, // 0x10
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
		topName,
	}
	escMode = []bindFunc{
		bad, bad, matchClose, bad, bad, bad, matchOpen, bad, // 0x00
		bad, bad, bad, bad, bad, bad, bad, bad, // 0x08
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x10
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, describe, // 0x20
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, describe, // 0x30
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x40
		bad, bad, bad, bad, bad, enterU8, bad, bad, // 0x50
		bad, bad, bad, esc5b, collapse, bad, bad, bad, // 0x58
		bad, bad, backWord, bad, delForwardWord, bad, forwardWord, bad, // 0x60
		bad, bad, bad, bad, bad, bad, bad, bad, // 0x68
		bad, bad, bad, bad, bad, enterU4, historyBack, bad, // 0x70
		bad, bad, bad, bad, bad, bad, bad, delBackWord, // 0x78
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x80
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x90
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xa0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xb0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xc0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xd0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xe0
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0xf0
		escName,
	}
	esc5bMode = []bindFunc{
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x00
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x10
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x20
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x30
		bad, up, down, forward, back, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x40
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, shiftTab, bad, bad, bad, bad, bad, // 0x50
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
		esc5bName,
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
		unicodeName,
	}
	// Update this if the key bindings for history are changed.
	historyBindings map[string]bindFunc
)

func init() {
	topMode = rootMode
	// Update this if the key bindings for history are changed.
	historyBindings = map[string]bindFunc{
		"\x16":  historyForward,
		"\x1bv": historyBack,
		"\x12":  searchBack,
		"\x13":  searchForward,
	}
}

func topName(ed *editor, b byte) {
	ed.msg = ""
}

func escName(ed *editor, b byte) {
	ed.msg = "M-"
}

func esc5bName(ed *editor, b byte) {
	ed.msg = "esc [ "
}

func unicodeName(ed *editor, b byte) {
	ed.msg = "unicode "
}

func (ed *editor) modeName() string {
	e := editor{}
	ed.mode[256](&e, 0)
	return e.msg
}

const hexMap = "0123456789abcdef"

func bad(ed *editor, b byte) {
	_, _ = ed.out.Write([]byte{0x07})
	mod := ed.modeName()
	var charName []byte
	for i := 0; i < ed.kcnt; i++ {
		c := ed.key[i]
		switch {
		case c == 0x1b:
			if mod != "M-" {
				charName = append(charName, 'M', '-')
			}
		case c < 0x20:
			charName = append(charName, 'C', '-', 'a'+c-1)
		case c == 0x7f:
			charName = append(charName, 'D', 'E', 'L')
		case 0x80 <= c:
			charName = append(charName, '\\', 'u', '0', '0', hexMap[c>>4], hexMap[c&0x0f])
		default:
			charName = append(charName, c)
		}
	}
	seq := ed.key[:ed.kcnt]
	msg := fmt.Appendf(nil, "key %s%s is undefined. sequence: %#v", mod, charName, seq)
	ed.kcnt = 0
	ed.displayMessage(msg)
	ed.mode = topMode
}

func done(ed *editor, b byte) {
	ed.setCursor(ed.v0+len(ed.lines), 1)
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

func enter(ed *editor, _ byte) {
	_, h := term.GetSize(0)
	bottom := ed.v0 + len(ed.lines)
	if h <= bottom {
		diff := bottom + 1 - h
		ed.scroll(diff)
		ed.v0 -= diff
	}
	ed.line = len(ed.lines) - 1
	ed.pos = len(ed.lines[ed.line])
	// Let the editor handle the eval.
}

func nl(ed *editor, b byte) {
	_, h := term.GetSize(0)
	bottom := ed.v0 + len(ed.lines)
	if h <= bottom {
		diff := bottom + 1 - h
		ed.scroll(diff)
		ed.v0 -= diff
	}
	line := ed.lines[ed.line]
	ed.lines = append(ed.lines, nil)
	ed.line++
	if ed.line < len(ed.lines)-1 {
		copy(ed.lines[ed.line+1:], ed.lines[ed.line:])
	}
	if ed.pos < len(line) {
		ed.lines[ed.line-1] = line[:ed.pos]
		ed.lines[ed.line] = line[ed.pos:]
	}
	ed.setCursor(ed.v0+ed.line-1, ed.foff+ed.pos)
	ed.clearToEnd()
	for i := ed.line; i < len(ed.lines); i++ {
		ed.setCursor(ed.v0+i, ed.foff)
		ed.clearToEnd()
		_, _ = ed.out.Write([]byte(string(ed.lines[i])))
	}
	ed.pos = 0
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
}

func addByte(ed *editor, b byte) {
	if ed.pos == len(ed.lines[ed.line]) {
		if _, err := ed.out.Write([]byte{b}); err != nil {
			panic(err)
		}
		ed.lines[ed.line] = append(ed.lines[ed.line], rune(b))
		ed.pos++
	} else {
		line := ed.lines[ed.line]
		line = append(line, ' ') // make sure there is space for a new rune
		copy(line[ed.pos+1:], line[ed.pos:])
		line[ed.pos] = rune(b)
		ed.lines[ed.line] = line
		if _, err := ed.out.Write([]byte(string(line[ed.pos:]))); err != nil {
			panic(err)
		}
		ed.pos++
	}
	// TBD handle wraps
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
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
	ed.line, ed.pos = ed.findWordStart()
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func forwardWord(ed *editor, _ byte) {
	ed.line, ed.pos = ed.findWordEnd()
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
	if p := ed.findOpenParen(); p != nil {
		ed.line = p.line
		ed.pos = p.pos
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	}
	ed.mode = topMode
}

func matchOpen(ed *editor, _ byte) {
	if p := ed.findCloseParen(); p != nil {
		ed.line = p.line
		ed.pos = p.pos
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	}
	ed.mode = topMode
}

func delForward(ed *editor, _ byte) {
	line := ed.lines[ed.line]
	if ed.pos < len(line) {
		line = append(line[:ed.pos], line[ed.pos+1:]...)
		ed.lines[ed.line] = line
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
		ed.clearToEnd()
		_, _ = ed.out.Write([]byte(string(line[ed.pos:])))
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
		_, _ = ed.out.Write([]byte(string(line[ed.pos:])))
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
	toLine, toPos := ed.findWordEnd()
	for i := ed.line; i < len(ed.lines); i++ {
		ed.setCursor(ed.v0+i, ed.foff)
		ed.clearToEnd()
	}
	ed.deleteRange(ed.line, ed.pos, toLine, toPos)
	for i := ed.line; i < len(ed.lines); i++ {
		ed.setCursor(ed.v0+i, ed.foff)
		_, _ = ed.out.Write([]byte(string(ed.lines[i])))
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func delBackWord(ed *editor, _ byte) {
	toLine, toPos := ed.findWordStart()
	for i := toLine; i < len(ed.lines); i++ {
		ed.setCursor(ed.v0+i, ed.foff)
		ed.clearToEnd()
	}
	ed.deleteRange(toLine, toPos, ed.line, ed.pos)
	ed.line = toLine
	ed.pos = toPos
	for i := toLine; i < len(ed.lines); i++ {
		ed.setCursor(ed.v0+i, ed.foff)
		_, _ = ed.out.Write([]byte(string(ed.lines[i])))
	}
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
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
		_, _ = ed.out.Write([]byte(string([]rune{r, r0})))
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	}
	ed.mode = topMode
}

func collapse(ed *editor, _ byte) {
	start := ed.pos - 1
	end := ed.pos
	line := ed.lines[ed.line]
	for ; 0 <= start; start-- {
		if line[start] != ' ' {
			break
		}
	}
	start++
	for ; end < len(line); end++ {
		if line[end] != ' ' {
			break
		}
	}
	ed.lines[ed.line] = append(line[:start], line[end:]...)
	ed.pos = start
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.clearToEnd()
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	_, _ = ed.out.Write([]byte(string(ed.lines[ed.line][ed.pos:])))
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func nlAfter(ed *editor, _ byte) {
	nl(ed, ' ')
	ed.line--
	ed.pos = len(ed.lines[ed.line])
	ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos)
	ed.mode = topMode
}

func tab(ed *editor, _ byte) {
	ed.mode = topMode
	if ed.dirty.lines != nil {
		ed.updateDirty(1)
		return
	}
	// TBD completion
}

func shiftTab(ed *editor, _ byte) {
	if ed.dirty.lines != nil {
		ed.updateDirty(-1)
	} else {
		_, _ = ed.out.Write([]byte{0x07})
	}
	ed.mode = topMode
}

func help(ed *editor, _ byte) {
	header := `__SLIP REPL Editor__


This editor includes history, tab completions, word (symbol) descriptions, and
parenthesis matching. In the key binding table __M-__ indicates pressing the
meta or option key or pressing the escape key before the rest of the
sequence. A __C-__ indicates the control key is held while pressing the key. A
shift key is denoted with a __^__ character. Key bindings are:

`
	keys := []string{
		"\x1b[1mC-a\x1b[m   move to line start",
		"\x1b[1mC-b\x1b[m   move left one",
		"\x1b[1mC-c\x1b[m   exit",
		"\x1b[1mC-d\x1b[m   delete one forward",
		"\x1b[1mC-e\x1b[m   move to line end",
		"\x1b[1mC-f\x1b[m   move right one",
		"\x1b[1mC-h\x1b[m   show this help page",
		"\x1b[1mTAB\x1b[m   word completion or help scroll",
		"\x1b[1mS-TAB\x1b[m help scroll back",
		"\x1b[1mC-k\x1b[m   delete to line end",
		"\x1b[1mC-j\x1b[m   insert newline",
		"\x1b[1mC-n\x1b[m   move down one",
		"\x1b[1mC-o\x1b[m   insert newline after",
		"\x1b[1mC-p\x1b[m   move up one",
		"\x1b[1mC-r\x1b[m   search history back",
		"\x1b[1mC-s\x1b[m   search history forward",
		"\x1b[1mC-t\x1b[m   swap characters",
		"\x1b[1mC-v\x1b[m   next in history",
		"\x1b[1mDEL\x1b[m   delete one back",
		"\x1b[1mM-C-b\x1b[m move back to matching paren",
		"\x1b[1mM-C-f\x1b[m move forward to matching paren",
		"\x1b[1mC-/\x1b[m   describe word",
		"\x1b[1mM-/\x1b[m   describe word",
		"\x1b[1mM-?\x1b[m   describe word",
		"\x1b[1mM-\\\x1b[m   collapse space",
		"\x1b[1mM-b\x1b[m   move back one word",
		"\x1b[1mM-d\x1b[m   delete one word",
		"\x1b[1mM-f\x1b[m   move forward one word",
		"\x1b[1mM-u\x1b[m   enter 4 byte unicode",
		"\x1b[1mM-U\x1b[m   enter 8 byte unicode",
		"\x1b[1mM-v\x1b[m   previous in history",
		"\x1b[1mM-DEL\x1b[m delete previous word",
		"\x1b[1mENTER\x1b[m evaluate form",
	}
	w, _ := term.GetSize(0)
	w -= 6
	indent := 3
	leftPad := bytes.Repeat([]byte{' '}, indent)
	buf := slip.AppendDoc(nil, header, indent, w, true)
	buf = bytes.TrimSpace(buf)
	buf = append(buf, '\n', '\n')

	colCnt := w / 38 // enough for the longest key binding plus 2 for spacing
	klines := len(keys)/colCnt + 1
	for i := 0; i < klines; i++ {
		buf = append(buf, leftPad...)
		for j := 0; j < colCnt; j++ {
			if len(keys) <= i+j*klines {
				continue
			}
			k := keys[i+j*klines]
			buf = append(buf, k...)
			// 45 is the map length plus the 7 bytes used for making the key
			// sequence bold.
			buf = append(buf, bytes.Repeat([]byte{' '}, 45-len(k))...)
		}
		buf = append(buf, '\n')
	}
	ed.displayHelp(buf)
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
	if start < 0 || end-start == 0 {
		_, _ = ed.out.Write([]byte{0x07})
		ed.displayMessage([]byte("could not determine what to describe"))
		ed.mode = topMode
		return
	}
	word := string(line[start:end])
	w, _ := term.GetSize(0)
	buf := cl.AppendDescribe(nil, slip.Symbol(word), &scope, 3, w-7, true)
	buf = bytes.TrimSpace(buf)

	ed.displayHelp(buf)
}

func formDup(form [][]rune) [][]rune {
	d := make([][]rune, len(form))
	for i, line := range form {
		l2 := make([]rune, len(line))
		copy(l2, line)
		d[i] = l2
	}
	return d
}

func historyOverride(ed *editor) bool {
	k := string(ed.key[:ed.kcnt])
	f := historyBindings[k]
	if f == nil {
		ed.keepForm()
		ed.override = nil
		return false
	}
	f(ed, ' ')
	return true
}

func historyBack(ed *editor, _ byte) {
	switch {
	case ed.override == nil:
		ed.hist.cur = len(ed.hist.forms) - 1
	case ed.hist.cur <= 0:
		ed.override = historyOverride
		ed.mode = topMode
		return
	default:
		ed.hist.cur--
	}
	if form := ed.hist.get(); form != nil {
		ed.setForm(form)
	}
	ed.override = historyOverride
	ed.mode = topMode
}

func historyForward(ed *editor, _ byte) {
	switch {
	case ed.override == nil:
		ed.hist.cur = 0
	case len(ed.hist.forms)-1 <= ed.hist.cur:
		ed.override = historyOverride
		ed.setForm([][]rune{{}})
		ed.mode = topMode
		return
	default:
		ed.hist.cur++
	}
	if form := ed.hist.get(); form != nil {
		ed.setForm(form)
	}
	ed.override = historyOverride
	ed.mode = topMode
}

func historySearchOverride(ed *editor) bool {
	k := string(ed.key[:ed.kcnt])
	f := historyBindings[k]
	var b byte = 'x'
	if f == nil {
		if ed.key[0] < 0x20 {
			ed.keepForm()
			ed.override = nil
			ed.hist.pattern = ed.hist.pattern[:0]
			ed.hist.searchDir = 0
			return false
		}
		if ed.hist.searchDir == forwardDir {
			f = searchForward
		} else {
			f = searchBack
		}
		b = 'r'
	}
	f(ed, b)
	return true
}

func searchBack(ed *editor, b byte) {
	switch {
	case ed.override == nil:
		ed.hist.pattern = ed.hist.pattern[:0]
		ed.hist.cur = len(ed.hist.forms) - 1
	default:
		start := ed.hist.cur
		if 0 < len(ed.hist.pattern) {
			start--
		}
		if b == 'r' {
			r, _ := utf8.DecodeRune(ed.key)
			if r == '\x7f' {
				if 0 < len(ed.hist.pattern) {
					ed.hist.pattern = ed.hist.pattern[:len(ed.hist.pattern)-1]
				}
			} else {
				ed.hist.pattern = append(ed.hist.pattern, r)
			}
		}
		if form := ed.hist.searchBack(start, string(ed.hist.pattern)); form != nil {
			ed.setForm(form)
			ed.hist.cur = start
		}
	}
	buf := fmt.Appendf(nil, "search backwards: %s", string(ed.hist.pattern))
	ed.displayMessage(buf)
	ed.override = historySearchOverride
	ed.hist.searchDir = backwardDir
	ed.mode = topMode
}

func searchForward(ed *editor, b byte) {
	switch {
	case ed.override == nil:
		ed.hist.pattern = ed.hist.pattern[:0]
		ed.hist.cur = 0
	default:
		start := ed.hist.cur
		if 0 < len(ed.hist.pattern) {
			start++
		}
		if b == 'r' {
			r, _ := utf8.DecodeRune(ed.key)
			if r == '\x7f' {
				if 0 < len(ed.hist.pattern) {
					ed.hist.pattern = ed.hist.pattern[:len(ed.hist.pattern)-1]
				}
			} else {
				ed.hist.pattern = append(ed.hist.pattern, r)
			}
		}
		if form := ed.hist.searchForward(start, string(ed.hist.pattern)); form != nil {
			ed.setForm(form)
			ed.hist.cur = start
		}
	}
	buf := fmt.Appendf(nil, "search forwards: %s", string(ed.hist.pattern))
	ed.displayMessage(buf)
	ed.override = historySearchOverride
	ed.hist.searchDir = backwardDir
	ed.mode = topMode
}

func enterU4(ed *editor, _ byte) {
	// TBD use status line
	//  setup an override
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}

func enterU8(ed *editor, _ byte) {
	// TBD
	_, _ = ed.out.Write([]byte{0x07})
	ed.mode = topMode
}
