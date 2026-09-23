// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"sync/atomic"
	"unicode/utf8"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

// return true to eval form
type bindFunc func(ed *editor, b byte) bool

const (
	//   0123456789abcdef0123456789abcdef
	sepMap = "" +
		".........xx..x.................." + // 0x00
		"x.x....xxx................xx...." //   0x20
	sepMapLen = rune(len(sepMap))
)

var (
	topMode  []bindFunc
	rootMode = []bindFunc{
		bad, lineBegin, back, done, delForward, lineEnd, forward, bad, // 0x00
		help, tab, nl, delLineEnd, bad, enter, down, nlAfter, // 0x08
		up, bad, searchBack, searchForward, swapChar, clearForm, historyForward, ccut, // 0x10
		bad, cpaste, bad, esc, bad, bad, bad, describe, // 0x18
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
		bad, bad, matchClose, bad, bad, editForm, matchOpen, bad, // 0x00
		bad, bad, bad, bad, bad, bad, bad, bad, // 0x08
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x10
		bad, bad, bad, bad, bad, bad, bad, bad, // 0x20
		bad, bad, bad, bad, searchStashBack, bad, searchStashForward, describe, // 0x28
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, describe, // 0x30
		bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, termKey, // 0x40
		bad, bad, bad, nthStash, bad, enterUnicode, bad, bad, // 0x50
		bad, bad, bad, esc5b, collapse, bad, bad, bad, // 0x58
		bad, bad, backWord, bad, delForwardWord, eval, forwardWord, bad, // 0x60
		nthHistory, bad, bad, bad, bad, bad, stashForward, bad, // 0x68
		stashBack, bad, resetTerm, stashAdd, bad, enterUnicode, historyBack, ccopy, // 0x70
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
		bad, csi1, csi2, csi3, termKey, termKey, termKey, bad, bad, bad, bad, bad, bad, bad, bad, bad, // 0x30
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
	// Modified keys in the xterm form of esc [ 1 ; <modifier> <key> along
	// with esc [ 1 ~ for home.
	csi1Mode   []bindFunc
	csiModMode []bindFunc
	// Delete key as esc [ 3 ~ and esc [ 3 ; <modifier> ~.
	csi3Mode    []bindFunc
	csi3ModMode []bindFunc
	// Insert as esc [ 2 ~ and F9 to F12 as esc [ 2 <digit> ~.
	csi2Mode []bindFunc
	// Terminal keys such as the modified arrows, end, page up, the function
	// keys, and the esc O application keys have no default bindings but can
	// be bound by the user. This mode follows the prefix for those keys.
	termKeyMode []bindFunc
	// helpRows are the key bindings displayed on the help page. The key is the
	// canonical key name. The label defaults to the key and the description
	// defaults to the doc of the action bound to the key.
	helpRows = []struct {
		key   string
		label string
		desc  string
	}{
		{key: "C-a"},
		{key: "C-b"},
		{key: "C-c"},
		{key: "C-d"},
		{key: "C-e"},
		{key: "C-f"},
		{key: "C-h"},
		{key: "C-j"},
		{key: "C-k"},
		{key: "C-n"},
		{key: "C-o"},
		{key: "C-p"},
		{key: "C-r"},
		{key: "C-s"},
		{key: "C-t"},
		{key: "C-u"},
		{key: "C-v"},
		{key: "C-w"},
		{key: "C-y"},
		{key: "M-C-b"},
		{key: "M-C-e"},
		{key: "M-C-f"},
		{key: "C-_", label: "C-/"},
		{key: "M-/"},
		{key: "M-?"},
		{key: "M-\\"},
		{key: "M-b"},
		{key: "M-d"},
		{key: "M-e"},
		{key: "M-f"},
		{key: "M-h"},
		{key: "M-r"},
		{key: "M-s"},
		{key: "M-S"},
		{key: "M-u", desc: "enter 4 byte unicode"},
		{key: "M-U", desc: "enter 8 byte unicode"},
		{key: "M-v"},
		{key: "M-w"},
		{key: "M-,"},
		{key: "M-."},
		{key: "TAB"},
		{key: "S-<tab>"},
		{key: "DEL"},
		{key: "M-DEL"},
		{key: "<up>"},
		{key: "<down>"},
		{key: "<right>"},
		{key: "<left>"},
		{key: "RET", label: "ENTER"},
	}
	// Functions used by the history and stash overrides for user bound
	// actions.
	historyAliases map[string]bindFunc
	stashAliases   map[string]bindFunc
	// Update this if the key bindings for history are changed.
	historyBindings map[string]bindFunc
	stashBindings   map[string]bindFunc
)

func init() {
	topMode = rootMode
	// The modified arrow, home, end, delete, and function keys have no
	// default bindings but the prefixes are defined so the keys can be
	// bound by the user. F5 to F8 are esc [ 1 <digit> ~.
	csi1Mode = sparseMode(map[byte]bindFunc{';': csiMod, '5': termKey, '7': termKey, '8': termKey, '9': termKey})
	// Alt (3) and ctrl (5) modifiers.
	csiModMode = sparseMode(map[byte]bindFunc{'3': termKey, '5': termKey})
	csi3Mode = sparseMode(map[byte]bindFunc{';': csi3Mod})
	csi3ModMode = sparseMode(map[byte]bindFunc{'5': termKey})
	csi2Mode = sparseMode(map[byte]bindFunc{'0': termKey, '1': termKey, '3': termKey, '4': termKey})
	termKeyMode = sparseMode(nil)
	// Update this if the key bindings for history are changed.
	historyBindings = map[string]bindFunc{
		"\x16":      historyForward,
		"\x0e":      historyForward,
		"\x1b\x5bB": historyForward,
		"\x1bv":     historyBack,
		"\x10":      historyBack,
		"\x1b\x5bA": historyBack,
		"\x12":      searchBack,
		"\x13":      searchForward,
	}
	stashBindings = map[string]bindFunc{
		"\x1bn": stashForward,
		// "\x1b\x1b\x5b\x42": stashForward, // TBD need 4 way mapping
		"\x1bp": stashBack,
		// "\x1b\x1b\x5b\x41": stashBack, // TBD need 4 way mapping
		"\x1b,": searchStashBack,
		"\x1b.": searchStashForward,
	}
	historyAliases = map[string]bindFunc{
		"history-back":           historyBack,
		"previous-line":          historyBack,
		"history-forward":        historyForward,
		"next-line":              historyForward,
		"search-history-back":    searchBack,
		"search-history-forward": searchForward,
	}
	stashAliases = map[string]bindFunc{
		"stash-back":           stashBack,
		"stash-forward":        stashForward,
		"search-stash-back":    searchStashBack,
		"search-stash-forward": searchStashForward,
	}
	initActions()
}

// sparseMode returns a mode with only the provided bindings, all other keys
// are bad.
func sparseMode(binds map[byte]bindFunc) []bindFunc {
	mode := make([]bindFunc, 256)
	for i := range mode {
		mode[i] = bad
	}
	for b, f := range binds {
		mode[b] = f
	}
	return mode
}

const hexMap = "0123456789abcdef"

func bad(ed *editor, b byte) bool {
	ed.logf("=> %02x bad\n", b)
	ed.write([]byte{0x07})
	// The rest of the bytes read are part of the undefined key and are
	// discarded.
	sq := ed.keyBytes
	if ed.keyNext < ed.key.cnt {
		sq = append(sq[:len(sq):len(sq)], ed.key.buf[ed.keyNext:ed.key.cnt]...)
	}
	msg := fmt.Appendf(nil, "key %s is undefined. sequence: %#v", formatKey(sq), sq)
	ed.key.cnt = 0
	ed.displayMessage(msg)
	ed.mode = topMode
	return false
}

func done(ed *editor, b byte) bool {
	ed.logf("=> %02x done\n", b)
	ed.setCursor(ed.v0+len(ed.lines), 1)
	panic(io.EOF)
}

func topUni(ed *editor, b byte) bool {
	ed.logf("=> %02x topUni\n", b)
	ed.uni = ed.uni[:0]
	ed.uni = append(ed.uni, b)
	ed.mode = unicodeMode
	return false
}

func addUni(ed *editor, b byte) bool {
	ed.logf("=> %02x addUni\n", b)
	ed.uni = append(ed.uni, b)
	if utf8.Valid(ed.uni) {
		r, _ := utf8.DecodeRune(ed.uni)
		ed.addRune(r)
		ed.mode = topMode
		return false
	}
	if 6 <= len(ed.uni) {
		ed.write([]byte{0x07})
		msg := fmt.Appendf(nil, "invalid UTF-8 sequence: %#v", ed.uni)
		ed.displayMessage(msg)
		ed.mode = topMode
	}
	return false
}

func enter(ed *editor, b byte) bool {
	ed.logf("=> %02x enter\n", b)
	if evalOnClose {
		nl(ed, b)
	} else {
		ed.evalForm()
	}
	ed.mode = topMode
	return true
}

func eval(ed *editor, b byte) bool {
	ed.logf("=> %02x eval\n", b)
	ed.evalForm()
	ed.mode = topMode
	return true
}

func resetTerm(ed *editor, b byte) bool {
	ed.logf("=> %02x reset\n", b)
	ed.reset()
	ed.clearScreen()
	ed.home()
	ed.v0, _ = ed.getCursor()
	ed.setCursor(ed.v0, 1)

	return true
}

func nl(ed *editor, b byte) bool {
	ed.logf("=> %02x nl\n", b)
	bottom := ed.v0 + len(ed.lines)
	h := int(atomic.LoadInt32(&ed.height))
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
	ed.drawLine(ed.line - 1)
	ed.shift = 0
	ed.pos = 0
	for i := ed.line; i < len(ed.lines); i++ {
		ed.drawLine(i)
	}
	ed.setCursorCurrent()
	return false
}

func addByte(ed *editor, b byte) bool {
	ed.logf("=> %02x addByte\n", b)
	ed.addRune(rune(b))
	ed.mode = topMode
	return false
}

func esc(ed *editor, b byte) bool {
	ed.logf("=> %02x esc\n", b)
	ed.mode = escMode
	return false
}

func esc5b(ed *editor, b byte) bool {
	ed.logf("=> %02x esc5b\n", b)
	ed.mode = esc5bMode
	return false
}

func csi1(ed *editor, b byte) bool {
	ed.logf("=> %02x csi1\n", b)
	ed.mode = csi1Mode
	return false
}

func csiMod(ed *editor, b byte) bool {
	ed.logf("=> %02x csiMod\n", b)
	ed.mode = csiModMode
	return false
}

func csi3(ed *editor, b byte) bool {
	ed.logf("=> %02x csi3\n", b)
	ed.mode = csi3Mode
	return false
}

func csi3Mod(ed *editor, b byte) bool {
	ed.logf("=> %02x csi3Mod\n", b)
	ed.mode = csi3ModMode
	return false
}

func csi2(ed *editor, b byte) bool {
	ed.logf("=> %02x csi2\n", b)
	ed.mode = csi2Mode
	return false
}

func termKey(ed *editor, b byte) bool {
	ed.logf("=> %02x termKey\n", b)
	ed.mode = termKeyMode
	return false
}

func back(ed *editor, b byte) bool {
	ed.logf("=> %02x back\n", b)
	ed.pos--
	if ed.pos < 0 {
		if 0 < ed.line {
			ed.line--
			ed.shift = 0
			ed.pos = len(ed.lines[ed.line])
			ed.drawLine(ed.line + 1)
		} else {
			ed.pos = 0
		}
	}
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func forward(ed *editor, b byte) bool {
	ed.logf("=> %02x forward\n", b)
	ed.pos++
	if len(ed.lines[ed.line]) < ed.pos {
		if ed.line+1 < len(ed.lines) {
			ed.line++
			ed.shift = 0
			ed.pos = 0
			ed.drawLine(ed.line - 1)
		} else {
			ed.pos = len(ed.lines[ed.line])
		}
	}
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func up(ed *editor, b byte) bool {
	ed.logf("=> %02x up\n", b)
	if 0 < ed.line {
		ed.line--
		ed.shift = 0
		ed.drawLine(ed.line + 1)
		if len(ed.lines[ed.line]) < ed.pos {
			ed.pos = len(ed.lines[ed.line])
			ed.adjustShift(true)
		}
	} else if ed.lines.Empty() {
		return historyBack(ed, b)
	}
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func down(ed *editor, b byte) bool {
	ed.logf("=> %02x down\n", b)
	if ed.line+1 < len(ed.lines) {
		ed.line++
		ed.shift = 0
		ed.drawLine(ed.line - 1)
		if len(ed.lines[ed.line]) < ed.pos {
			ed.pos = len(ed.lines[ed.line])
			ed.adjustShift(true)
		}
	} else if ed.lines.Empty() {
		return historyForward(ed, b)
	}
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func backWord(ed *editor, b byte) bool {
	ed.logf("=> %02x backWord\n", b)
	n := ed.line
	ed.line, ed.pos = ed.findWordStart()
	if ed.line != n {
		ed.drawLine(n)
	}
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func forwardWord(ed *editor, b byte) bool {
	ed.logf("=> %02x forwardWord\n", b)
	n := ed.line
	ed.line, ed.pos = ed.findWordEnd()
	if ed.line != n {
		ed.drawLine(n)
	}
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func formBegin(ed *editor, b byte) bool {
	ed.logf("=> %02x formBegin\n", b)
	n := ed.line
	ed.line = 0
	ed.pos = 0
	ed.shift = 0
	if n != ed.line {
		ed.drawLine(n)
	}
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func formEnd(ed *editor, b byte) bool {
	ed.logf("=> %02x formEnd\n", b)
	n := ed.line
	ed.line = len(ed.lines) - 1
	ed.pos = len(ed.lines[ed.line])
	ed.shift = 0
	if n != ed.line {
		ed.drawLine(n)
	}
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func lineBegin(ed *editor, b byte) bool {
	ed.logf("=> %02x lineBegin\n", b)
	ed.pos = 0
	if 0 < ed.shift {
		ed.shift = 0
		ed.drawLine(ed.line)
	}
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func lineEnd(ed *editor, b byte) bool {
	ed.logf("=> %02x lineEnd\n", b)
	ed.pos = len(ed.lines[ed.line])
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func matchClose(ed *editor, b byte) bool {
	ed.logf("=> %02x matchClose\n", b)
	if p := ed.findOpenParen(); p != nil {
		ed.line = p.line
		ed.pos = p.pos
		ed.adjustShift(true)
		ed.setCursorCurrent()
	}
	ed.mode = topMode
	return false
}

func matchOpen(ed *editor, b byte) bool {
	ed.logf("=> %02x matchOpen\n", b)
	if p := ed.findCloseParen(); p != nil {
		ed.line = p.line
		ed.pos = p.pos
		ed.adjustShift(true)
		ed.setCursorCurrent()
	}
	ed.mode = topMode
	return false
}

// delForward deletes one forward or exits if there is nothing to delete and
// the form is empty.
func delForward(ed *editor, b byte) bool {
	ed.logf("=> %02x delForward\n", b)
	if len(ed.lines[ed.line]) <= ed.pos && len(ed.lines)-1 <= ed.line && ed.lines.Empty() {
		return done(ed, b)
	}
	return delChar(ed, b)
}

func delChar(ed *editor, b byte) bool {
	ed.logf("=> %02x delChar\n", b)
	line := ed.lines[ed.line]
	switch {
	case ed.pos < len(line):
		line = append(line[:ed.pos], line[ed.pos+1:]...)
		ed.lines[ed.line] = line
		ed.adjustShift(true)
	case ed.line < len(ed.lines)-1:
		line = ed.lines[ed.line+1]
		ed.lines = append(ed.lines[:ed.line+1], ed.lines[ed.line+2:]...)
		ed.lines[ed.line] = append(ed.lines[ed.line], line...)
		ed.adjustShift(false)
		ed.setCursor(ed.v0+len(ed.lines), 0)
		ed.clearLine()
		ed.display()
	}
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func delBack(ed *editor, b byte) bool {
	ed.logf("=> %02x delBack\n", b)
	line := ed.lines[ed.line]
	if 0 < ed.pos {
		ed.pos--
		line = append(line[:ed.pos], line[ed.pos+1:]...)
		ed.lines[ed.line] = line
		ed.adjustShift(true)
	} else if 0 < ed.line {
		ed.line--
		ed.pos = len(ed.lines[ed.line])
		ed.lines = append(ed.lines[:ed.line+1], ed.lines[ed.line+2:]...)
		ed.lines[ed.line] = append(ed.lines[ed.line], line...)
		ed.adjustShift(false)
		ed.setCursor(ed.v0+len(ed.lines), 0)
		ed.clearLine()
		ed.display()
	}
	ed.setCursorCurrent()
	ed.mode = topMode

	return false
}

func delForwardWord(ed *editor, b byte) bool {
	ed.logf("=> %02x delForwardWord\n", b)
	cnt := len(ed.lines)
	toLine, toPos := ed.findWordEnd()
	ed.deleteRange(ed.line, ed.pos, toLine, toPos)
	for i := ed.line; i < cnt; i++ {
		ed.drawLine(i)
	}
	ed.setCursorCurrent()
	ed.mode = topMode

	return false
}

func delBackWord(ed *editor, b byte) bool {
	ed.logf("=> %02x delBackWord\n", b)
	cnt := len(ed.lines)
	toLine, toPos := ed.findWordStart()
	ed.deleteRange(toLine, toPos, ed.line, ed.pos)
	for i := ed.line; i < cnt; i++ {
		ed.drawLine(i)
	}
	ed.line = toLine
	ed.pos = toPos
	ed.setCursorCurrent()
	ed.mode = topMode

	return false
}

func delLineEnd(ed *editor, b byte) bool {
	ed.logf("=> %02x delLineEnd\n", b)
	line := ed.lines[ed.line]
	if ed.pos < len(line) {
		cut := line[ed.pos:]
		cmd := exec.Command("pbcopy")
		cmd.Stdin = bytes.NewReader([]byte(string(cut)))
		// Ignore errors as pbcopy may not exist.
		_ = cmd.Run()
		line = line[:ed.pos]
		ed.lines[ed.line] = line
		ed.adjustShift(true)
	} else if ed.line < len(ed.lines)-1 {
		line = ed.lines[ed.line+1]
		ed.lines = append(ed.lines[:ed.line+1], ed.lines[ed.line+2:]...)
		ed.lines[ed.line] = append(ed.lines[ed.line], line...)
		ed.setCursor(ed.v0+len(ed.lines), 0)
		ed.clearLine()
		ed.display()
	}
	ed.setCursorCurrent()
	ed.mode = topMode

	return false
}

func swapChar(ed *editor, b byte) bool {
	ed.logf("=> %02x swapChar\n", b)
	if 0 < ed.pos && ed.pos < len(ed.lines[ed.line]) {
		r0 := ed.lines[ed.line][ed.pos-1]
		r := ed.lines[ed.line][ed.pos]
		ed.lines[ed.line][ed.pos] = r0
		ed.lines[ed.line][ed.pos-1] = r
		ed.setCursorPos(ed.line, ed.pos-1)
		ed.write([]byte(string([]rune{r, r0})))
		ed.setCursorCurrent()
	}
	ed.mode = topMode

	return false
}

func collapse(ed *editor, b byte) bool {
	ed.logf("=> %02x collapse\n", b)
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
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func nlAfter(ed *editor, b byte) bool {
	ed.logf("=> %02x nlAfter\n", b)
	nl(ed, ' ')
	ed.line--
	ed.drawLine(ed.line + 1)
	ed.pos = len(ed.lines[ed.line])
	ed.adjustShift(true)
	ed.setCursorCurrent()
	ed.mode = topMode
	return false
}

func tab(ed *editor, b byte) bool {
	ed.logf("=> %02x tab\n", b)
	ed.mode = topMode
	if ed.dirty.lines != nil {
		ed.updateDirty(1)
		return false
	}
	line := ed.lines[ed.line]
	pos := ed.pos - 1
	for ; 0 <= pos; pos-- {
		r := line[pos]
		if r <= ' ' || r == '(' || r == ')' || r == '\'' || r == ':' {
			break
		}
	}
	pos++
	if pos < ed.pos {
		word := string(line[pos:ed.pos])
		wa, lo, hi := WordMatch(word)
		if 0 < len(wa) {
			if added := expandWord(word, wa, lo, hi); 0 < len(added) {
				if ed.pos == len(ed.lines[ed.line]) {
					ed.write([]byte(string(added)))
					ed.lines[ed.line] = append(ed.lines[ed.line], added...)
					ed.pos += len(added)
				} else {
					line := ed.lines[ed.line]
					end := line[ed.pos:]
					line = append(line[:ed.pos], append(added, end...)...)
					ed.lines[ed.line] = line
					ed.write([]byte(string(line[ed.pos:])))
					ed.pos += len(added)
				}
			} else {
				ed.completer.lo = lo
				ed.completer.hi = hi
				ed.completer.index = -1
				ed.completer.target = word
				ed.override = completeOverride
				ed.displayCompletions()
			}
		}
	} else {
		ed.setCursorCurrent()
	}
	ed.mode = topMode
	return false
}

func completeOverride(ed *editor) bool {
	var name string
	if a := keyAction(string(ed.key.buf[:ed.key.cnt])); a != nil {
		name = a.name
	}
	switch name {
	case "tab", "forward-char": // next
		ed.completer.index++
		if ed.completer.hi-ed.completer.lo < ed.completer.index {
			ed.completer.index = 0
		}
	case "back-char":
		ed.completer.index--
		if ed.completer.index < 0 {
			ed.completer.index = ed.completer.hi - ed.completer.lo
		}
	case "next-line":
		if ed.completer.index < 0 {
			ed.completer.index = 0
		} else {
			ed.completer.index += ed.completer.colCnt
		}
		if ed.completer.hi-ed.completer.lo < ed.completer.index {
			ed.completer.index %= ed.completer.colCnt
		}
	case "previous-line":
		if ed.completer.index < 0 {
			ed.completer.index = 0
		}
		ed.completer.index -= ed.completer.colCnt
		if ed.completer.index < 0 {
			lastRow := (ed.completer.hi - ed.completer.lo + 1) / ed.completer.colCnt * ed.completer.colCnt
			ed.completer.index = lastRow + ed.completer.index%ed.completer.colCnt
			if ed.completer.hi-ed.completer.lo < ed.completer.index {
				ed.completer.index = ed.completer.hi - ed.completer.lo
			}
		}
	case "newline", "enter":
		if 0 <= ed.completer.index {
			word := completerWords[ed.completer.lo+ed.completer.index]
			added := []rune(word)[len(ed.completer.target):]
			if ed.pos == len(ed.lines[ed.line]) {
				ed.lines[ed.line] = append(ed.lines[ed.line], added...)
				ed.pos += len(added)
			} else {
				line := ed.lines[ed.line]
				end := line[ed.pos:]
				line = append(line[:ed.pos], append(added, end...)...)
				ed.lines[ed.line] = line
				ed.pos += len(added)
			}
			ed.drawLine(ed.line)
		}
		ed.key.cnt = 0
		ed.override = nil
		return false
	default:
		ed.override = nil
		return false
	}
	ed.displayCompletions()
	return true
}

func expandWord(word string, wa []string, lo, hi int) (added []rune) {
	w0 := []rune(wa[lo])
	for i := len(word); i < len(w0); i++ {
		r := w0[i]
		for j := lo + 1; j <= hi; j++ {
			w := []rune(wa[j])
			if len(w) <= i || w[i] != r {
				return
			}
		}
		added = append(added, r)
	}
	return
}

func shiftTab(ed *editor, b byte) bool {
	ed.logf("=> %02x shiftTab\n", b)
	if ed.dirty.lines != nil {
		ed.updateDirty(-1)
	} else {
		bad(ed, b)
	}
	ed.mode = topMode
	return false
}

func help(ed *editor, b byte) bool {
	ed.logf("=> %02x help\n", b)
	header := `__SLIP REPL Editor__


This editor includes history, tab completions, word (symbol) descriptions, and
parenthesis matching. In the key binding table __M-__ indicates pressing the
meta or option key or pressing the escape key before the rest of the
sequence. A __C-__ indicates the control key is held while pressing the key. A
shift key is denoted with a __S-__. Some keys have different behavior if the
current form is blank. The alternate behavior is included in parenthesis. Key
bindings are:

`
	ub := userBinds.Load()
	kbs := ub.list
	var keys [][]byte
	for _, row := range helpRows {
		label := row.label
		if len(label) == 0 {
			label = row.key
		}
		_, seqs, _ := parseKey(row.key)
		desc := row.desc
		if len(desc) == 0 {
			desc = defaultKeys[seqs[0]].doc
		}
		for _, seq := range seqs {
			if ub.acts[seq] != nil {
				desc += " *"
				break
			}
		}
		keys = append(keys, helpEntry(label, desc, 6))
	}
	w := int(atomic.LoadInt32(&ed.width))
	indent := 3
	leftPad := bytes.Repeat([]byte{' '}, indent)
	buf := slip.AppendDoc(nil, header, indent, w-6, true)
	buf = bytes.TrimSpace(buf)
	buf = append(buf, '\n', '\n')
	if 0 < len(kbs) {
		buf = append(buf, leftPad...)
		buf = append(buf, "Your bindings (defaults they replace are marked with *):\n"...)
		width := 0
		for _, kb := range kbs {
			width = max(width, printWidth([]byte(kb.key)))
		}
		for _, kb := range kbs {
			name := "undefined"
			if kb.act != nil {
				name = kb.act.name
			}
			buf = append(buf, leftPad...)
			buf = append(buf, helpEntry(kb.key, name, width+2)...)
			buf = append(buf, '\n')
		}
		buf = append(buf, '\n')
	}
	colCnt := w / 41 // enough for the longest key binding description plus 2 for spacing
	klines := len(keys)/colCnt + 1
	for i := 0; i < klines; i++ {
		buf = append(buf, leftPad...)
		for j := 0; j < colCnt; j++ {
			if len(keys) <= i+j*klines {
				continue
			}
			k := keys[i+j*klines]
			buf = append(buf, k...)
			buf = append(buf, bytes.Repeat([]byte{' '}, max(0, 41-printWidth(k)))...)
		}
		buf = append(buf, '\n')
	}
	ed.displayHelp(buf, w, int(atomic.LoadInt32(&ed.height)))
	ed.mode = topMode
	return false
}

// helpEntry returns a help page entry with a bold label and a description
// that starts in the seventh column unless the label is too long.
func helpEntry(label, desc string, width int) []byte {
	entry := fmt.Appendf(nil, "\x1b[1m%s\x1b[m", label)
	entry = append(entry, bytes.Repeat([]byte{' '}, max(1, width-printWidth([]byte(label))))...)
	return append(entry, desc...)
}

// printWidth returns the display width of text, ignoring ANSI sequences.
func printWidth(text []byte) (w int) {
	var esc bool
	for _, r := range string(text) {
		switch {
		case r == 0x1b:
			esc = true
		case esc:
			esc = r != 'm'
		default:
			w += RuneWidth(r)
		}
	}
	return
}

func describe(ed *editor, b byte) bool {
	ed.logf("=> %02x describe\n", b)
	var (
		start int
		end   int
	)
	line := ed.lines[ed.line]
	for start = ed.pos - 1; 0 <= start; start-- {
		r := line[start]
		if r < sepMapLen && sepMap[r] == 'x' {
			start++
			break
		}
	}
	for end = ed.pos; end < len(line); end++ {
		r := line[end]
		if r < sepMapLen && sepMap[r] == 'x' {
			break
		}
	}
	if start < 0 || end-start == 0 {
		ed.write([]byte{0x07})
		ed.displayMessage([]byte("could not determine what to describe"))
		ed.mode = topMode
		return false
	}
	word := string(line[start:end])
	h := int(atomic.LoadInt32(&ed.height))
	w := int(atomic.LoadInt32(&ed.width))
	buf := cl.AppendDescribe(nil, slip.Symbol(word), &scope, 3, w-6, true)
	buf = bytes.TrimSpace(buf)

	ed.displayHelp(buf, w, h)
	ed.mode = topMode
	return false
}

func clearForm(ed *editor, b byte) bool {
	ed.logf("=> %02x clearForm\n", b)
	ed.clearForm()
	ed.mode = topMode
	return false
}

func historyOverride(ed *editor) bool {
	f := ed.overrideBinding(historyBindings, historyAliases)
	if f == nil {
		ed.keepForm()
		ed.override = nil
		return false
	}
	f(ed, ' ')
	return true
}

func historyBack(ed *editor, b byte) bool {
	ed.logf("=> %02x historyBack\n", b)
	switch {
	case ed.override == nil:
		TheHistory.cur = len(TheHistory.forms) - 1
	case TheHistory.cur <= 0:
		ed.override = historyOverride
		ed.mode = topMode
		return false
	default:
		TheHistory.cur--
	}
	if form := TheHistory.Get(); form != nil {
		ed.setForm(form)
	}
	ed.override = historyOverride
	ed.mode = topMode
	return false
}

func historyForward(ed *editor, b byte) bool {
	ed.logf("=> %02x historyForward\n", b)
	switch {
	case ed.override == nil:
		TheHistory.cur = 0
	case len(TheHistory.forms)-1 <= TheHistory.cur:
		ed.override = historyOverride
		ed.setForm(Form{{}})
		ed.mode = topMode
		return false
	default:
		TheHistory.cur++
	}
	if form := TheHistory.Get(); form != nil {
		ed.setForm(form)
	}
	ed.override = historyOverride
	ed.mode = topMode
	return false
}

func historySearchOverride(ed *editor) bool {
	f := ed.overrideBinding(historyBindings, historyAliases)
	var b byte = 'x'
	if f == nil {
		if ed.key.buf[0] < 0x20 {
			ed.keepForm()
			ed.override = nil
			TheHistory.pattern = TheHistory.pattern[:0]
			TheHistory.searchDir = 0
			return false
		}
		if TheHistory.searchDir == forwardDir {
			f = searchForward
		} else {
			f = searchBack
		}
		b = 'r'
	}
	f(ed, b)
	return true
}

func searchBack(ed *editor, b byte) bool {
	ed.logf("=> %02x searchBack\n", b)
	if ed.override == nil {
		TheHistory.pattern = TheHistory.pattern[:0]
		TheHistory.cur = len(TheHistory.forms) - 1
	} else {
		orig := TheHistory.cur
		if 0 < len(TheHistory.pattern) {
			TheHistory.cur--
		}
		if b == 'r' {
			r, _ := utf8.DecodeRune(ed.key.buf)
			if r == '\x7f' {
				if 0 < len(TheHistory.pattern) {
					TheHistory.pattern = TheHistory.pattern[:len(TheHistory.pattern)-1]
				}
			} else {
				TheHistory.pattern = append(TheHistory.pattern, r)
			}
		}
		if form := TheHistory.SearchBack(string(TheHistory.pattern)); form != nil {
			ed.setForm(form)
		} else {
			TheHistory.cur = orig
		}
	}
	buf := fmt.Appendf(nil, "search backwards: %s", string(TheHistory.pattern))
	ed.displayMessage(buf)
	ed.override = historySearchOverride
	TheHistory.searchDir = backwardDir
	ed.mode = topMode
	return false
}

func searchForward(ed *editor, b byte) bool {
	ed.logf("=> %02x searchForward\n", b)
	if ed.override == nil {
		TheHistory.pattern = TheHistory.pattern[:0]
		TheHistory.cur = 0
	} else {
		orig := TheHistory.cur
		if 0 < len(TheHistory.pattern)-1 {
			TheHistory.cur++
		}
		if b == 'r' {
			r, _ := utf8.DecodeRune(ed.key.buf)
			if r == '\x7f' {
				if 0 < len(TheHistory.pattern) {
					TheHistory.pattern = TheHistory.pattern[:len(TheHistory.pattern)-1]
				}
			} else {
				TheHistory.pattern = append(TheHistory.pattern, r)
			}
		}
		if form := TheHistory.SearchForward(string(TheHistory.pattern)); form != nil {
			ed.setForm(form)
		} else {
			TheHistory.cur = orig
		}
	}
	buf := fmt.Appendf(nil, "search forwards: %s", string(TheHistory.pattern))
	ed.displayMessage(buf)
	ed.override = historySearchOverride
	TheHistory.searchDir = forwardDir
	ed.mode = topMode
	return false
}

func nthHistoryOverride(ed *editor) bool {
	k := ed.getKey()
	switch k {
	case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
		ed.ri = (ed.ri * 10) + uint32(k[0]-'0')
	case "\n", "\r":
		ed.override = nil
		ed.clearKey()
		if form := TheHistory.Nth(TheHistory.Size() - int(ed.ri)); 0 < len(form) {
			ed.setForm(form)
		} else {
			ed.setForm(NewForm([]byte{}))
		}
		return false
	case "\x1b": // esc
		ed.clearKey()
		ed.override = nil
		return false
	case "\x7f":
		ed.clearKey()
		ed.ri /= 10
	default:
		if k[0] < 0x20 {
			ed.override = nil
			return false
		}
		return true
	}
	ed.displayMessage(fmt.Appendf(nil, "history index: %-8d", ed.ri))

	return true
}

func nthHistory(ed *editor, b byte) bool {
	ed.logf("=> %02x nthHistory\n", b)
	ed.ri = 0
	ed.displayMessage([]byte("history index: 0"))
	ed.override = nthHistoryOverride
	ed.mode = topMode
	return false
}

func enterUnicode(ed *editor, b byte) bool {
	ed.logf("=> %02x enterUnicode\n", b)
	ed.ri = 0
	ed.displayMessage([]byte("unicode: \\u0000"))
	ed.override = unicodeOverride
	ed.mode = topMode
	return false
}

// key length in bytes
func (ed *editor) keyLen() (cnt int) {
	if 0 < ed.key.cnt {
		switch {
		case ed.key.buf[0] == 0x1b: // esc
			cnt = 1
			if 1 < ed.key.cnt {
				switch ed.key.buf[1] {
				case 0x1b: // second esc
					cnt = 4
				case 0x5b:
					cnt = 3
					if 2 < ed.key.cnt && (ed.key.buf[2] == 0x31 || ed.key.buf[2] == 0x32) {
						cnt = ed.key.cnt
					}
				case 0x4f:
					cnt = 3
				default:
					cnt = 2
				}
			}
		case ed.key.buf[0] <= 0x7f:
			cnt = 1
		default:
			_, cnt = utf8.DecodeRune(ed.key.buf)
		}
	}
	return
}

func editForm(ed *editor, b byte) bool {
	ed.logf("=> %02x editForm\n", b)
	var args []string
	xed := externalEditor
	if len(xed) == 0 {
		xed = os.Getenv("EDITOR")
		if len(xed) == 0 {
			ed.mode = topMode
			return false
		}
		parts := strings.Split(xed, " ")
		xed = parts[0]
		if len(editorFlags) == 0 {
			args = append(args, parts[1:]...)
		}
	}
	for _, sflag := range editorFlags {
		if flag, _ := sflag.(slip.String); 0 < len(flag) {
			args = append(args, string(flag))
		}
	}
	f, err := os.CreateTemp("", "*.lisp")
	if err != nil {
		panic(err)
	}
	defer func() {
		_ = f.Close()
		_ = os.Remove(f.Name())
	}()
	var buf []byte
	for _, line := range ed.lines {
		buf = append(buf, string(line)...)
		buf = append(buf, '\n')
	}
	if _, err = f.Write(buf); err != nil {
		panic(err)
	}
	_ = f.Close()

	args = append(args, f.Name())
	ed.pause.Store(true)
	defer func() {
		ed.pause.Store(false)
		// discard any key sequences pending.
		<-ed.seqChan
	}()
	cmd := exec.Command(xed, args...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err = cmd.Run(); err != nil {
		panic(fmt.Sprintf("%s: %s\n", err, stderr.String()))
	}
	if f, err = os.Open(f.Name()); err != nil {
		panic(err)
	}
	if buf, err = io.ReadAll(f); err != nil {
		panic(err)
	}
	ed.setForm(NewForm(buf))
	ed.mode = topMode

	return false
}

// copy to clipboard on macOS or if pbcopy is defined or aliased.
func ccopy(ed *editor, b byte) bool {
	ed.logf("=> %02x ccopy\n", b)
	cmd := exec.Command("pbcopy")
	cmd.Stdin = bytes.NewReader(ed.lines.Append(nil))
	if err := cmd.Run(); err != nil {
		panic(err)
	}
	ed.mode = topMode

	return false
}

// cut to clipboard on macOS or if pbcopy is defined or aliased.
func ccut(ed *editor, b byte) bool {
	ed.logf("=> %02x ccut\n", b)
	cmd := exec.Command("pbcopy")
	cmd.Stdin = bytes.NewReader(ed.lines.Append(nil))
	if err := cmd.Run(); err != nil {
		panic(err)
	}
	ed.reset()

	return true
}

// paste to clipboard on macOS or if pbpaste is defined or aliased.
func cpaste(ed *editor, b byte) bool {
	ed.logf("=> %02x cpaste\n", b)
	cmd := exec.Command("pbpaste")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	if err := cmd.Run(); err != nil {
		panic(err)
	}
	ed.queueText(buf.Bytes())

	return false
}

func stashAdd(ed *editor, b byte) bool {
	ed.logf("=> %02x stashAdd\n", b)
	TheStash.Add(ed.lines)
	ed.mode = topMode

	return false
}

func nthStashOverride(ed *editor) bool {
	k := ed.getKey()
	switch k {
	case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
		ed.ri = (ed.ri * 10) + uint32(k[0]-'0')
	case "\n", "\r":
		ed.override = nil
		ed.clearKey()
		if form := TheStash.Nth(TheStash.Size() - int(ed.ri)); 0 < len(form) {
			ed.setForm(form)
		} else {
			ed.setForm(NewForm([]byte{}))
		}
		return false
	case "\x1b": // esc
		ed.clearKey()
		ed.override = nil
		return false
	case "\x7f":
		ed.clearKey()
		ed.ri /= 10
	default:
		if k[0] < 0x20 {
			ed.override = nil
			return false
		}
		return true
	}
	ed.displayMessage(fmt.Appendf(nil, "stash index: %-8d", ed.ri))

	return true
}

func nthStash(ed *editor, b byte) bool {
	ed.logf("=> %02x nthStash\n", b)
	ed.ri = 0
	ed.displayMessage([]byte("stash index: 0"))
	ed.override = nthStashOverride
	ed.mode = topMode
	return false
}

func stashOverride(ed *editor) bool {
	f := ed.overrideBinding(stashBindings, stashAliases)
	if f == nil {
		ed.keepForm()
		ed.override = nil
		return false
	}
	f(ed, ' ')
	return true
}

func stashBack(ed *editor, b byte) bool {
	ed.logf("=> %02x stashBack\n", b)
	switch {
	case ed.override == nil:
		TheStash.cur = len(TheStash.forms) - 1
	case TheStash.cur <= 0:
		ed.override = stashOverride
		ed.mode = topMode
		return false
	default:
		TheStash.cur--
	}
	if form := TheStash.Get(); form != nil {
		ed.setForm(form)
	}
	ed.override = stashOverride
	ed.mode = topMode
	return false
}

func stashForward(ed *editor, b byte) bool {
	ed.logf("=> %02x stashForward\n", b)
	switch {
	case ed.override == nil:
		TheStash.cur = 0
	case len(TheStash.forms)-1 <= TheStash.cur:
		ed.override = stashOverride
		ed.setForm(Form{{}})
		ed.mode = topMode
		return false
	default:
		TheStash.cur++
	}
	if form := TheStash.Get(); form != nil {
		ed.setForm(form)
	}
	ed.override = stashOverride
	ed.mode = topMode
	return false
}

func stashSearchOverride(ed *editor) bool {
	f := ed.overrideBinding(stashBindings, stashAliases)
	var b byte = 'x'
	if f == nil {
		if ed.key.buf[0] < 0x20 {
			ed.keepForm()
			ed.override = nil
			TheStash.pattern = TheStash.pattern[:0]
			TheStash.searchDir = 0
			return false
		}
		if TheStash.searchDir == forwardDir {
			f = searchStashForward
		} else {
			f = searchStashBack
		}
		b = 'r'
	}
	f(ed, b)
	return true
}

func searchStashBack(ed *editor, b byte) bool {
	ed.logf("=> %02x searchStashBack\n", b)
	if ed.override == nil {
		TheStash.pattern = TheStash.pattern[:0]
		TheStash.cur = len(TheStash.forms) - 1
	} else {
		orig := TheStash.cur
		if 0 < len(TheStash.pattern) {
			TheStash.cur--
		}
		if b == 'r' {
			r, _ := utf8.DecodeRune(ed.key.buf)
			if r == '\x7f' {
				if 0 < len(TheStash.pattern) {
					TheStash.pattern = TheStash.pattern[:len(TheStash.pattern)-1]
				}
			} else {
				TheStash.pattern = append(TheStash.pattern, r)
			}
		}
		if form := TheStash.SearchBack(string(TheStash.pattern)); form != nil {
			ed.setForm(form)
		} else {
			TheStash.cur = orig
		}
	}
	buf := fmt.Appendf(nil, "search stash backwards: %s", string(TheStash.pattern))
	ed.displayMessage(buf)
	ed.override = stashSearchOverride
	TheStash.searchDir = backwardDir
	ed.mode = topMode
	return false
}

func searchStashForward(ed *editor, b byte) bool {
	ed.logf("=> %02x searchStashForward\n", b)
	if ed.override == nil {
		TheStash.pattern = TheStash.pattern[:0]
		TheStash.cur = 0
	} else {
		orig := TheStash.cur
		if 0 < len(TheStash.pattern)-1 {
			TheStash.cur++
		}
		if b == 'r' {
			r, _ := utf8.DecodeRune(ed.key.buf)
			if r == '\x7f' {
				if 0 < len(TheStash.pattern) {
					TheStash.pattern = TheStash.pattern[:len(TheStash.pattern)-1]
				}
			} else {
				TheStash.pattern = append(TheStash.pattern, r)
			}
		}
		if form := TheStash.SearchForward(string(TheStash.pattern)); form != nil {
			ed.setForm(form)
		} else {
			TheStash.cur = orig
		}
	}
	buf := fmt.Appendf(nil, "search stash forwards: %s", string(TheStash.pattern))
	ed.displayMessage(buf)
	ed.override = stashSearchOverride
	TheStash.searchDir = forwardDir
	ed.mode = topMode
	return false
}

func (ed *editor) getKey() string {
	return string(ed.key.buf[:ed.keyLen()])
}

func (ed *editor) clearKey() {
	cnt := ed.keyLen()
	copy(ed.key.buf, ed.key.buf[:cnt])
	ed.key.cnt -= cnt
}

func unicodeOverride(ed *editor) bool {
	k := ed.getKey()
	switch k {
	case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
		ed.ri = (ed.ri << 4) + uint32(k[0]-'0')
	case "a", "b", "c", "d", "e", "f":
		ed.ri = (ed.ri << 4) + uint32(k[0]-'a') + 10
	case "A", "B", "C", "D", "E", "F":
		ed.ri = (ed.ri << 4) + uint32(k[0]-'A') + 10
	case "\n", "\r":
		ed.override = nil
		ed.clearKey()
		if !utf8.ValidRune(rune(ed.ri)) {
			ed.clearKey()
			ed.displayMessage(fmt.Appendf(nil, "\\u%x is not a valid code point", ed.ri))
		} else {
			ed.addRune(rune(ed.ri))
		}
		return false
	case "\x1b": // esc
		ed.clearKey()
		ed.override = nil
		return false
	case "\x7f":
		ed.clearKey()
		ed.ri >>= ed.ri
	default:
		if k[0] < 0x20 {
			ed.override = nil
			return false
		}
		ed.displayMessage(fmt.Appendf(nil, "%s is not a valid hexadecimal character", k))
		return true
	}
	var buf []byte
	switch {
	case utf8.MaxRune < rune(ed.ri):
		buf = fmt.Appendf(nil, "\\u%x is not a valid code point", ed.ri)
	case 0xFFFF < ed.ri:
		buf = fmt.Appendf(nil, "unicode: \\u%08x", ed.ri)
	default:
		buf = fmt.Appendf(nil, "unicode: \\u%04x", ed.ri)
	}
	ed.displayMessage(buf)
	return true
}
