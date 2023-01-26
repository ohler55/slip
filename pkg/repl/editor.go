// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/signal"
	"sync/atomic"
	"syscall"
	"unicode/utf8"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl/term"
)

const MAX_DIM = 9999

type seq struct {
	cnt int
	buf []byte
}

type editor struct {
	lines      Form
	v0         int // first terminal line of display
	key        seq
	uni        []byte
	ri         uint32
	msg        string
	mode       []bindFunc
	line       int
	pos        int
	foff       int // form offset (right after prompt)
	dirty      dirty
	match      point // matching parens that needs to be redrawn on move
	lastSpot   point
	in         io.Reader
	fd         int // in fd
	out        io.Writer
	width      int32
	height     int32
	depth      int
	shift      int // rune shift on current line
	origState  *term.State
	hist       History
	override   func(ed *editor) bool // return true if handled
	completer  Completer
	resizeChan chan os.Signal
	seqChan    chan *seq
}

func (ed *editor) initialize() {
	if ed.out != nil {
		return
	}
	ed.out = scope.Get(slip.Symbol(stdOutput)).(io.Writer)
	ed.mode = topMode
	ed.key.buf = make([]byte, 32)
	ed.match.line = -1
	ed.hist.SetLimit(1000) // initial value that the user can replace by setting *repl-history-limit*
	ed.hist.Load(historyFilename)
	ed.foff = printSize(prompt) + 1 // terminal positions are one based and not zero based so add one
	ed.seqChan = make(chan *seq, 100)
	ed.in = scope.Get(slip.Symbol(stdInput)).(io.Reader)
	if fs, ok := ed.in.(*slip.FileStream); ok {
		ed.fd = int(((*os.File)(fs)).Fd())
		ed.origState = term.MakeRaw(ed.fd)
	}
	ed.completer.Init()
	for name := range slip.CurrentPackage.Funcs {
		ed.completer.Insert(name)
	}
	for name := range slip.CurrentPackage.Vars {
		ed.completer.Insert(name)
	}
	ed.completer.Sort()

	go ed.chanRead()

	_, _ = ed.out.Write([]byte("Entering the SLIP REPL editor. Type ctrl-h for help and key bindings.\n"))
	ed.v0, _ = ed.getCursor()
	_, _ = ed.getSize()
	ed.setCursor(ed.v0, 1)

	ed.resizeChan = make(chan os.Signal, 10)
	signal.Notify(ed.resizeChan, syscall.SIGWINCH)
}

func (ed *editor) stop() {
	if ed.origState != nil {
		term.Restore(ed.fd, ed.origState)
		ed.origState = nil
	}
	if ed.resizeChan != nil {
		ed.resizeChan <- nil
		ed.resizeChan = nil
	}
	ed.seqChan <- nil
	ed.reset()
	ed.out = nil
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

func (ed *editor) addWord(word string) {
	ed.completer.Add(word)
}

func (ed *editor) removeWord(word string) {
	ed.completer.Remove(word)
}

func (ed *editor) display() {
	for i := 0; i < len(ed.lines); i++ {
		ed.drawLine(i)
	}
	ed.setCursorCurrent()
}

func (ed *editor) displayRune(line, pos int) {
	if 0 <= line && line < len(ed.lines) && 0 <= pos && pos < len(ed.lines[line]) {
		ed.setCursorPos(line, pos)
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

func printSize(s string) (cnt int) {
	var esc bool
	for _, r := range []rune(s) {
		switch r {
		case '\x1b':
			esc = true
		case 'm':
			if esc {
				esc = false
			}
		default:
			if !esc {
				cnt++
			}
		}
	}
	return
}

func (ed *editor) chanRead() {
	var err error
	for {
		s := seq{cnt: 0, buf: make([]byte, 32)}
		if s.cnt, err = ed.in.Read(s.buf); err != nil {
			// shutting down
			return
		}
		if 0 < s.cnt {
			ed.seqChan <- &s
		}
	}
}

func (ed *editor) read() (out []byte) {
	if len(ed.lines) == 0 {
		ed.v0, _ = ed.getCursor()
		ed.setCursor(ed.v0, 1)
		ed.clearLine()
		_, _ = ed.out.Write([]byte(prompt))
		ed.foff = printSize(prompt) + 1 // terminal positions are one based and not zero based so add one
		ed.lines = Form{{}}
	} else {
		ed.adjustShift(true)
		ed.setCursorCurrent()
	}
top:
	for {
		select {
		case key := <-ed.seqChan:
			ed.key.cnt = key.cnt
			copy(ed.key.buf, key.buf)
		case sig := <-ed.resizeChan:
			if sig == nil {
				return nil
			}
			if len(ed.resizeChan) == 0 {
				w, _ := ed.getSize()
				ed.v0, _ = ed.getCursor()
				ed.setCursor(ed.v0, 1)
				ed.clearDown()
				ed.dirty.lines = nil
				ed.override = nil
				ed.dirty.cnt = 0
				ed.display()
				if !modifiedVars["*print-right-margin*"] {
					slip.DefaultPrinter().RightMargin = uint(w)
				}
				continue top
			}
		}
		// dirty and not tab and not shift-tab
		if 0 < ed.dirty.cnt &&
			ed.key.buf[0] != 0x09 && !(ed.key.buf[0] == 0x1b && ed.key.buf[1] == 0x5b && ed.key.buf[2] == 0x5a) {
			ed.setCursor(ed.v0+len(ed.lines), 1)
			ed.clearDown()
			ed.setCursorCurrent()
			ed.dirty.lines = nil
		}
		if ed.override != nil && ed.override(ed) {
			continue
		}
		for i := 0; i < ed.key.cnt; i++ {
			b := ed.key.buf[i]
			if ed.mode[b](ed, b) {
				if 0 <= ed.match.line {
					n := ed.match.line
					ed.match.line = -1
					ed.displayRune(n, ed.match.pos)
				}
				break top
			}
		}
		if ed.lastSpot.line != ed.line || ed.lastSpot.pos != ed.pos {
			// clear last match
			if 0 <= ed.match.line {
				n := ed.match.line
				ed.match.line = -1
				ed.displayRune(n, ed.match.pos)
				ed.setCursorCurrent()
			}
			var p *point
			pos := ed.pos
			if pos < len(ed.lines[ed.line]) && ed.lines[ed.line][pos] == '(' {
				if p = ed.findCloseParen(); p != nil && 0 < p.pos {
					p.pos--
				} else {
					p = nil
				}
			} else {
				pos--
				if 0 <= pos && ed.lines[ed.line][pos] == ')' {
					p = ed.findOpenParen()
					if evalOnClose && ed.isFirstChar(p) {
						ed.evalForm()
						if 0 <= ed.match.line {
							n := ed.match.line
							ed.match.line = -1
							ed.displayRune(n, ed.match.pos)
						}
						break top
					}
				}
			}
			if p != nil {
				ed.match = *p
				ed.displayRune(p.line, p.pos)
				ed.setCursorCurrent()
			}
			ed.lastSpot.line = ed.line
			ed.lastSpot.pos = ed.pos
		}
	}
	ed.setCursor(ed.v0+len(ed.lines), 1)
	for _, line := range ed.lines {
		out = append(out, string(line)...)
		out = append(out, '\n')
	}
	return
}

func (ed *editor) evalForm() {
	bottom := ed.v0 + len(ed.lines)
	h := int(atomic.LoadInt32(&ed.height))
	if h <= bottom {
		diff := bottom + 1 - h
		ed.scroll(diff)
		ed.v0 -= diff
	}
	ed.line = len(ed.lines) - 1
	ed.pos = len(ed.lines[ed.line])
}

func (ed *editor) drawLine(n int) {
	if len(ed.lines) <= n {
		ed.setCursor(ed.v0+n, 1)
		ed.clearLine()
		return
	}
	max := int(atomic.LoadInt32(&ed.width)) - ed.foff
	var rline []rune
	back := ed.foff
	line := ed.lines[n]

	switch {
	case ed.line == n && 0 < ed.shift:
		// rline = append(rline, 'ᐊ', ' ')
		rline = append(rline, 'ᗕ', ' ')
		back = 2
		line = line[ed.shift:]
	case 0 < n:
		rline = append(rline, ' ', ' ')
		back = 2
	default:
		rline = append(rline, []rune(prompt)...)
	}
	wp := 0
	for _, r := range line {
		wp += RuneWidth(r)
		if wp <= max {
			rline = append(rline, r)
			continue
		}
		rline[len(rline)-1] = ' '
		rline = append(rline, 'ᗒ')
		break
	}
	ed.setCursor(ed.v0+n, ed.foff-back)
	ed.clearLine()
	_, _ = ed.out.Write([]byte(string(rline)))
}

func (ed *editor) lineWidth() (pw, lw int) {
	if 0 < len(ed.lines) {
		for i, r := range ed.lines[ed.line] {
			rw := RuneWidth(r)
			lw += rw
			if i < ed.pos {
				pw += rw
			}
		}
	}
	return
}

func (ed *editor) adjustShift(draw bool) {
	max := int(atomic.LoadInt32(&ed.width)) - ed.foff - 1
	from := 0 // from end
	line := ed.lines[ed.line]
	if ed.pos < ed.shift {
		if 0 <= ed.pos {
			ed.shift = ed.pos
		}
	} else {
		cnt := ed.pos
		for ; 0 < cnt && from < max; cnt-- {
			from += RuneWidth(line[cnt-1])
		}
		if ed.shift < cnt {
			ed.shift = cnt
		}
	}
	if draw {
		ed.drawLine(ed.line)
	}
}

func (ed *editor) runesTo(w int) (cnt int) {
	if 0 < len(ed.lines) {
		var r rune
		for cnt, r = range ed.lines[ed.line] {
			w -= RuneWidth(r)
			if w <= 0 {
				cnt++
				break
			}
		}
	}
	return
}

func (ed *editor) addRune(r rune) {
	if len(ed.lines) == 0 {
		ed.lines = [][]rune{}
	}
	line := ed.lines[ed.line]
	if ed.pos == len(line) {
		line = append(line, r)
	} else {
		end := line[ed.pos:]
		line = append(line[:ed.pos], append([]rune{r}, end...)...)
	}
	ed.lines[ed.line] = line
	ed.pos++
	max := int(atomic.LoadInt32(&ed.width)) - ed.foff
	if pw, lw := ed.lineWidth(); max < lw {
		if max <= pw {
			ed.shift = ed.runesTo(pw - max)
		}
		ed.drawLine(ed.line)
	} else {
		ed.setCursor(ed.v0+ed.line, ed.foff+ed.pos-1)
		_, _ = ed.out.Write([]byte(string(line[ed.pos-1:])))
	}
	ed.setCursorCurrent()
}

func (ed *editor) getSize() (w, h int) {
	if hs, _ := sizer.(hasSize); hs != nil {
		w, h = hs.getSize()
	} else {
		w, h = term.GetSize(ed.fd)
	}
	atomic.StoreInt32(&ed.height, int32(h))
	atomic.StoreInt32(&ed.width, int32(w))
	return
}

// ANSI sequences
func (ed *editor) getCursor() (v, h int) {
	if _, err := ed.out.Write([]byte("\x1b[6n")); err != nil {
		return ed.v0, ed.foff
	}
	key := <-ed.seqChan
	var mode int
done:
	for i, b := range key.buf {
		if key.cnt <= i {
			break
		}
		switch b {
		case '\n', '\r':
			// ignore
		case '\x1b':
			mode++
		case '[':
			if mode == 1 {
				mode++
			}
		case ';':
			if mode == 2 {
				mode++
			}
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if mode == 2 {
				v = v*10 + int(b-'0')
			} else {
				h = h*10 + int(b-'0')
			}
		case 'R':
			break done
		default:
			v, h = ed.v0, ed.foff
			break done
		}
	}
	return
}

func (ed *editor) setCursor(v, h int) {
	if v < 0 {
		v = 0
	}
	if h < 0 {
		h = 0
	}
	_, _ = fmt.Fprintf(ed.out, "\x1b[%d;%dH", v, h)
}

func (ed *editor) setCursorPos(line, pos int) {
	cpos := ed.foff
	if line < len(ed.lines) {
		rline := ed.lines[line]
		if len(rline) < pos {
			pos = len(rline)
		}
		for i := 0; i < pos; i++ {
			cpos += RuneWidth(rline[i])
		}
	}
	cpos -= ed.shift
	ed.setCursor(ed.v0+line, cpos)
}

func (ed *editor) setCursorCurrent() {
	ed.setCursorPos(ed.line, ed.pos)
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

func (ed *editor) clearDown() {
	_, _ = ed.out.Write([]byte(("\x1b[J")))
}

func (ed *editor) home() {
	_, _ = ed.out.Write([]byte(("\x1b[H")))
}

func (ed *editor) scroll(n int) {
	if 0 < n {
		_, _ = fmt.Fprintf(ed.out, "\x1b[%dS", n)
	} else if n < 0 {
		_, _ = fmt.Fprintf(ed.out, "\x1b[%dT", n)
	}
}

func (ed *editor) box(top, left, h, w, barTop, barBottom int) {
	// Save cursor position and then turn the cursor invisible if supported.
	_, _ = ed.out.Write([]byte{'\x1b', '7', '\x1b', '[', '?', '2', '5', 'l'})

	ed.setCursor(top, left)
	line := utf8.AppendRune(nil, '┌')
	line = append(line, bytes.Repeat(utf8.AppendRune(nil, '─'), w-2)...)
	line = utf8.AppendRune(line, '┒')
	_, _ = ed.out.Write(line)

	leftEdge := utf8.AppendRune(nil, '│')
	rightEdge := utf8.AppendRune(nil, '┃')
	// barEdge := utf8.AppendRune(nil, '▊')
	barEdge := utf8.AppendRune(nil, '█')

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
	_, _ = ed.out.Write([]byte{'\x1b', '8', '\x1b', '[', '?', '2', '5', 'h'})
}

func (ed *editor) displayMessage(msg []byte) {
	bottom := ed.v0 + len(ed.lines)
	cnt := 1
	ed.dirty.cnt = cnt + 1
	h := int(atomic.LoadInt32(&ed.height))
	if h <= bottom+cnt {
		diff := bottom + cnt - h
		ed.scroll(diff)
		ed.v0 -= diff
	}
	ed.setCursor(ed.v0+len(ed.lines), 1)
	_, _ = ed.out.Write([]byte{'\x1b', '[', '7', 'm'})
	if 1 < ed.foff {
		_, _ = ed.out.Write(bytes.Repeat([]byte{' '}, ed.foff-1))
	}
	pad := int(atomic.LoadInt32(&ed.width)) - len(msg) - ed.foff
	if 0 < pad {
		msg = append(msg, bytes.Repeat([]byte{' '}, pad)...)
	}
	msg = append(msg, "\x1b[m"...)
	_, _ = ed.out.Write(msg)
	ed.setCursorCurrent()
}

func (ed *editor) displayHelp(doc []byte, w, h int) {
	bottom := ed.v0 + len(ed.lines)
	box := scope.Get("*repl-help-box*") != nil
	indent := 0
	pad := 0
	if box {
		w -= 4
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
	ed.setCursorCurrent()
	ed.mode = topMode
}

func (ed *editor) displayCompletions() {
	words := ed.completer.words[ed.completer.lo : ed.completer.hi+1]
	w := int(atomic.LoadInt32(&ed.width))
	leftPad := []byte{' ', ' ', ' '}
	colWidth := 0
	for _, word := range words {
		if colWidth < len(word) {
			colWidth = len(word)
		}
	}
	var buf []byte
	colWidth += 2
	colCnt := (w - 6) / colWidth
	ed.completer.colCnt = colCnt
	klines := len(words)/colCnt + 1
	for i := 0; i < klines; i++ {
		if 0 < i {
			buf = append(buf, leftPad...)
		}
		for j := 0; j < colCnt; j++ {
			index := i*colCnt + j
			if len(words) <= index {
				continue
			}
			word := words[index]
			if ed.completer.index == index {
				buf = append(buf, '\x1b', '[', '7', 'm')
				buf = append(buf, word...)
				buf = append(buf, '\x1b', '[', 'm')
			} else {
				buf = append(buf, word...)
			}
			buf = append(buf, bytes.Repeat([]byte{' '}, colWidth-len(word))...)
		}
		buf = append(buf, '\n')
	}
	ed.displayHelp(buf, w, int(atomic.LoadInt32(&ed.height)))
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
		cnt := ed.dirty.cnt - 3
		barTop := cnt * ed.dirty.top / len(ed.dirty.lines)
		bar := cnt * cnt / len(ed.dirty.lines)
		ed.box(ed.v0+len(ed.lines), 2, ed.dirty.cnt-2, int(atomic.LoadInt32(&ed.width))-2, barTop, barTop+bar)
	}
	ed.setCursorCurrent()
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

func (ed *editor) isFirstChar(p *point) bool {
	pos := p.pos - 1
	for i := p.line; 0 <= i; i-- {
		line := ed.lines[i]
		for ; 0 <= pos; pos-- {
			if line[pos] != ' ' {
				return false
			}
		}
		if 0 < i {
			pos = len(ed.lines[i-1]) - 1
		}
	}
	return true
}

func (ed *editor) findWordEnd() (ln int, pos int) {
	pos = ed.pos + 1
	ln = ed.line
	first := true
lineLoop:
	for ; ln < len(ed.lines); ln++ {
		line := ed.lines[ln]
		if first {
			first = false
		} else {
			pos = 0
		}
		for ; pos < len(line); pos++ {
			r := line[pos]
			if sepMapLen <= r || sepMap[r] != 'x' {
				break lineLoop
			}
		}
	}
	if len(ed.lines) <= ln {
		ln = len(ed.lines) - 1
		pos = len(ed.lines[ln])
	} else {
		line := ed.lines[ln]
		for ; pos < len(line); pos++ {
			r := line[pos]
			if r < sepMapLen && sepMap[r] == 'x' {
				break
			}
		}
	}
	return
}

func (ed *editor) findWordStart() (ln int, pos int) {
	pos = ed.pos - 1
	ln = ed.line
	// Skip until a word character (non-separator) is encountered.
	first := true
lineLoop:
	for ; 0 <= ln; ln-- {
		line := ed.lines[ln]
		if first {
			first = false
		} else {
			pos = len(line) - 1
		}
		for ; 0 <= pos; pos-- {
			r := line[pos]
			if sepMapLen <= r || sepMap[r] != 'x' {
				break lineLoop
			}
		}
	}
	if ln < 0 {
		ln = 0
		pos = -1
	} else {
		line := ed.lines[ln]
		for ; 0 <= pos; pos-- {
			r := line[pos]
			if r < sepMapLen && sepMap[r] == 'x' {
				break
			}
		}
	}
	pos++
	return
}

func (ed *editor) deleteRange(fromLine, fromPos, toLine, toPos int) {
	if fromLine == toLine {
		line := ed.lines[toLine]
		ed.lines[toLine] = append(line[:fromPos], line[toPos:]...)
		return
	}
	ed.lines[fromLine] = append(ed.lines[fromLine][:fromPos], ed.lines[toLine][toPos:]...)
	if toLine < len(ed.lines) {
		ed.lines = append(ed.lines[:fromLine+1], ed.lines[toLine+1:]...)
	} else {
		ed.lines = ed.lines[:fromLine+1]
	}
}

func (ed *editor) addToHistory() {
	ed.hist.Add(ed.lines)
}

func (ed *editor) setForm(form Form) {
	for i := ed.line; 0 < i; i-- { // zero is cleared when ed.display is called
		ed.setCursor(ed.v0+i, ed.foff)
		ed.clearToEnd()
	}
	ed.lines = form
	ed.line = len(ed.lines) - 1
	ed.pos = len(ed.lines[ed.line])
	ed.shift = 0
	ed.adjustShift(false)
	bottom := ed.v0 + len(ed.lines)
	h := int(atomic.LoadInt32(&ed.height))
	if h <= bottom {
		diff := bottom + 1 - h
		ed.scroll(diff)
		ed.v0 -= diff
	}
	ed.display()
}

func (ed *editor) keepForm() {
	ed.lines = ed.lines.Dup()
	ed.line = len(ed.lines) - 1
}

func getHistoryLimit() slip.Object {
	if ed, ok := replReader.(*editor); ok {
		return slip.Fixnum(ed.hist.limit)
	}
	return nil
}

func setHistoryLimit(value slip.Object) {
	ed, ok := replReader.(*editor)
	if ok {
		var num slip.Fixnum
		if num, ok = value.(slip.Fixnum); !ok {
			slip.PanicType("*repl-history-limit*", value, "fixnum")
		}
		if num < 0 {
			num = 0
		}
		ed.hist.SetLimit(int(num))
	}
}
