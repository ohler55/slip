// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"runtime/debug"

	"github.com/ohler55/slip"
)

// Termock is a mock terminal for testing the editor.
type Termock struct {
	// input keys
	input  chan []byte
	output chan string
	width  int
	height int
	// cursor position
	cv int
	ch int
	// save cursor position
	csv int
	csh int
}

// NewTermock creates a new Termock with an initial size described by high and
// wide.
func NewTermock(high, wide int) *Termock {
	return &Termock{
		input:  make(chan []byte, 10),
		output: make(chan string, 100),
		width:  wide,
		height: high,
		cv:     1,
		ch:     1,
	}
}

func (tm *Termock) Read(p []byte) (n int, err error) {
	key := <-tm.input
	copy(p, key)
	n = len(key)
	return
}

var attrMap = map[int]string{
	0:   "<normal>",
	1:   "<bold>",
	2:   "<dim>",
	3:   "<italics>",
	4:   "<underline>",
	5:   "<blink>",
	6:   "<blink>",
	7:   "<inverse>",
	27:  "<not-inverse>",
	30:  "<black>",
	31:  "<red>",
	32:  "<green>",
	33:  "<yellow>",
	34:  "<blue>",
	35:  "<magenta>",
	36:  "<cyan>",
	37:  "<white>",
	40:  "<black-background>",
	41:  "<red-background>",
	42:  "<green-background>",
	43:  "<yellow-background>",
	44:  "<blue-background>",
	45:  "<magenta-background>",
	46:  "<cyan-background>",
	47:  "<white-background>",
	90:  "<gray>",
	91:  "<bright-red>",
	92:  "<bright-green>",
	93:  "<bright-yellow>",
	94:  "<bright-blue>",
	95:  "<bright-magenta>",
	96:  "<bright-cyan>",
	97:  "<bright-white>",
	100: "<gray-background>",
	101: "<bright-red-background>",
	102: "<bright-green-background>",
	103: "<bright-yellow-background>",
	104: "<bright-blue-background>",
	105: "<bright-magenta-background>",
	106: "<bright-cyan-background>",
	107: "<bright-white-background>",
}

// Write the bytes.
func (tm *Termock) Write(p []byte) (n int, err error) {
	var start int
	for i := 0; i < len(p); i++ {
		b := p[i]
		switch b {
		case '\n':
			tm.cv++
			tm.ch = 1
			if tm.height < tm.cv {
				tm.cv = tm.height // scroll up one
			}
		case '\r':
			tm.ch = 1
		case '\x1b':
			if start < i {
				tm.output <- string(p[start:i])
			}
			n0, n1, code, cnt := tm.parseAnsi(p, i)
			i += cnt
			start = i
			switch code {
			case '\x00':
				switch n0 {
				case 7: // save cursor
					tm.csv = tm.cv
					tm.csh = tm.ch
				case 8: // restore cursor
					tm.cv = tm.csv
					tm.ch = tm.csh
				}
			case 'n': // cursor location
				if n0 == 6 {
					tm.input <- fmt.Appendf(nil, "\x1b[%d;%dR", tm.cv, tm.ch)
				}
			case 'H': // set cursor location
				tm.output <- fmt.Sprintf("<set-cursor %d:%d>", n0, n1)
				tm.cv = n0
				if tm.height < tm.cv {
					tm.cv = tm.height
				}
				tm.ch = n1
				if tm.width < tm.ch {
					tm.ch = tm.width
				}
			case 'J': // clear down or whole screen
				switch n0 {
				case 0: // clean down
					tm.output <- fmt.Sprintf("<clear-down %d>", tm.cv)
				case 2: // clear screen
					tm.output <- "<clear-screen>"
				}
			case 'K': // clear
				switch n0 {
				case 0: // clear to start
					tm.output <- fmt.Sprintf("<clear-to-end %d:%d>", tm.cv, tm.ch)
				case 1: // cleat to end
					tm.output <- fmt.Sprintf("<clear-to-start %d:%d>", tm.cv, tm.ch)
				case 2: // clear line
					tm.output <- fmt.Sprintf("<clear-line %d>", tm.cv)
				}
			case 'S': // scroll up
				tm.cv -= n0
				if tm.cv < 1 {
					tm.cv = 1
				}
				tm.output <- fmt.Sprintf("<scroll-up %d>", n0)
			case 'T': // scroll down
				tm.cv += n0
				if tm.height < tm.cv {
					tm.cv = tm.height
				}
				tm.output <- fmt.Sprintf("<scroll-down %d>", n0)
			case 'l', 'h':
				// ignore cursor visibility codes (7 and 8)
			case 'm': // character attributes
				tm.output <- attrMap[n0]
				if 0 < n1 {
					tm.output <- attrMap[n1]
				}
			case 's': // save cursor
				tm.csv = tm.cv
				tm.csh = tm.ch
			case 'u': // restore cursor
				tm.cv = tm.csv
				tm.ch = tm.csh
			default:
				fmt.Printf("--- %q\n", p)
				debug.PrintStack()
				// TBD queries add no additional output
				// cursor moves and clears output <clear-line> or something similar
			}
		default:
			// keep going
			tm.ch++
		}
	}
	if start < len(p) {
		tm.output <- string(p[start:])
	}
	return len(p), nil
}

func (tm *Termock) Input(keys ...string) {
	for _, k := range keys {
		tm.input <- []byte(k)
	}
}

func (tm *Termock) Output() string {
	// TBD add timeout
	return <-tm.output
}

// Assume a good code so no error checking.
func (tm *Termock) parseAnsi(buf []byte, start int) (n0, n1 int, code byte, cnt int) {
	var mode int
	cnt = len(buf)
	for i := start; i < len(buf); i++ {
		b := buf[i]
		switch b {
		case '\x1b', '[', ';':
			mode++
		case '?':
			// ignore
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			switch mode {
			case 1: // esc-7 or esc-8 most likely
				n0 = n0*10 + int(b-'0')
			case 2:
				n0 = n0*10 + int(b-'0')
			case 3:
				n1 = n1*10 + int(b-'0')
			default:
				return
			}
		default:
			if mode != 1 {
				code = b
			}
			cnt = i - start + 1
			return
		}
	}
	return
}

// String returns the instance type.
func (tm *Termock) String() string {
	return "Termock"
}

// Append the object to a byte slice.
func (tm *Termock) Append(b []byte) []byte {
	return append(b, "Termock"...)
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (tm *Termock) Simplify() any {
	return nil
}

// Equal returns true if this Object and the other are equal in value.
func (tm *Termock) Equal(other slip.Object) bool {
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (tm *Termock) Hierarchy() []slip.Symbol {
	return []slip.Symbol{slip.Symbol("termock")}
}

// Eval the object.
func (tm *Termock) Eval(s *slip.Scope, depth int) slip.Object {
	return nil
}

func (tm *Termock) getSize() (w, h int) {
	return tm.width, tm.height
}
