// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"strconv"

	"github.com/ohler55/slip"
)

// - 0123456789abcdef0123456789abcdef
const dirScanMap = "" +
	"..........x....................." + // 0x00
	"....xxx.xxx.x..x..........x.xxxx" + // 0x20
	"xxxxxxxx.x.....xx.xxx..xx..x.xx." + // 0x40
	".xxxxxxx.x.....xx.xxx..xx..xxxx." + // 0x60
	"................................" + // 0x80
	"................................" + // 0xa0
	"................................" + // 0xc0
	"................................" //   0xe0

type control struct {
	str    []byte
	end    int
	pos    int
	out    []byte
	args   slip.List
	argPos int
}

func (c *control) process() {
	for c.pos < c.end {
		b := c.str[c.pos]
		if b == '~' {
			c.readDir()
			continue
		}
		c.out = append(c.out, b)
		c.pos++
	}
}

func (c *control) readDir() {
	var (
		colon  bool
		at     bool
		params []any
	)
	c.pos++ // move past ~
	for c.pos < c.end {
		b := c.str[c.pos]
		c.pos++
		switch b {
		case ':':
			if colon {
				panic(fmt.Sprintf("invalid directive at %d of %q", c.pos-1, c.str))
			}
			colon = true
		case '@':
			if at {
				panic(fmt.Sprintf("invalid directive at %d of %q", c.pos-1, c.str))
			}
			at = true
		case ',':
			if colon || at {
				panic(fmt.Sprintf("invalid directive at %d of %q", c.pos-1, c.str))
			}
			prev := c.str[c.pos-2]
			if prev == '~' || prev == ',' {
				params = append(params, nil)
			}
		case '#':
			params = append(params, c.argPos+1)
		case 'v':
			var p any
			if 0 <= c.argPos {
				p = c.args[c.argPos]
				c.argPos--
			}
			params = append(params, p)
		case '\'':
			p := c.readParam()
			params = append(params, slip.ReadCharacter(p))
		case '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			c.pos--
			p := c.readParam()
			// TBD could be made more efficient
			if n, err := strconv.ParseInt(string(p), 10, 64); err == nil {
				params = append(params, int(n))
			} else {
				panic(fmt.Sprintf("invalid directive at %d of %q. %s", c.pos-1, c.str, err))
			}
		case '\n':
		case '$':
		case '%':
		case '&':
		case '(':
		case '*':
		case '/':
		case '<':
		case '=':
		case '?':
		case 'A', 'a':
			c.dirA(colon, at, params)
			return
		case 'B', 'b':
		case 'C', 'c':
		case 'D', 'd':
		case 'E', 'e':
		case 'F', 'f':
		case 'G', 'g':
		case 'I', 'i':
		case 'O', 'o':
		case 'P', 'p':
		case 'R', 'r':
		case 'S', 's':
		case 'T', 't':
		case 'W', 'w':
		case 'X', 'x':
		case '[':
		case '{':
		case '|':
		case '~':
			c.dirTilde(colon, at, params)
			return
		default:
			panic(fmt.Sprintf("invalid directive at %d of %q", c.pos, c.str))
		}
	}
}

func (c *control) readParam() []byte {
	start := c.pos
	for ; c.pos < c.end; c.pos++ {
		if dirScanMap[c.str[c.pos]] == 'x' {
			break
		}
	}
	return c.str[start:c.pos]
}

// TBD use common func for A and S
//  take printer, colon, at, params
func (c *control) dirA(colon, at bool, params []any) {
	var arg slip.Object
	if 0 <= c.argPos {
		arg = c.args[c.argPos]
		c.argPos--
	}
	if !colon && !at && len(params) == 0 { // bare ~A, the most common case
		if s, ok := arg.(slip.String); ok {
			c.out = append(c.out, s...)
		} else {
			p := *slip.DefaultPrinter()
			p.Escape = false
			p.Readably = false
			p.Append(c.out, arg, 0)
		}
		return
	}
	var (
		out []byte
		pad []byte
	)
	p := *slip.DefaultPrinter()
	p.Escape = false
	p.Readably = false
	switch ta := arg.(type) {
	case nil:
		if colon {
			out = append(out, "()"...)
		} else {
			p.Append(out, ta, 0)
		}
	case slip.String:
		out = append(out, ta...)
	default:
		out = p.Append(out, ta, 0)
	}
	mincol := 0
	colinc := 1
	minpad := 1
	padchar := []byte{' '}
	mincol = c.getIntParam(0, params, mincol)
	colinc = c.getIntParam(1, params, colinc)
	minpad = c.getIntParam(2, params, minpad)
	padchar = c.getCharParam(3, params, padchar)
	for ; 0 < minpad; minpad-- {
		pad = append(pad, padchar...)
	}
	for len(out)+len(pad) < mincol {
		for i := colinc; 0 < i; i-- {
			pad = append(pad, padchar...)
		}
	}
	if at { // left pad
		c.out = append(c.out, pad...)
		c.out = append(c.out, out...)
	} else {
		c.out = append(c.out, out...)
		c.out = append(c.out, pad...)
	}
}

func (c *control) dirTilde(colon, at bool, params []any) {
	n := 1
	if 0 < len(params) {
		switch tp := params[0].(type) {
		case int:
			n = tp
		case slip.Integer:
			n = int(tp.RealValue())
		default:
			panic(fmt.Sprintf("invalid directive parameter at %d of %q", c.pos, c.str))
		}
	}
	for ; 0 < n; n-- {
		c.out = append(c.out, '~')
	}
}

func (c *control) getIntParam(pos int, params []any, defVal int) int {
	if pos < len(params) {
		switch tp := params[pos].(type) {
		case nil:
			// leave as is
		case int:
			return tp
		case slip.Integer:
			return int(tp.RealValue())
		default:
			panic(fmt.Sprintf("invalid directive parameter at %d of %q", c.pos, c.str))
		}
	}
	return defVal
}

func (c *control) getCharParam(pos int, params []any, defVal []byte) []byte {
	if pos < len(params) {
		switch tp := params[pos].(type) {
		case nil:
			// leave as is
		case slip.Character:
			return []byte(string([]rune{rune(tp)}))
		default:
			panic(fmt.Sprintf("invalid directive parameter at %d of %q", c.pos, c.str))
		}
	}
	return defVal
}
