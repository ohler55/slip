// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"bytes"
	"fmt"
	"math"
	"math/big"
	"strconv"

	"github.com/ohler55/slip"
	"golang.org/x/text/cases"
	"golang.org/x/text/language"
)

// - 0123456789abcdef0123456789abcdef
const (
	dirScanMap = "" +
		"..........x....................." + // 0x00
		"....xxx.xxx.x..x..........x.xxxx" + // 0x20
		"xxxxxxxx.x.....xx.xxx..xx..x.xx." + // 0x40
		".xxxxxxx.x.....xx.xxx..xx..xxxx." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................" //   0xe0
)

var (
	romanNumerals = [4][10]string{
		{"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"},
		{"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"},
		{"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"},
		{"", "M", "MM", "MMM", "", "", "", "", "", ""},
	}
	oldRomanNumerals = [4][10]string{
		{"", "I", "II", "III", "IIII", "V", "VI", "VII", "VIII", "VIIII"},
		{"", "X", "XX", "XXX", "XXXX", "L", "LX", "LXX", "LXXX", "LXXXX"},
		{"", "C", "CC", "CCC", "CCCC", "D", "DC", "DCC", "DCCC", "DCCCC"},
		{"", "M", "MM", "MMM", "", "", "", "", "", ""},
	}
	cardinalTriples = []string{
		"", // less than 1000
		"thousand",
		"million",
		"billion",
		"trillion",
		"quadrillion",
		"quantillion",
		"sextillion",
		"septillion",
		"octillion",
		"nonillion",
		"decillion",
		"undecillion",
		"duodecillion",
		"tredecillion",
		"quattuordecillion",
		"quindecillion",
		"sexdecillion",
		"septendecillion",
		"octodecillion",
		"novemdecillion",
		"vigintillion",
	}
	cardinalOne  = [10]string{"", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}
	cardinalTeen = [10]string{"ten", "eleven", "twelve", "thirteen", "fourteen",
		"fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}
	cardinalTen = [10]string{"twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"}
	ordinalOne  = [10]string{"", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth"}
	ordinalTeen = [10]string{"tenth", "eleventh", "twelfth", "thirteenth", "fourteenth",
		"fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth"}
)

type control struct {
	scope  *slip.Scope
	str    []byte
	end    int
	pos    int
	out    []byte
	args   slip.List
	argPos int
	stop   bool
}

type floatFormatter struct {
	w            int
	d            int
	e            int
	k            int
	overflowChar []byte
	padChar      []byte
	exponentChar []byte
	digits       []byte
	exp          int
	neg          bool
	notNum       bool
}

// Write appends to c.out.
func (c *control) Write(p []byte) (n int, err error) {
	c.out = append(c.out, p...)
	return len(p), nil
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
				invalidDir(c.str, c.pos-1)
			}
			colon = true
		case '@':
			if at {
				invalidDir(c.str, c.pos-1)
			}
			at = true
		case ',':
			if colon || at {
				invalidDir(c.str, c.pos-1)
			}
			prev := c.str[c.pos-2]
			if prev == '~' || prev == ',' {
				params = append(params, nil)
			}
		case '#':
			params = append(params, len(c.args)-c.argPos)
		case 'v':
			var p any
			if 0 <= c.argPos {
				p = c.args[c.argPos]
				c.argPos++
			}
			params = append(params, p)
		case '\'':
			p := c.readParam()
			params = append(params, slip.ReadCharacter(p))
		case '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			c.pos--
			p := c.readParam()
			if n, err := strconv.ParseInt(string(p), 10, 64); err == nil {
				params = append(params, int(n))
			} else {
				slip.NewPanic("invalid directive at %d of %q. %s", c.pos-1, c.str, err)
			}
		case '\n':
			c.dirNewline(colon, at, params)
			return
		case '$':
			c.dirMoney(colon, at, params)
			return
		case '%':
			c.dirPercent(colon, at, params)
			return
		case '&':
			c.dirAmp(colon, at, params)
			return
		case '(':
			c.dirCase(colon, at, params)
			return
		case '*':
			c.dirMove(colon, at, params)
			return
		case '/':
			c.dirCall(colon, at, params)
			return
		case '<':
			c.dirJustify(colon, at, params)
			return
		case '=':
			c.dirEval(colon, at, params)
			return
		case '?':
			c.dirProc(colon, at, params)
			return
		case 'A', 'a':
			c.dirA(colon, at, params)
			return
		case 'B', 'b':
			c.dirB(colon, at, params)
			return
		case 'C', 'c':
			c.dirC(colon, at, params)
			return
		case 'D', 'd':
			c.dirD(colon, at, params)
			return
		case 'E', 'e':
			c.dirE(colon, at, params)
			return
		case 'F', 'f':
			c.dirF(colon, at, params)
			return
		case 'G', 'g':
			c.dirG(colon, at, params)
			return
		case 'I', 'i':
			// Does nothing for now.
			return
		case 'O', 'o':
			c.dirO(colon, at, params)
			return
		case 'P', 'p':
			c.dirP(colon, at, params)
			return
		case 'R', 'r':
			c.dirR(colon, at, params)
			return
		case 'S', 's':
			c.dirS(colon, at, params)
			return
		case 'T', 't':
			c.dirT(colon, at, params)
			return
		case 'W', 'w':
			c.dirW(colon, at, params)
			return
		case 'X', 'x':
			c.dirX(colon, at, params)
			return
		case '[':
			c.dirCond(colon, at, params)
			return
		case '{':
			c.dirIter(colon, at, params)
			return
		case '|':
			c.dirPage(colon, at, params)
			return
		case '^':
			c.stop = true
			return
		case '~':
			c.dirTilde(colon, at, params)
			return
		default:
			invalidDir(c.str, c.pos)
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

func (c *control) dirNewline(colon, at bool, params []any) {
	switch {
	case colon && at:
		invalidDirParam(c.str, c.pos)
	case colon:
		return
	case at:
		c.out = append(c.out, '\n')
		for ; c.pos < c.end; c.pos++ {
			switch c.str[c.pos] {
			case ' ', '\t': // Tab may not be whitespace in lisp but we assume it is for now.
				// skip it
			default:
				return
			}
		}
	default:
		// skip spaces in control
		for ; c.pos < c.end; c.pos++ {
			switch c.str[c.pos] {
			case ' ', '\t':
				// skip it
			default:
				return
			}
		}
	}
}

func (c *control) dirMoney(colon, at bool, params []any) {
	d := c.getIntParam(0, params, 2, true)
	n := c.getIntParam(1, params, 1, true)
	w := c.getIntParam(2, params, 0, true)
	padchar := c.getCharParam(3, params, []byte{' '})
	var val float64
	if 0 <= c.argPos {
		arg := c.args[c.argPos]
		c.argPos++
		if r, ok := arg.(slip.Real); ok {
			val = r.RealValue()
		} else {
			slip.NewPanic("expected a real argument for directive at %d of %q", c.pos, c.str)
		}
	}
	if colon {
		if val < 0.0 {
			val = -val
			c.out = append(c.out, '-')
			w--
		} else if at {
			c.out = append(c.out, '+')
			w--
		}
	}

	buf := strconv.AppendFloat(nil, val, 'f', d, 64)
	if 1 < n {
		var cnt int
	done:
		for _, b := range buf {
			switch b {
			case '-':
				// don't count
			case '.':
				break done
			default:
				cnt++
			}
		}
		if cnt < n {
			n -= cnt
			b2 := make([]byte, len(buf)+n)
			var i int
			if buf[0] == '-' {
				b2[0] = '-'
				buf = buf[1:]
				i++
			}
			for ; 0 < n; n-- {
				b2[i] = '0'
				i++
			}
			copy(b2[i:], buf)
			buf = b2
		}
	}
	if len(buf) < w {
		for i := w - len(buf); 0 < i; i-- {
			c.out = append(c.out, padchar...)
		}
	}
	c.out = append(c.out, buf...)
}

func (c *control) dirPercent(colon, at bool, params []any) {
	n := 1
	if 0 < len(params) {
		switch tp := params[0].(type) {
		case int:
			n = tp
		case slip.Integer:
			n = int(tp.RealValue())
		default:
			invalidDirParam(c.str, c.pos)
		}
	}
	for ; 0 < n; n-- {
		c.out = append(c.out, '\n')
	}
}

func (c *control) dirAmp(colon, at bool, params []any) {
	n := 1
	if 0 < len(params) {
		switch tp := params[0].(type) {
		case int:
			n = tp
		case slip.Integer:
			n = int(tp.RealValue())
		default:
			invalidDirParam(c.str, c.pos)
		}
	}
	if 0 < len(c.out) && c.out[len(c.out)-1] == '\n' {
		n--
	}
	for ; 0 < n; n-- {
		c.out = append(c.out, '\n')
	}
}

func scanDirBlock(buf []byte, pos int, dirName string, open, close byte, colonOk bool) int {
	end := len(buf)
	var (
		colon bool
		at    bool
		tilde bool
	)
	for pos < end {
		b := buf[pos]
		pos++
		switch {
		case tilde:
			switch b {
			case ':':
				colon = true
			case '@':
				at = true
			case open:
				pos = scanDirBlock(buf, pos, dirName, open, close, colonOk) + 2
			case close:
				if at || (colon && !colonOk) {
					invalidDir(buf, pos)
				}
				if colon {
					return pos - 3
				}
				return pos - 2
			default:
				tilde = false
			}
		case b == '~':
			tilde = true
			colon = false
			at = false
		default:
			tilde = false
		}
	}
	panic(slip.NewError("%s directive not terminated at %d of %q", dirName, pos, buf))
}

func (c *control) dirCase(colon, at bool, params []any) {
	pos := scanDirBlock(c.str, c.pos, "case", '(', ')', false)
	c2 := *c
	c2.out = make([]byte, 0, pos-c.pos)
	c2.end = pos
	c2.process()

	c.pos = pos + 2 // past ~)
	c.argPos = c2.argPos

	switch {
	case colon && at:
		c.out = append(c.out, bytes.ToUpper(c2.out)...)
	case colon:
		c2.out = bytes.ToLower(c2.out)
		caser := cases.Title(language.English)
		c.out = append(c.out, caser.Bytes(c2.out)...)
	case at:
		c2.out = bytes.ToLower(c2.out)
		caser := cases.Title(language.English)
		if i := bytes.Index(c2.out, []byte{' '}); 0 < i {
			c.out = append(c.out, caser.Bytes(c2.out[:i])...)
			c.out = append(c.out, c2.out[i:]...)
		} else {
			c.out = append(c.out, caser.Bytes(c2.out)...)
		}
	default:
		c.out = append(c.out, bytes.ToLower(c2.out)...)
	}
}

func (c *control) dirMove(colon, at bool, params []any) {
	n := 1
	var changed bool
	if 0 < len(params) {
		switch tp := params[0].(type) {
		case int:
			n = tp
			changed = true
		case slip.Integer:
			n = int(tp.RealValue())
			changed = true
		default:
			invalidDirParam(c.str, c.pos)
		}
	}
	switch {
	case colon && at:
		invalidDirParam(c.str, c.pos)
	case colon:
		c.argPos -= n
	case at:
		if !changed {
			n = 0
		}
		c.argPos = n
	default:
		c.argPos += n
	}
}

func (c *control) dirCall(colon, at bool, params []any) {
	start := c.pos
	for ; c.pos < c.end; c.pos++ {
		if c.str[c.pos] == '/' {
			break
		}
	}
	if c.end <= c.pos {
		slip.NewPanic("call directive not terminated at %d of %q", c.pos, c.str)
	}
	c.pos++
	name := c.str[start : c.pos-1]
	fi := slip.MustFindFunc(string(name)) // panics if not found
	args := make(slip.List, 4)
	args[0] = &slip.OutputStream{Writer: c}
	if 0 <= c.argPos {
		args[1] = c.args[c.argPos]
		c.argPos++
	}
	if colon {
		args[2] = slip.True
	}
	if at {
		args[3] = slip.True
	}
	fi.Apply(c.scope, args, 0)
}

func (c *control) dirJustify(colon, at bool, params []any) {
	var (
		mincol  int
		colinc  int
		minpad  int
		padchar []byte
	)
	mincol = c.getIntParam(0, params, 0, true)
	colinc = c.getIntParam(1, params, 1, true)
	minpad = c.getIntParam(2, params, 0, true)
	padchar = c.getCharParam(3, params, []byte{' '})

	segments, special, pos := c.scanJustify(c.str, c.pos)

	if special != nil {
		special.process()
		// Only append to c.out if the rest don't fit in a line.
		c.argPos = special.argPos
	}
	padCnt := mincol
	if padCnt == 0 {
		padCnt = int(c.scope.Get(slip.Symbol("*print-right-margin*")).(slip.Fixnum))
	}
	for _, c2 := range segments {
		c2.process()
		padCnt -= len(c2.out)
		c.argPos = c2.argPos
		if c2.stop {
			break
		}
	}
	if padCnt-(len(segments)-1)*minpad < 0 {
		padCnt += (-padCnt / colinc) + colinc
	}
	segCnt := len(segments) - 1
	if at {
		segCnt++
	}
	var out []byte
	if colon {
		segCnt++
		cnt := padCnt / segCnt
		if cnt < minpad {
			cnt = minpad
		}
		out = append(out, bytes.Repeat(padchar, cnt)...)
		segCnt--
		padCnt -= cnt
	}
	for i, seg := range segments {
		if 0 < i {
			cnt := padCnt / segCnt
			if cnt < minpad {
				cnt = minpad
			}
			out = append(out, bytes.Repeat(padchar, cnt)...)
			segCnt--
			padCnt -= cnt
		}
		out = append(out, seg.out...)
	}
	if at {
		cnt := padCnt / segCnt
		if cnt < minpad {
			cnt = minpad
		}
		out = append(out, bytes.Repeat(padchar, cnt)...)
	}
	if special != nil {
		max := int(c.scope.Get(slip.Symbol("*print-right-margin*")).(slip.Fixnum))
		if max < len(out) {
			c.out = append(c.out, special.out...)
		}
	}
	c.out = append(c.out, out...)
	c.pos = pos + 2
}

func (c *control) scanJustify(buf []byte, pos int) ([]*control, *control, int) {
	end := len(buf)
	var (
		segments []*control
		special  *control
		colon    bool
		at       bool
		tilde    bool
	)
	start := pos
	for pos < end {
		b := buf[pos]
		pos++
		switch {
		case tilde:
			switch b {
			case ':':
				colon = true
			case '@':
				at = true
			case ';':
				str := string(buf[start : pos-2])
				c2 := control{
					scope:  c.scope,
					str:    []byte(str),
					end:    len(str),
					args:   c.args,
					argPos: c.argPos, // reset before processing
				}
				if colon {
					special = &c2
				} else {
					segments = append(segments, &c2)
				}
				start = pos
				tilde = false
			case '<':
				_, _, pos = c.scanJustify(buf, pos)
				pos += 2
				tilde = false
			case '>':
				if at || colon {
					invalidDir(buf, pos)
				}
				str := string(buf[start : pos-2])
				c2 := control{
					scope:  c.scope,
					str:    []byte(str),
					end:    len(str),
					args:   c.args,
					argPos: c.argPos, // reset before processing
				}
				segments = append(segments, &c2)
				return segments, special, pos - 2
			case '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',':
				// remain in tilde
			case '\'':
				// Read character and stay in tilde.
				for ; pos < end; pos++ {
					if dirScanMap[buf[pos]] == 'x' {
						break
					}
				}
			default:
				tilde = false
			}
		case b == '~':
			tilde = true
			colon = false
			at = false
		default:
			tilde = false
		}
	}
	panic(slip.NewError("justification directive not terminated at %d of %q", pos, buf))
}

func (c *control) dirEval(colon, at bool, params []any) {
	start := c.pos
	end := c.end - 1
	for ; c.pos < end; c.pos++ {
		if c.str[c.pos] == '~' && c.str[c.pos+1] == '=' {
			break
		}
	}
	if end <= c.pos {
		slip.NewPanic("eval directive not terminated at %d of %q", c.pos, c.str)
	}
	val := slip.Read(c.str[start:c.pos]).Eval(c.scope, nil)
	if s, ok := val.(slip.String); ok {
		c.out = append(c.out, s...)
	} else {
		p := *slip.DefaultPrinter()
		p.Escape = false
		p.Readably = false
		c.out = p.Append(c.out, val, 0)
	}
	c.pos += 2
}

func (c *control) dirProc(colon, at bool, params []any) {
	var ctrl []byte
	if c.argPos < len(c.args) {
		ss, ok := c.args[c.argPos].(slip.String)
		if !ok {
			slip.NewPanic("recursive processing directive expected a control string at %d of %q", c.pos, c.str)
		}
		ctrl = []byte(ss)
		c.argPos++
	}
	c2 := control{
		scope: c.scope,
		str:   ctrl,
		end:   len(ctrl),
	}
	if at {
		c2.args = c.args
		c2.argPos = c.argPos
	} else {
		var args slip.List
		if c.argPos < len(c.args) {
			var ok bool
			if args, ok = c.args[c.argPos].(slip.List); !ok {
				slip.NewPanic("recursive processing directive expected an argument list at %d of %q", c.pos, c.str)
			}
		}
		c2.args = args
		c2.argPos = 0
	}
	c2.process()
	if at {
		c.argPos = c2.argPos
	} else {
		c.argPos++
	}
	c.out = append(c.out, c2.out...)
}

func (c *control) dirA(colon, at bool, params []any) {
	if !colon && !at && len(params) == 0 { // bare ~A, the most common case
		var arg slip.Object
		if 0 <= c.argPos {
			arg = c.args[c.argPos]
			c.argPos++
		}
		switch ta := arg.(type) {
		case slip.String:
			c.out = append(c.out, ta...)
		case slip.Condition:
			c.out = append(c.out, ta.Error()...)
		default:
			p := *slip.DefaultPrinter()
			p.Escape = false
			p.Readably = false
			c.out = p.Append(c.out, arg, 0)
		}
		return
	}
	p := *slip.DefaultPrinter()
	p.Escape = false
	p.Readably = false
	c.dirAS(colon, at, params, &p)
}

func (c *control) dirB(colon, at bool, params []any) {
	c.dirInt(colon, at, params, 2)
}

func (c *control) dirC(colon, at bool, params []any) {
	var (
		arg slip.Character
		ok  bool
	)
	if 0 <= c.argPos {
		arg, ok = c.args[c.argPos].(slip.Character)
		c.argPos++
	}
	if !ok {
		slip.NewPanic("character directive expected a character argument at %d of %q", c.pos, c.str)
	}
	switch {
	case colon && at:
		// Should also pront out the keyboard modified need to type the
		// character but since we don't know anything about the keyboard that
		// par is left off and the colon behavior is used.
		c.out = append(c.out, arg.Append(nil)[2:]...)
	case colon:
		c.out = append(c.out, arg.Append(nil)[2:]...)
	case at:
		c.out = arg.Append(c.out)
	default:
		c.out = append(c.out, string([]rune{rune(arg)})...)
	}
}

func (c *control) dirD(colon, at bool, params []any) {
	c.dirInt(colon, at, params, 10)
}

func (c *control) dirInt(colon, at bool, params []any, base int) {
	var (
		arg slip.Object
		out []byte
		neg bool
	)
	if 0 <= c.argPos {
		arg = c.args[c.argPos]
		c.argPos++
	}
	mincol := 0
	padchar := []byte{' '}
	commachar := []byte{','}
	commaint := 3
	mincol = c.getIntParam(0, params, mincol, true)
	padchar = c.getCharParam(1, params, padchar)
	commachar = c.getCharParam(2, params, commachar)
	commaint = c.getIntParam(3, params, commaint, true)
	if commaint < 1 {
		invalidDir(c.str, c.pos)
	}
	switch ta := arg.(type) {
	case slip.Fixnum:
		out = strconv.AppendInt(nil, int64(ta), base)
		neg = ta < 0
	case *slip.Bignum:
		out = ((*big.Int)(ta)).Append(nil, base)
		neg = ((*big.Int)(ta)).Sign() < 0
	default:
		neg = true // stops @ addition of a +
		colon = false
		p := *slip.DefaultPrinter()
		p.Escape = true
		p.Readably = true
		p.Base = 10
		out = p.Append(nil, ta, 0)
	}
	if at && !neg {
		out = append([]byte{'+'}, out...)
	}
	if colon {
		dlen := len(out) - 1
		if out[0] == '-' || out[0] == '+' {
			dlen--
		}
		i := len(out) - (dlen / commaint * commaint)
		expanded := make([]byte, 0, len(out)+len(out)/commaint*len(commachar)) // might have extra bytes
		prev := 0
		for ; i < len(out); i += commaint {
			expanded = append(expanded, out[prev:i]...)
			expanded = append(expanded, commachar...)
			prev = i
		}
		expanded = append(expanded, out[prev:]...)
		out = expanded
	}
	if len(out) < mincol {
		mincol -= len(out)
		for ; 0 < mincol; mincol-- {
			c.out = append(c.out, padchar...)
		}
	}
	c.out = append(c.out, out...)
}

func (c *control) getEFGarg(ff *floatFormatter) {
	var arg slip.Object
	if 0 <= c.argPos {
		arg = c.args[c.argPos]
		c.argPos++
	}
	// golang big.Float fails to preserve digits when printing. The last few
	// become noise even with a very high precision so no attempt is made to
	// support long-float other that as a double-float.
	switch ta := arg.(type) {
	case slip.Fixnum:
		if ff.neg = ta < 0; ff.neg {
			ta = -ta
		}
		ff.digits = fmt.Appendf(nil, "%d", ta)
	case *slip.Bignum:
		num := (*big.Int)(ta)
		ff.neg = num.Sign() < 0
		num = num.Abs(num)
		ff.digits = num.Append(nil, 10)
	case slip.Real:
		num := ta.RealValue()
		if ff.neg = num < 0.0; ff.neg {
			num = -num
		}
		ff.exp = int(math.Floor(math.Log10(num)))
		ff.digits = strconv.AppendFloat(nil, num, 'e', -1, 64)
		ff.digits = ff.digits[:bytes.IndexByte(ff.digits, 'e')]
		copy(ff.digits[1:], ff.digits[2:])
		ff.digits = ff.digits[:len(ff.digits)-1]
		ff.exp -= len(ff.digits) - 1
	default:
		p := *slip.DefaultPrinter()
		p.Escape = false
		p.Readably = false
		ff.digits = p.Append(nil, ta, 0)
		ff.notNum = true
	}
}

func roundBytes(digits []byte, max int) []byte {
	diff := len(digits) - max
	if 0 < diff {
	round:
		// Round the digits.
		if digits[max] < '5' {
			digits = digits[:max]
		} else {
			digits = digits[:max]
			carry := true
			for i := len(digits) - 1; 0 <= i && carry; i-- {
				digit := digits[i]
				digit++
				if '9' < digit {
					carry = true
					digits[i] = '0'
				} else {
					digits[i] = digit
					carry = false
				}
			}
			if carry {
				digits = append([]byte{'1'}, digits...)
				goto round
			}
		}
	}
	return digits
}

func (c *control) appendEFG(ff *floatFormatter, out []byte) {
	if 0 < ff.w {
		if len(out) < ff.w {
			c.out = append(c.out, bytes.Repeat(ff.padChar, ff.w-len(out))...)
		} else if ff.w < len(out) && 0 < len(ff.overflowChar) {
			c.out = append(c.out, bytes.Repeat(ff.overflowChar, ff.w)...)
			return
		}
	}
	c.out = append(c.out, out...)
}

func (c *control) dirE(colon, at bool, params []any) {
	c.dirEappend(at, c.dirEsetup(params))
}

func (c *control) dirEsetup(params []any) *floatFormatter {
	var ff floatFormatter

	ff.w = c.getIntParam(0, params, 0, true)
	ff.d = c.getIntParam(1, params, 0, true)
	ff.e = c.getIntParam(2, params, 0, true)
	ff.k = c.getIntParam(3, params, 1, false)
	ff.overflowChar = c.getCharParam(4, params, nil)
	ff.padChar = c.getCharParam(5, params, []byte{' '})
	ff.exponentChar = c.getCharParam(6, params, []byte{'e'})
	if 0 < ff.d {
		if 0 < ff.k {
			if ff.d+2 <= ff.k {
				slip.NewPanic("a positive k value for a floating-point directive must be less than d+2")
			}
		} else if ff.k < 0 {
			if ff.k <= -ff.d {
				slip.NewPanic("a negative k value for a floating-point directive must be greater than -d")
			}
		}
	}
	c.getEFGarg(&ff)

	return &ff
}

func (c *control) dirEappend(at bool, ff *floatFormatter) {
	if 0 < ff.d && !ff.notNum {
		max := ff.d
		if ff.k < 0 {
			max += ff.k
		} else if 0 < ff.k {
			max++
		}
		diff := len(ff.digits) - max
		ff.exp += diff
		if 0 < diff {
			ff.digits = roundBytes(ff.digits, max)
		} else if diff < 0 {
			ff.digits = append(ff.digits, bytes.Repeat([]byte{'0'}, -diff)...)
		}
	}
	if ff.notNum {
		c.appendEFG(ff, ff.digits)
		return
	}
	var out []byte
	if ff.neg {
		out = append(out, '-')
	} else if at {
		out = append(out, '+')
	}
	switch {
	case ff.k == 0:
		out = append(out, "0."...)
		out = append(out, ff.digits...)
	case 0 < ff.k:
		if ff.k < len(ff.digits) {
			out = append(out, ff.digits[:ff.k]...)
			out = append(out, '.')
			out = append(out, ff.digits[ff.k:]...)
		} else {
			out = append(out, ff.digits...)
			out = append(out, bytes.Repeat([]byte{'0'}, ff.k-len(ff.digits))...)
			out = append(out, ".0"...)
		}
	case ff.k < 0:
		out = append(out, "0."...)
		out = append(out, bytes.Repeat([]byte{'0'}, -ff.k)...)
		out = append(out, ff.digits...)
	}
	out = append(out, ff.exponentChar...)
	e := int64(ff.exp + len(ff.digits) - ff.k)
	out = fmt.Appendf(out, "%+0[1]*d", ff.e+1, e)

	c.appendEFG(ff, out)
}

func (c *control) dirF(colon, at bool, params []any) {
	c.dirFappend(at, c.dirFsetup(params))
}

func (c *control) dirFsetup(params []any) *floatFormatter {
	var ff floatFormatter

	ff.w = c.getIntParam(0, params, 0, true)
	ff.d = c.getIntParam(1, params, 0, true)
	ff.k = c.getIntParam(2, params, 0, false)
	ff.overflowChar = c.getCharParam(3, params, nil)
	ff.padChar = c.getCharParam(4, params, []byte{' '})
	c.getEFGarg(&ff)

	return &ff
}

func (c *control) dirFappend(at bool, ff *floatFormatter) {
	if ff.notNum {
		c.appendEFG(ff, ff.digits)
		return
	}
	var out []byte
	if ff.neg {
		out = append(out, '-')
	} else if at {
		out = append(out, '+')
	}
	e := ff.exp + ff.k
	if 0 < ff.d && ff.d < -e {
		diff := -e - ff.d
		ff.exp += diff
		e += diff
		ff.digits = roundBytes(ff.digits, len(ff.digits)-diff)
	}
	switch {
	case 0 <= e:
		out = append(out, ff.digits...)
		if 0 < e {
			out = append(out, bytes.Repeat([]byte{'0'}, e)...)
		}
		out = append(out, '.')
		if 1 < ff.d {
			out = append(out, bytes.Repeat([]byte{'0'}, ff.d)...)
		} else {
			out = append(out, '0')
		}
	case -e < len(ff.digits):
		cnt := len(ff.digits) + e
		out = append(out, ff.digits[:cnt]...)
		out = append(out, '.')
		out = append(out, ff.digits[cnt:]...)
		if -e < ff.d {
			out = append(out, bytes.Repeat([]byte{'0'}, ff.d+e)...)
		}
	default: // len(ff.digits) <= -e
		out = append(out, "0."...)
		cnt := -e - len(ff.digits)
		out = append(out, bytes.Repeat([]byte{'0'}, cnt)...)
		out = append(out, ff.digits...)
		if cnt < ff.d {
			out = append(out, bytes.Repeat([]byte{'0'}, ff.d+e)...)
		}
	}
	c.appendEFG(ff, out)
}

func (c *control) dirG(colon, at bool, params []any) {
	ff := c.dirEsetup(params)
	var cnt int
	if 0 <= ff.exp {
		cnt = len(ff.digits) + ff.exp + 2
	} else {
		cnt = -ff.exp + 2
	}
	if cnt <= ff.w || (-7 <= cnt && cnt <= 7) {
		if len(params) < 4 || params[3] == nil {
			ff.k = 0
		}
		c.dirFappend(at, ff)
	} else {
		c.dirEappend(at, ff)
	}
}

func (c *control) dirO(colon, at bool, params []any) {
	c.dirInt(colon, at, params, 8)
}

func (c *control) dirP(colon, at bool, params []any) {
	if colon {
		c.argPos--
	}
	if c.argPos < 0 || len(c.args) <= c.argPos {
		slip.NewPanic("missing argument for Plural directive at %d of %q", c.pos, c.str)
	}
	arg := c.args[c.argPos]
	c.argPos++
	n, ok := arg.(slip.Fixnum)
	switch {
	case ok && n == 1:
		if at {
			c.out = append(c.out, 'y')
		}
	case at:
		c.out = append(c.out, 'i', 'e', 's')
	default:
		c.out = append(c.out, 's')
	}
}

func (c *control) dirR(colon, at bool, params []any) {
	if len(c.args) <= c.argPos {
		slip.NewPanic("missing argument for Radix directive at %d of %q", c.pos, c.str)
	}
	var (
		digits []byte
		words  []string
		sep    string
	)
	arg := c.args[c.argPos]
	c.argPos++
	switch ta := arg.(type) {
	case slip.Fixnum:
		digits = strconv.AppendInt(nil, int64(ta), 10)
	case *slip.Bignum:
		digits = (*big.Int)(ta).Append(nil, 10)
	default:
		slip.PanicType("argument to radix directive", ta, "fixnum", "bignum")
	}
	if at {
		if digits[0] == '-' {
			slip.NewPanic("number too small to print using the Radix directive at %d of %q", c.pos, c.str)
		}
		if 4 < len(digits) || (3 < len(digits) && '3' < digits[0]) {
			slip.NewPanic("number too large to print using the Radix directive at %d of %q", c.pos, c.str)
		}
		// prints arg as a Roman numeral: IV.
		table := romanNumerals
		if colon {
			// prints arg as an old Roman numeral: IIII.
			table = oldRomanNumerals
		}
		r := 0
		for i := len(digits) - 1; 0 <= i; i-- {
			words = append(words, table[r][digits[i]-'0'])
			r++
		}
	} else {
		// prints arg as a cardinal English number: four.
		neg := false
		if digits[0] == '-' {
			neg = true
			digits = digits[1:]
		}
		if len(digits) == 1 && digits[0] == '0' {
			if colon {
				c.out = append(c.out, "zeroth"...)
			} else {
				c.out = append(c.out, "zero"...)
			}
			return
		}
		one := cardinalOne
		teen := cardinalTeen
		if colon {
			// prints arg as an ordinal English number: fourth.
			one = ordinalOne
			teen = ordinalTeen
		}
		i := len(digits) - 1
		for _, trip := range cardinalTriples {
			if 0 < len(trip) {
				words = append(words, trip)
			}
			zero := true
			d := digits[i]
			i--
			if i < 0 {
				words = append(words, one[d-'0'])
				break
			}
			d10 := digits[i]
			i--
			switch d10 {
			case '0':
				if d != '0' {
					zero = false
					words = append(words, one[d-'0'])
				}
			case '1':
				zero = false
				words = append(words, teen[d-'0'])
			default:
				zero = false
				words = append(words, one[d-'0'])
				words = append(words, cardinalTen[d10-'0'-2])
			}
			one = cardinalOne
			teen = cardinalTeen
			if 0 <= i {
				d := digits[i]
				i--
				if d != '0' {
					zero = false
					words = append(words, "hundred")
					words = append(words, one[d-'0'])
				}
			}
			if zero {
				words = words[:len(words)-1]
			}
			if i < 0 {
				break
			}
		}
		if neg {
			words = append(words, "negative")
		}
		sep = " "
	}
	for i := len(words) - 1; 0 <= i; i-- {
		c.out = append(c.out, words[i]...)
		if 0 < i {
			c.out = append(c.out, sep...)
		}
	}
}

func (c *control) dirS(colon, at bool, params []any) {
	p := *slip.DefaultPrinter()
	p.Escape = true
	p.Readably = true
	c.dirAS(colon, at, params, &p)
}

func (c *control) dirAS(colon, at bool, params []any, p *slip.Printer) {
	var (
		arg slip.Object
		out []byte
		pad []byte
	)
	if 0 <= c.argPos {
		arg = c.args[c.argPos]
		c.argPos++
	}
	switch ta := arg.(type) {
	case nil:
		if colon {
			out = append(out, "()"...)
		} else {
			out = p.Append(out, ta, 0)
		}
	case slip.String:
		if p.Escape {
			out = p.Append(out, ta, 0)
		} else {
			out = append(out, ta...)
		}
	case slip.Condition:
		if p.Readably {
			out = ta.Append(out)
		} else {
			out = append(out, ta.Error()...)
		}
	default:
		out = p.Append(out, ta, 0)
	}
	mincol := 0
	colinc := 1
	minpad := 0
	padchar := []byte{' '}
	mincol = c.getIntParam(0, params, mincol, true)
	colinc = c.getIntParam(1, params, colinc, true)
	minpad = c.getIntParam(2, params, minpad, true)
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

func (c *control) dirT(colon, at bool, params []any) {
	// A colon modifier indicates relative to a section which we can't
	// determined so for now ignore.
	colnum := 0
	colinc := 1
	colnum = c.getIntParam(0, params, colnum, true)
	colinc = c.getIntParam(1, params, colinc, true)
	var (
		target int // target offset from 'from'
		from   int // from the start of the line
		start  int // start of line
	)
	if at {
		for len(spaces) < colnum {
			c.out = append(c.out, spaces...)
			colnum -= len(spaces)
		}
		c.out = append(c.out, spaces[:colnum]...)
		start = bytes.LastIndexAny(c.out, "\n\r\f")
		if start < 0 {
			from = len(c.out)
		} else {
			start++
			from = len(c.out) - start
		}
		if from == from/colinc*colinc {
			target = from
		} else {
			target = from/colinc*colinc + colinc
		}
	} else {
		start = bytes.LastIndexAny(c.out, "\n\r\f")
		if start < 0 {
			from = len(c.out)
		} else {
			start++
			from = len(c.out) - start
		}
		target = colnum * colinc
		if target < from {
			target = from/colinc*colinc + colinc
		}
	}
	target -= from
	for len(spaces) < target {
		c.out = append(c.out, spaces...)
		target -= len(spaces)
	}
	c.out = append(c.out, spaces[:target]...)
}

func (c *control) dirW(colon, at bool, params []any) {
	var arg slip.Object
	if 0 <= c.argPos {
		arg = c.args[c.argPos]
		c.argPos++
	}
	p := *slip.DefaultPrinter()
	if colon {
		p.Pretty = true
	}
	if at {
		p.Level = math.MaxInt
		p.Length = math.MaxInt
	}
	c.out = p.Append(c.out, arg, 0)
}

func (c *control) dirX(colon, at bool, params []any) {
	c.dirInt(colon, at, params, 16)
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
			invalidDir(c.str, c.pos)
		}
	}
	for ; 0 < n; n-- {
		c.out = append(c.out, '~')
	}
}

func (c *control) dirCond(colon, at bool, params []any) {
	n := -1
	if 0 < len(params) {
		switch tp := params[0].(type) {
		case int:
			n = tp
		case slip.Integer:
			n = int(tp.RealValue())
		default:
			invalidDir(c.str, c.pos)
		}
	}
	var arg slip.Object
	if colon || at || n < 0 {
		if c.argPos < len(c.args) {
			arg = c.args[c.argPos]
			c.argPos++
		}
	}
	strs, def, pos := scanCond(c.str, c.pos)
	switch {
	case colon && at:
		invalidDir(c.str, c.pos)
	case colon:
		if len(strs) != 2 || 0 < len(def) {
			slip.NewPanic("invalid form for conditional directive with : modifier at %d of %q", c.pos, c.str)
		}
		if arg == nil {
			c.subProcess(strs[0])
		} else {
			c.subProcess(strs[1])
		}
	case at:
		if len(strs) != 1 || 0 < len(def) {
			slip.NewPanic("invalid form for conditional directive with @ modifier at %d of %q", c.pos, c.str)
		}
		if arg != nil {
			c.argPos--
			c.subProcess(strs[0])
		}
	default:
		if n < 0 {
			if no, ok := arg.(slip.Fixnum); ok {
				n = int(no)
			} else {
				slip.PanicType("conditional directive argument", arg, "fixnum")
			}
		}
		if 0 <= n && n < len(strs) {
			c.subProcess(strs[n])
		} else if 0 < len(def) {
			c.subProcess(def)
		}
	}
	c.pos = pos + 2
}

func scanCond(buf []byte, pos int) ([]string, string, int) {
	var strs []string
	end := len(buf)
	var (
		colon   bool
		at      bool
		tilde   bool
		defNext bool
	)
	start := pos
	for pos < end {
		b := buf[pos]
		pos++
		switch {
		case tilde:
			switch b {
			case ':':
				colon = true
			case '@':
				at = true
			case ';':
				strs = append(strs, string(buf[start:pos-2]))
				start = pos
				if colon {
					defNext = true
				}
			case '[':
				// This ends up with a double scan, maybe fine for the rare
				// case where it occurs.
				_, _, pos = scanCond(buf, pos)
				pos += 2
			case ']':
				if at || colon {
					invalidDir(buf, pos)
				}
				var def string
				if defNext {
					def = string(buf[start : pos-2])
				} else {
					strs = append(strs, string(buf[start:pos-2]))
				}
				return strs, def, pos - 2
			default:
				tilde = false
			}
		case b == '~':
			tilde = true
			colon = false
			at = false
		default:
			tilde = false
		}
	}
	panic(slip.NewError("conditional directive not terminated at %d of %q", pos, buf))
}

func (c *control) subProcess(str string) {
	c2 := control{
		scope:  c.scope,
		str:    []byte(str),
		end:    len(str),
		args:   c.args,
		argPos: c.argPos,
	}
	c2.process()
	c.out = append(c.out, c2.out...)
	c.argPos = c2.argPos
}

func (c *control) dirIter(colon, at bool, params []any) {
	pos := scanDirBlock(c.str, c.pos, "iteration", '{', '}', true)
	start := c.pos
	c2 := *c
	c2.out = make([]byte, 0, pos-start)
	c2.end = pos
	var atLeastOnce bool
	c.pos = pos + 2
	// If terminated by ~:}...
	if c.pos < len(c.str) && c.str[c.pos] == '}' {
		c.pos++
		atLeastOnce = true
	}
	n := math.MaxInt
	n = c.getIntParam(0, params, n, true)
	switch {
	case colon && at:
		// The iteration consumes format arguments that must be lists.
		for ; 0 < n; n-- {
			if (len(c.args) <= c.argPos && !atLeastOnce) || c2.stop {
				break
			}
			c2.args = slip.List{}
			if c.argPos < len(c.args) {
				c2.args = objAsList(c.args[c.argPos], "iteration directive argument")
				c.argPos++
			}
			c2.argPos = 0
			c2.pos = start
			c2.process()
			c.out = append(c.out, c2.out...)
			c2.out = c2.out[:0]
			atLeastOnce = false
		}
	case colon:
		// The iterator argument must be a list of lists with the each list
		// element being consumed by one iteration.
		var argList slip.List
		if c.argPos < len(c.args) {
			argList = objAsList(c.args[c.argPos], "iteration directive argument")
			c.argPos++
		}
		if atLeastOnce && len(argList) == 0 {
			argList = slip.List{slip.List{}}
		}
		for _, al := range argList {
			if n <= 0 || c2.stop {
				break
			}
			n--
			c2.args = objAsList(al, "iteration directive sub-argument")
			c2.argPos = 0
			c2.pos = start
			c2.process()
			c.out = append(c.out, c2.out...)
			c2.out = c2.out[:0]
		}
	case at:
		// The iteration arguments are consumed from the format arguments.
		for ; 0 < n; n-- {
			if (len(c2.args) <= c2.argPos && !atLeastOnce) || c2.stop {
				break
			}
			c2.pos = start
			c2.process()
			c.out = append(c.out, c2.out...)
			c2.out = c2.out[:0]
			atLeastOnce = false
		}
		c.argPos = c2.argPos
	default:
		// The iterator argument must be a list that is consumed progressively
		// for each iteration.
		c2.args = nil
		if c.argPos < len(c.args) {
			c2.args = objAsList(c.args[c.argPos], "iteration directive argument")
			c.argPos++
		}
		c2.argPos = 0
		for ; 0 < n; n-- {
			if (len(c2.args) <= c2.argPos && !atLeastOnce) || c2.stop {
				break
			}
			c2.pos = start
			c2.process()
			c.out = append(c.out, c2.out...)
			c2.out = c2.out[:0]
			atLeastOnce = false
		}
	}
}

func (c *control) dirPage(colon, at bool, params []any) {
	n := 1
	if 0 < len(params) {
		switch tp := params[0].(type) {
		case int:
			n = tp
		case slip.Integer:
			n = int(tp.RealValue())
		default:
			invalidDir(c.str, c.pos)
		}
	}
	for ; 0 < n; n-- {
		c.out = append(c.out, '\f')
	}
}

func (c *control) getIntParam(pos int, params []any, defVal int, notNeg bool) int {
	if pos < len(params) {
		switch tp := params[pos].(type) {
		case nil:
			// leave as is
		case int:
			if notNeg && tp < 0 {
				slip.NewPanic("directive parameter is negative at %d of %q", c.pos, c.str)
			}
			return tp
		case slip.Integer:
			n := int(tp.RealValue())
			if notNeg && n < 0 {
				slip.NewPanic("directive parameter is negative at %d of %q", c.pos, c.str)
			}
			return n
		default:
			invalidDir(c.str, c.pos)
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
			invalidDir(c.str, c.pos)
		}
	}
	return defVal
}

func objAsList(obj slip.Object, loc string) (list slip.List) {
	switch tobj := obj.(type) {
	case nil:
		list = slip.List{}
	case slip.List:
		list = tobj
	default:
		slip.PanicType(loc, obj, "list")
	}
	return
}

func invalidDir(buf []byte, pos int) {
	slip.NewPanic("invalid directive at %d of %q", pos-1, buf)
}

func invalidDirParam(buf []byte, pos int) {
	slip.NewPanic("invalid directive parameter at %d of %q", pos-1, buf)
}
