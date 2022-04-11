// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"fmt"
	"math/big"
	"strconv"
	"time"
)

const (
	skipByte    = 'a'
	skipNewline = 'b'
	commentByte = ';'
	commentDone = 'c'

	openParen  = '('
	closeParen = ')'

	digitByte     = 'd'
	numberDone    = 'N'
	revertToToken = 'R'

	signByte   = '+'
	signNumber = 'n'
	signToken  = 'w'

	tokenStart = 't'
	tokenDone  = 'T'
	/*
		singleQuote = 'q'
		doubleQuote = 'Q'
		stringByte  = 's'
		stringDone  = 'S'

		escByte     = 'e'
		escOk       = 'k'
		escUnicode4 = 'u'
		escUnicode8 = 'U'
		runeDigit   = '1'
		runeHexA    = 'H'
		runeHexa    = 'h'

		charByte  = '#'
		charSlash = '/'
		charDone  = 'C'
	*/

	//   0123456789abcdef0123456789abcdef
	valueMode = "" +
		".........ab..a.................." + // 0x00
		"a.Q#..tq()t+.+ttddddddddddt;ttt." + // 0x20
		"ttttttttttttttttttttttttttt...tt" + // 0x40
		".tttttttttttttttttttttttttt...t." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................v" //  0xe0

	//   0123456789abcdef0123456789abcdef
	commentMode = "" +
		".........ac..a.................." + // 0x00
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x20
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x40
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x60
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x80
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0xa0
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0xc0
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa;" //  0xe0

	//   0123456789abcdef0123456789abcdef
	tokenMode = "" +
		".........TT..T.................." + // 0x00
		"T.......TTaa.aaaaaaaaaaaaaa.aaa." + // 0x20
		"aaaaaaaaaaaaaaaaaaaaaaaaaaa...aa" + // 0x40
		".aaaaaaaaaaaaaaaaaaaaaaaaaa...a." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................t" //  0xe0

	//   0123456789abcdef0123456789abcdef
	numberMode = "" +
		".........NN..N.................." + // 0x00
		"N.......NNRa.aa.aaaaaaaaaaR...RR" + // 0x20
		"RRRRRaRRRRRRRRRRRRRRRRRRRRR...RR" + // 0x40
		"RRRRRaRRRRRRRRRRRRRRRRRRRRR...R." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................n" //  0xe0

	//   0123456789abcdef0123456789abcdef
	signMode = "" +
		".........TT..T.................." + // 0x00
		"T.......TTww.wwwnnnnnnnnnnw.www." + // 0x20
		"wwwwwwwwwwwwwwwwwwwwwwwwwww...ww" + // 0x40
		".wwwwwwwwwwwwwwwwwwwwwwwwww...w." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................t" //  0xe0
)

// Code is a list of S-Expressions read from LISP source code. It is a means
// of keeping loaded code together so that it can be evaluated and optimized
// for subsequent evaluations.
type Code []Object

type reader struct {
	tokenStart int
	stack      []Object
	starts     []int
	line       int
	lineStart  int
	pos        int

	code Code
}

// ReadString reads LISP source code and return a Code instance.
func ReadString(src string) (code Code) {
	return (&reader{}).read([]byte(src))
}

// Read LISP source code and return a Code instance.
func Read(src []byte) (code Code) {
	return (&reader{}).read(src)
}

func (r *reader) read(src []byte) Code {
	mode := valueMode
	var (
		b byte
		// buf []byte
	)
	for r.pos, b = range src {
	Retry:
		switch mode[b] {
		case skipNewline:
			r.line++
			r.lineStart = r.pos
		case skipByte:
			// skip
		case commentByte:
			mode = commentMode
		case commentDone:
			mode = valueMode

		case openParen:
			r.starts = append(r.starts, len(r.stack))
			r.stack = append(r.stack, nil)
		case closeParen:
			r.closeList()

		case tokenStart:
			r.tokenStart = r.pos
			mode = tokenMode
		case tokenDone:
			r.pushToken(src)
			mode = valueMode
			goto Retry

		case signByte:
			r.tokenStart = r.pos
			mode = signMode
		case signNumber:
			mode = numberMode
		case signToken:
			mode = tokenMode

		case digitByte:
			r.tokenStart = r.pos
			mode = numberMode
		case numberDone:
			r.pushNumber(src)
			mode = valueMode
			goto Retry
		case revertToToken:
			mode = tokenMode

			// TBD string

			// TBD |symbol|

			// TBD char

			// TBD #, maybe same as char
		}
	}
	r.pos++
	switch mode {
	case tokenMode:
		r.pushToken(src)
	case numberMode:
		r.pushNumber(src)
		/*
			case charMode:
				r.pushChar(src)
			case stringMode, runeMode, escMode:
				r.raise("string no terminated")
				// TBD | deliminated symbol
		*/
	}
	return r.code
}

func (r *reader) raise(format string, args ...interface{}) {
	f := make([]byte, 0, len(format)+9)
	f = append(f, format...)
	f = append(f, " at %d:%d"...)
	args = append(args, r.line, r.pos-r.lineStart)
	panic(fmt.Sprintf(string(f), args...))
}

func (r *reader) closeList() {
	if len(r.starts) == 0 {
		r.raise("unmatched close parenthesis")
	}
	start := r.starts[len(r.starts)-1]
	size := len(r.stack) - start - 1
	list := make(List, 0, size)
	for i := start + size; start < i; i-- {
		list = append(list, r.stack[i])
		r.stack[i] = nil // make sure reference to value is removed from stack
	}
	if 0 < start {
		r.stack = r.stack[:start+1]
		r.stack[start] = list
		r.starts = r.starts[:len(r.starts)-1]
	} else {
		r.stack = r.stack[:0]
		r.starts = r.starts[:0]
		r.code = append(r.code, list)
	}
}

// Converts tokens to the correct type and then pushes that value onto the
// stack.
func (r *reader) pushToken(src []byte) {
	size := r.pos - r.tokenStart
	var (
		obj   Object
		token []byte
	)
	if size == 1 && (src[r.tokenStart] == 't' || src[r.tokenStart] == 'T') {
		obj = True
		goto Push
	}
	token = src[r.tokenStart:r.pos]
	if size == 3 && bytes.EqualFold([]byte("nil"), token) {
		obj = nil
		goto Push
	}
	switch token[0] {
	case '@':
		// This is an extension to common lisp to make time easier to deal with.
		s := string(token[1:])
		for _, layout := range []string{
			time.RFC3339Nano,
			time.RFC3339,
			"2006-01-02T15:04:05",
			"2006-01-02",
		} {
			if t, err := time.ParseInLocation(layout, s, time.UTC); err == nil {
				obj = Time(t)

				goto Push
			}
		}
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-':
		// TBD consider 123. for base 10
		s := string(token)
		if i, err := strconv.ParseInt(s, 10, 64); err == nil {
			obj = Fixnum(i)
			goto Push
		}
		if f, err := strconv.ParseFloat(s, 64); err == nil {
			obj = DoubleFloat(f)
			goto Push
		}

		// TBD
		// consider ratio n/m
		// handle bignum
		// for floats consider exponent characters for size

	}
	obj = Symbol(token)
Push:
	if 0 < len(r.stack) {
		r.stack = append(r.stack, obj)
	} else {
		r.code = append(r.code, obj)
	}
}

func (r *reader) pushNumber(src []byte) {
	// Numbers the easy way.
	// TBD parse number directly from bytes for a small optimization
	token := src[r.tokenStart:r.pos]
	var (
		obj Object
		bi  *big.Int
		s   string
	)
	if token[len(token)-1] == '.' {
		token = token[:len(token)-1]
		if i, err := strconv.ParseInt(string(token), 10, 64); err == nil {
			obj = Fixnum(i)
			goto Push
		}
	}
	s = string(token)
	if i, err := strconv.ParseInt(s, 10, 64); err == nil {
		obj = Fixnum(i)
		goto Push
	}
	if f, err := strconv.ParseFloat(s, 64); err == nil {
		obj = DoubleFloat(f)
		goto Push
	}
	bi = big.NewInt(0)
	if err := bi.UnmarshalText(src[r.tokenStart:r.pos]); err == nil {
		obj = (*Bignum)(bi)
		goto Push
	}
	r.pushToken(src)
	return

Push:
	if 0 < len(r.stack) {
		r.stack = append(r.stack, obj)
	} else {
		r.code = append(r.code, obj)
	}
}

/*
func (r *reader) pushHash(src []byte) {

	fmt.Printf("*** # token: %s\n", src[r.tokenStart:r.pos])
}

func (r *reader) pushChar(src []byte) {

	fmt.Printf("*** string: %s\n", src[r.tokenStart:r.pos])
}
*/

// String returns a string representation of the instance.
func (c Code) String() string {
	var b []byte
	b = append(b, '[')
	for i, obj := range c {
		if 0 < i {
			b = append(b, ' ')
		}
		b = ObjectAppend(b, obj)
	}
	b = append(b, ']')
	if 80 < len(b) {
		// Redo with newlines instead of just spaces. Not efficient but this
		// is only expected to be used for debugging.
		b = b[:0]
		b = append(b, '[')
		for i, obj := range c {
			if 0 < i {
				b = append(b, "\n "...)
			}
			b = ObjectAppend(b, obj)
		}
		b = append(b, ']')
	}
	return string(b)
}
