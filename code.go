// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"fmt"
	"math/big"
	"regexp"
	"strconv"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

const (
	skipByte    = 'a'
	skipNewline = 'k'
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

	doubleQuote = 'Q'
	stringByte  = 's'
	stringDone  = 'S'
	escByte     = 'e'
	escOne      = 'E'
	escUnicode4 = 'u'
	escUnicode8 = 'U'
	runeDigit   = '1'
	runeHexA    = 'H'
	runeHexa    = 'h'

	pipeByte = 'P'
	pipeDone = 'p'

	sharpByte    = '#'
	charSlash    = '/'
	charDone     = 'C'
	vectorByte   = 'V'
	binaryByte   = 'b'
	octByte      = 'o'
	hexByte      = 'x'
	intDone      = 'I'
	sharpIntByte = '9'
	sharpNumByte = '8'
	sharpQuote   = 'G'
	radixByte    = 'r'
	arrayByte    = 'A'
	swallowOpen  = '{'

	singleQuote = 'q'

	//   0123456789abcdef0123456789abcdef
	valueMode = "" +
		".........ak..a.................." + // 0x00
		"a.Q#..tq()t+.+ttddddddddddt;ttt." + // 0x20
		"ttttttttttttttttttttttttttt...tt" + // 0x40
		".tttttttttttttttttttttttttt.P.t." + // 0x60
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
		"N.......NNRa.aaRaaaaaaaaaaR...RR" + // 0x20
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

	//   0123456789abcdef0123456789abcdef
	stringMode = "" +
		".........ss..s.................." + // 0x00
		"ssSsssssssssssssssssssssssssssss" + // 0x20
		"ssssssssssssssssssssssssssssesss" + // 0x40
		"ssssssssssssssssssssssssssssssss" + // 0x60
		"ssssssssssssssssssssssssssssssss" + // 0x80
		"ssssssssssssssssssssssssssssssss" + // 0xa0
		"ssssssssssssssssssssssssssssssss" + // 0xc0
		"sssssssssssssssssssssssssssssssss" //  0xe0

	//   0123456789abcdef0123456789abcdef
	symbolMode = "" +
		".........ss..s.................." + // 0x00
		"ssssssssssssssssssssssssssssssss" + // 0x20
		"ssssssssssssssssssssssssssssesss" + // 0x40
		"sssssssssssssssssssssssssssspsss" + // 0x60
		"ssssssssssssssssssssssssssssssss" + // 0x80
		"ssssssssssssssssssssssssssssssss" + // 0xa0
		"ssssssssssssssssssssssssssssssss" + // 0xc0
		"ssssssssssssssssssssssssssssssssS" //  0xe0

	//   0123456789abcdef0123456789abcdef
	escMode = "" +
		"................................" + // 0x00
		"..E............................." + // 0x20
		".....................U......E..." + // 0x40
		"..E...E.......E...E.Eu.........." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................e" //  0xe0

	//   0123456789abcdef0123456789abcdef
	escByteMap = "" +
		"................................" + // 0x00
		"..\"............................." + // 0x20
		"............................\\..." + // 0x40
		"..\b...\f.......\n...\r.\t.........." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................E" //   0xe0

	//   0123456789abcdef0123456789abcdef
	runeMode = "" +
		"................................" + // 0x00
		"................1111111111......" + // 0x20
		".HHHHHH........................." + // 0x40
		".hhhhhh........................." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................r" //   0xe0

	//   0123456789abcdef0123456789abcdef
	sharpMode = "" +
		"................................" + // 0x00
		".......GV.......9999999999......" + // 0x20
		"..b............o........x.../..." + // 0x40
		"..b............o........x......." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................#" //  0xe0

	//   0123456789abcdef0123456789abcdef
	charMode = "" +
		".........CC..C.................." + // 0x00
		"C.......CCaa.aaaaaaaaaaaaaa.aaa." + // 0x20
		"aaaaaaaaaaaaaaaaaaaaaaaaaaa...aa" + // 0x40
		".aaaaaaaaaaaaaaaaaaaaaaaaaa...a." + // 0x60
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x80
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0xa0
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0xc0
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaC" //  0xe0

	//   0123456789abcdef0123456789abcdef
	intMode = "" +
		".........II..I.................." + // 0x00
		"I.......II.a.a..aaaaaaaaaa......" + // 0x20
		".aaaaaaaaaaaaaaaaaaaaaaaaaa....." + // 0x40
		".aaaaaaaaaaaaaaaaaaaaaaaaaa......" + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................i" //  0xe0

	//   0123456789abcdef0123456789abcdef
	sharpNumMode = "" +
		"................................" + // 0x00
		"................8888888888......" + // 0x20
		".A................r............." + // 0x40
		".A................r............." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................8" //  0xe0

	//   0123456789abcdef0123456789abcdef
	mustArrayMode = "" +
		"................................" + // 0x00
		"........{......................." + // 0x20
		"................................" + // 0x40
		"................................" + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................(" //  0xe0

	quoteMarker      = marker('q')
	sharpQuoteMarker = marker('#')
)

// marker on stack indicating a vector and not a list.
var vectorMarker = Vector{}

// Code is a list of S-Expressions read from LISP source code. It is a means
// of keeping loaded code together so that it can be evaluated and optimized
// for subsequent evaluations.
type Code []Object

type reader struct {
	tokenStart    int
	stack         []Object
	starts        []int
	line          int
	lineStart     int
	pos           int
	newQuote      func(args List) Object
	newSharpQuote func(args List) Object

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
	nextMode := valueMode
	var (
		b        byte
		buf      []byte
		rb       []byte
		rn       rune
		rcnt     int
		base     int
		sharpNum int
	)
	runeAppendByte := func(r byte) {
		rn = rn<<4 | rune(r)
		rcnt--
		if rcnt == 0 {
			if len(rb) < 8 {
				rb = make([]byte, 8)
			}
			n := utf8.EncodeRune(rb, rn)
			buf = append(buf, rb[:n]...)
			mode = nextMode
		}
	}
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

		case doubleQuote:
			r.tokenStart = r.pos + 1
			buf = buf[:0]
			mode = stringMode
			nextMode = stringMode
		case pipeByte:
			r.tokenStart = r.pos + 1
			buf = buf[:0]
			mode = symbolMode
			nextMode = symbolMode
		case stringByte:
			if 0 < len(buf) {
				buf = append(buf, b)
			}
		case stringDone:
			var obj Object
			if 0 < len(buf) {
				obj = String(buf)
			} else {
				obj = String(src[r.tokenStart:r.pos])
			}
			if 0 < len(r.stack) {
				r.stack = append(r.stack, obj)
			} else {
				r.code = append(r.code, obj)
			}
			mode = valueMode
		case pipeDone:
			var obj Object
			if 0 < len(buf) {
				obj = Symbol(buf)
			} else {
				obj = Symbol(src[r.tokenStart:r.pos])
			}
			if 0 < len(r.stack) {
				r.stack = append(r.stack, obj)
			} else {
				r.code = append(r.code, obj)
			}
			mode = valueMode

		case escByte:
			if len(buf) == 0 && r.tokenStart < r.pos {
				buf = append(buf, src[r.tokenStart:r.pos]...)
			}
			mode = escMode
		case escOne:
			buf = append(buf, escByteMap[b])
			mode = nextMode
		case escUnicode4:
			rn = 0
			rcnt = 4
			mode = runeMode
		case escUnicode8:
			rn = 0
			rcnt = 8
			mode = runeMode
		case runeDigit:
			runeAppendByte(b - '0')
		case runeHexA:
			runeAppendByte(b - 'A' + 10)
		case runeHexa:
			runeAppendByte(b - 'a' + 10)

		case sharpByte:
			mode = sharpMode
		case charSlash:
			r.tokenStart = r.pos + 1
			mode = charMode
		case charDone:
			r.pushChar(src)
			mode = valueMode
			goto Retry

		case vectorByte:
			r.starts = append(r.starts, len(r.stack))
			r.stack = append(r.stack, vectorMarker)
			mode = valueMode

		case binaryByte:
			r.tokenStart = r.pos + 1
			mode = intMode
			base = 2
		case octByte:
			r.tokenStart = r.pos + 1
			mode = intMode
			base = 8
		case hexByte:
			r.tokenStart = r.pos + 1
			mode = intMode
			base = 16
		case intDone:
			r.pushInteger(src, base)
			mode = valueMode
			goto Retry

		case sharpIntByte:
			mode = sharpNumMode
			sharpNum = int(b - '0')
		case sharpNumByte:
			sharpNum = sharpNum*10 + int(b-'0')
		case radixByte:
			r.tokenStart = r.pos + 1
			mode = intMode
			base = sharpNum

		case arrayByte:
			r.starts = append(r.starts, len(r.stack))
			switch sharpNum {
			case 0:
				r.stack = append(r.stack, &Array{})
			case 1:
				r.stack = append(r.stack, vectorMarker)
			default:
				if ArrayMaxRank < sharpNum {
					r.raise("%d exceeds the maximum Array rank of %d dimensions.", sharpNum, ArrayMaxRank)
				}
				r.stack = append(r.stack, &Array{dims: make([]int, sharpNum), sizes: make([]int, sharpNum)})
			}
			mode = mustArrayMode
		case swallowOpen:
			mode = valueMode

		case singleQuote:
			r.stack = append(r.stack, quoteMarker)
		case sharpQuote:
			r.stack = append(r.stack, sharpQuoteMarker)
			mode = valueMode

		default:
			switch mode {
			case sharpMode:
				r.raise("illegal sharp macro character: #%c", b)
			case intMode:
				r.raise("illegal base %d digit: #%c", base, b)
			default:
				r.raise("unexpected character: '%c' (0x%02x)", b, b)
			}
		}
	}
	r.pos++
	switch mode {
	case tokenMode:
		r.pushToken(src)
	case numberMode:
		r.pushNumber(src)
	case stringMode:
		r.partial("string not terminated")
	case runeMode:
		r.raise("rune not terminated")
	case escMode:
		r.raise("escaped character not terminated")
	case symbolMode:
		r.raise("|symbol| not terminated")
	case charMode:
		r.pushChar(src)
	case intMode:
		r.pushInteger(src, base)
	}
	if 0 < len(r.stack) {
		r.partial("list not terminated")
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

func (r *reader) partial(format string, args ...interface{}) {
	f := make([]byte, 0, len(format)+9)
	f = append(f, format...)
	f = append(f, " at %d:%d"...)
	args = append(args, r.line, r.pos-r.lineStart)
	panic(&Partial{Reason: fmt.Sprintf(string(f), args...), Depth: len(r.starts)})
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
	r.stack = r.stack[:start+1]
	var obj Object
	switch to := r.stack[start].(type) {
	case Vector:
		obj = Vector(list)
	case *Array:
		to.calcAndSet(list)
		obj = to
	default:
		if len(list) == 3 && list[1] == Symbol(".") {
			list[1] = list[2]
			obj = Cons(list[:2])
		} else {
			obj = list
		}
		if 0 < start {
			switch {
			case r.stack[start-1] == quoteMarker:
				if r.newQuote == nil {
					r.newQuote = CLPkg.Funcs["quote"].Create
				}
				obj = r.newQuote(List{obj})
				start--
				r.stack[start] = nil
				r.stack = r.stack[:start+1]
			case r.stack[start-1] == sharpQuoteMarker:
				if r.newSharpQuote == nil {
					r.newSharpQuote = CLPkg.Funcs["function"].Create
				}
				obj = r.newSharpQuote(List{obj})
				start--
				r.stack[start] = nil
				r.stack = r.stack[:start+1]
			}
		}
	}
	if 0 < start {
		r.stack[start] = obj
		r.starts = r.starts[:len(r.starts)-1]
	} else {
		r.stack = r.stack[:0]
		r.starts = r.starts[:0]
		r.code = append(r.code, obj)
	}
}

var (
	shortFloatRegex  = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]+(s[-+]?[0-9]+)?$`)
	singleFloatRegex = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]+(f[-+]?[0-9]+)?$`)
	doubleFloatRegex = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]+(d[-+]?[0-9]+)?$`)
	longFloatRegex   = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]+(l[-+]?[0-9]+)?$`)
)

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
	if 0 < len(r.stack) {
		switch {
		case r.stack[len(r.stack)-1] == quoteMarker:
			if r.newQuote == nil {
				r.newQuote = CLPkg.Funcs["quote"].Create
			}
			if len(r.stack) == 1 {
				r.code = append(r.code, r.newQuote(List{Symbol(token)}))
				r.stack[len(r.stack)-1] = nil
				r.stack = r.stack[:0]
			} else {
				r.stack[len(r.stack)-1] = r.newQuote(List{Symbol(token)})
			}
			return
		case r.stack[len(r.stack)-1] == sharpQuoteMarker:
			if r.newSharpQuote == nil {
				r.newSharpQuote = CLPkg.Funcs["function"].Create
			}
			if len(r.stack) == 1 {
				r.code = append(r.code, r.newSharpQuote(List{Symbol(token)}))
				r.stack[len(r.stack)-1] = nil
				r.stack = r.stack[:0]
			} else {
				r.stack[len(r.stack)-1] = r.newSharpQuote(List{Symbol(token)})
			}
			return
		}
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
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '+':
		/*
			s := string(token)
				if i, err := strconv.ParseInt(s, 10, 64); err == nil {
					obj = Fixnum(i)
					goto Push
				}
				if f, err := strconv.ParseFloat(s, 64); err == nil {
					obj = DoubleFloat(f)
					goto Push
				}
		*/
		if i := bytes.IndexByte(token, '/'); 0 < i {
			if num, err := strconv.ParseInt(string(token[:i]), 10, 64); err == nil {
				var den int64
				if den, err = strconv.ParseInt(string(token[i+1:]), 10, 64); err == nil {
					if 0 < den {
						obj = NewRatio(num, den)
						goto Push
					}
				}
			}
		}
		buf := bytes.ToLower(token)
		if doubleFloatRegex.Match(buf) {
			buf[bytes.IndexByte(buf, 'd')] = 'e'
			if f, err := strconv.ParseFloat(string(buf), 64); err == nil {
				obj = DoubleFloat(f)
				goto Push
			}
		}
		if shortFloatRegex.Match(buf) {
			buf[bytes.IndexByte(buf, 's')] = 'e'
			if f, err := strconv.ParseFloat(string(buf), 32); err == nil {
				obj = SingleFloat(f)
				goto Push
			}
		}
		if singleFloatRegex.Match(buf) {
			buf[bytes.IndexByte(buf, 'f')] = 'e'
			if f, err := strconv.ParseFloat(string(buf), 32); err == nil {
				obj = SingleFloat(f)
				goto Push
			}
		}
		if longFloatRegex.Match(buf) {
			buf[bytes.IndexByte(buf, 'l')] = 'e'
			cnt := len(buf) - 2 - bytes.Count(buf, []byte{'-'}) - bytes.Count(buf, []byte{'+'})
			if f, _, err := big.ParseFloat(string(buf), 10, uint(prec10t2*float64(cnt)), big.AwayFromZero); err == nil {

				obj = (*LongFloat)(f)
				goto Push
			}
		}
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
		bi = big.NewInt(0)
		if err := bi.UnmarshalText(token); err == nil {
			obj = (*Bignum)(bi)
			goto Push
		}
	}
	s = string(token)
	if i, err := strconv.ParseInt(s, 10, 64); err == nil {
		obj = Fixnum(i)
		goto Push
	}
	bi = big.NewInt(0)
	if _, ok := bi.SetString(s, 10); ok {
		obj = (*Bignum)(bi)
		goto Push
	}
	if f, err := strconv.ParseFloat(s, 64); err == nil {
		obj = DoubleFloat(f)
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

const hexByteValues = "" +
	"................................" + // 0x00
	"................\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09......" + // 0x20
	".\x0a\x0b\x0c\x0d\x0e\x0f........................." + // 0x40
	".\x0a\x0b\x0c\x0d\x0e\x0f........................." + // 0x60
	"................................" + // 0x80
	"................................" + // 0xa0
	"................................" + // 0xc0
	"................................" //   0xe0

var runeMap = map[string]Character{
	"backspace": Character('\b'),
	"newline":   Character('\n'),
	"page":      Character('\f'),
	"return":    Character('\r'),
	"rubout":    Character('\x7f'),
	"space":     Character(' '),
	"tab":       Character('\t'),
}

func (r *reader) pushChar(src []byte) {
	var c Character
	cnt := r.pos - r.tokenStart
	switch cnt {
	case 0:
		r.raise(`'#\' is not a valid character`)
	case 1:
		c = Character(src[r.tokenStart])
	default:
		var ok bool
		if c, ok = runeMap[string(bytes.ToLower(src[r.tokenStart:r.pos]))]; ok {
			break
		}
		if src[r.tokenStart] == 'u' || src[r.tokenStart] == 'U' {
			if 7 < cnt {
				break
			}
			var rn rune
			for _, b := range src[r.tokenStart+1 : r.pos] {
				rn = rn<<4 + rune(hexByteValues[b])
			}
			if rn <= unicode.MaxRune {
				c = Character(rn)
			}
			break
		}
		if rn, n := utf8.DecodeRune(src[r.tokenStart:r.pos]); 0 < n {
			c = Character(rn)
		}
	}
	if c == 0 {
		r.raise(`'#\%s' is not a valid character`, src[r.tokenStart:r.pos])
	}
	if 0 < len(r.stack) {
		r.stack = append(r.stack, c)
	} else {
		r.code = append(r.code, c)
	}
}

func (r *reader) pushInteger(src []byte, base int) {
	token := string(src[r.tokenStart:r.pos])
	var obj Object
	if i, err := strconv.ParseInt(token, base, 64); err == nil {
		obj = Fixnum(i)
	} else {
		bi := big.NewInt(0)
		if _, ok := bi.SetString(token, base); ok {
			obj = (*Bignum)(bi)
		} else {
			r.raise("%s is not a valid base 2 integer", token)
		}
	}
	if 0 < len(r.stack) {
		r.stack = append(r.stack, obj)
	} else {
		r.code = append(r.code, obj)
	}
}

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
		for _, obj := range c {
			b = append(b, "\n "...)
			b = ObjectAppend(b, obj)
		}
		if 0 < len(c) {
			b = append(b, '\n')
		}
		b = append(b, ']')
	}
	return string(b)
}

// Compile all the code elements. This evaluates all the defun, defvar, and
// defmacro calls and converts unquotes lists to functions.
func (c Code) Compile() {
	scope := NewScope()
	for i, obj := range c {
		list, ok := obj.(List)
		if !ok || len(list) == 0 {
			continue
		}
		var sym Symbol
		if sym, ok = list[len(list)-1].(Symbol); !ok {
			continue
		}
		var f Object
		switch strings.ToLower(string(sym)) {
		case "defun":
			f = ListToFunc(scope, list, 0)
			c[i] = f
		case "defvar":
			// TBD need a ListToFunc that sets up an undefined list
			f = ListToFunc(scope, list, 0)
			c[i] = f
		case "defmacro":
			panic("Defmacro not implemented yet")
		}
		if f != nil {
			f.Eval(scope, 0)
			// TBD is this correct? No need to eval defxxx more than once
			c[i] = nil
		}
	}
	// Now convert lists to functions.
	for i, obj := range c {
		list, ok := obj.(List)
		if !ok || len(list) == 0 {
			continue
		}
		c[i] = CompileList(list)
	}
}

// Eval all code elements and return the value of the last evaluation.
func (c Code) Eval(scope *Scope) (result Object) {
	for _, obj := range c {
		if obj != nil {
			result = obj.Eval(scope, 0)
		}
	}
	return
}
