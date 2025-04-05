// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"errors"
	"fmt"
	"io"
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
	sharpComplex = 'i'
	radixByte    = 'r'
	arrayByte    = 'A'
	swallowOpen  = '{'

	bitVectorByte = 'Z'
	bitVectorDone = 'z'

	singleQuote = 'q'
	backquote   = 'B'
	comma       = ','
	commaAt     = '@'

	blockStart = '|'
	blockEnd0  = '}'

	//   0123456789abcdef0123456789abcdef
	valueMode = "" +
		".........ak..a.................." + // 0x00
		"a.Q#..tq()tt,tttttttttttttt;ttt." + // 0x20
		"@tttttttttttttttttttttttttt...tt" + // 0x40
		"Btttttttttttttttttttttttttt.P.t." + // 0x60
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
		".......GV.Z.....9999999999......" + // 0x20
		"..bi...........o........x.../..." + // 0x40
		"..bi...........o........x...|..." + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................#" //  0xe0

	//   0123456789abcdef0123456789abcdef
	charMode = "" +
		".........CC..C.................." + // 0x00
		"C..a....CCaaaaaaaaaaaaaaaaa.aaa." + // 0x20
		"aaaaaaaaaaaaaaaaaaaaaaaaaaa...aa" + // 0x40
		".aaaaaaaaaaaaaaaaaaaaaaaaaa.a.a." + // 0x60
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

	//   0123456789abcdef0123456789abcdef
	blockCommentMode = "" +
		"aaaaaaaaaakaaaaaaaaaaaaaaaaaaaaa" + // 0x00
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x20
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x40
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaa}aaa" + // 0x60
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0x80
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0xa0
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" + // 0xc0
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|" //  0xe0

	//   0123456789abcdef0123456789abcdef
	blockEndMode = "" +
		"||||||||||k|||||||||||||||||||||" + // 0x00
		"|||c||||||||||||||||||||||||||||" + // 0x20
		"||||||||||||||||||||||||||||||||" + // 0x40
		"||||||||||||||||||||||||||||||||" + // 0x60
		"||||||||||||||||||||||||||||||||" + // 0x80
		"||||||||||||||||||||||||||||||||" + // 0xa0
		"||||||||||||||||||||||||||||||||" + // 0xc0
		"||||||||||||||||||||||||||||||||;" //  0xe0

	//   0123456789abcdef0123456789abcdef
	bitVectorMode = "" +
		".........zz..z.................." + // 0x00
		"z.......zz......aa.............." + // 0x20
		"................................" + // 0x40
		"................................" + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................z" //  0xe0

	quoteMarker      = marker('q')
	sharpQuoteMarker = marker('#')
	backquoteMarker  = marker('b')
	commaMarker      = marker(',')
	commaAtMarker    = marker('@')
)

var (
	// marker on stack indicating a vector and not a list.
	vectorMarker = &Vector{}

	// marker on stack indicating a vector and not a list.
	complexMarker = Complex(complex(0, 0))

	// Set as needed. Global since they are build in but from a different go
	// package.
	newQuote      func(args List) Object
	newSharpQuote func(args List) Object
	newBackquote  func(args List) Object
	newComma      func(args List) Object
	newCommaAt    func(args List) Object

	decimalRegex     = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]*$`)
	eFloatRegex      = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]*e[-+]?[0-9]+?$`)
	shortFloatRegex  = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]*s[-+]?[0-9]+?$`)
	singleFloatRegex = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]*f[-+]?[0-9]+?$`)
	doubleFloatRegex = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]*d[-+]?[0-9]+?$`)
	longFloatRegex   = regexp.MustCompile(`^[-+]?[0-9]+\.?[0-9]*l[-+]?[0-9]+?$`)

	intRxs = []*regexp.Regexp{
		nil, nil,
		regexp.MustCompile(`^[-+]?[0-1]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-2]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-3]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-4]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-5]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-6]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-7]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-8]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-b]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-c]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-d]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-e]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-f]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-g]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-h]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-i]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-j]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-k]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-l]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-m]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-n]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-o]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-p]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-q]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-r]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-s]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-t]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-u]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-v]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-w]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-x]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-y]+\.?$`),
		regexp.MustCompile(`^[-+]?[0-9a-z]+\.?$`),
	}
	ratioRxs = []*regexp.Regexp{
		nil, nil,
		regexp.MustCompile(`^[-+]?[0-1]+/[-+]?[0-1]+$`),
		regexp.MustCompile(`^[-+]?[0-2]+/[-+]?[0-2]+$`),
		regexp.MustCompile(`^[-+]?[0-3]+/[-+]?[0-3]+$`),
		regexp.MustCompile(`^[-+]?[0-4]+/[-+]?[0-4]+$`),
		regexp.MustCompile(`^[-+]?[0-5]+/[-+]?[0-5]+$`),
		regexp.MustCompile(`^[-+]?[0-6]+/[-+]?[0-6]+$`),
		regexp.MustCompile(`^[-+]?[0-7]+/[-+]?[0-7]+$`),
		regexp.MustCompile(`^[-+]?[0-8]+/[-+]?[0-8]+$`),
		regexp.MustCompile(`^[-+]?[0-9]+/[-+]?[0-9]+$`),
		regexp.MustCompile(`^[-+]?[0-9a]+/[-+]?[0-9a]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-b]+/[-+]?[0-9a-b]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-c]+/[-+]?[0-9a-c]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-d]+/[-+]?[0-9a-d]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-e]+/[-+]?[0-9a-e]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-f]+/[-+]?[0-9a-f]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-g]+/[-+]?[0-9a-g]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-h]+/[-+]?[0-9a-h]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-i]+/[-+]?[0-9a-i]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-j]+/[-+]?[0-9a-j]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-k]+/[-+]?[0-9a-k]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-l]+/[-+]?[0-9a-l]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-m]+/[-+]?[0-9a-m]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-n]+/[-+]?[0-9a-n]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-o]+/[-+]?[0-9a-o]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-p]+/[-+]?[0-9a-p]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-q]+/[-+]?[0-9a-q]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-r]+/[-+]?[0-9a-r]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-s]+/[-+]?[0-9a-s]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-t]+/[-+]?[0-9a-t]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-u]+/[-+]?[0-9a-u]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-v]+/[-+]?[0-9a-v]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-w]+/[-+]?[0-9a-w]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-x]+/[-+]?[0-9a-x]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-y]+/[-+]?[0-9a-y]+$`),
		regexp.MustCompile(`^[-+]?[0-9a-z]+/[-+]?[0-9a-z]+$`),
	}
)

// Code is a list of S-Expressions read from LISP source code. It is a means
// of keeping loaded code together so that it can be evaluated and optimized
// for subsequent evaluations.
type Code []Object

type reader struct {
	tokenStart int
	mode       string
	nextMode   string
	stack      []Object
	starts     []int
	carry      []byte // carry over from previous stream read
	buf        []byte
	line       int
	lineStart  int
	pos        int
	base       int // temp base
	rbase      int // read base
	sharpNum   int
	code       Code
	rb         []byte
	rn         rune
	rcnt       int
	intRx      *regexp.Regexp
	ratioRx    *regexp.Regexp
	floatType  Symbol
	one        bool
	more       bool // more to read
}

func (r *reader) scoped(s *Scope) {
	r.rbase = 10
	r.intRx = intRxs[10]
	r.ratioRx = ratioRxs[10]
	if num, ok := s.get("*read-base*").(Fixnum); ok && 1 < num && num <= 36 {
		r.rbase = int(num)
		r.intRx = intRxs[num]
		r.ratioRx = ratioRxs[num]
	}
	ff, _ := s.get("*read-default-float-format*").(Symbol)
	switch ff {
	case SingleFloatSymbol, ShortFloatSymbol, DoubleFloatSymbol, LongFloatSymbol:
		r.floatType = ff
	default: // default to double-float
		r.floatType = DoubleFloatSymbol
	}
}

// ReadString reads LISP source code and return a Code instance.
func ReadString(src string, s *Scope) (code Code) {
	cr := reader{
		mode:     valueMode,
		nextMode: valueMode,
	}
	cr.scoped(s)
	cr.read([]byte(src))

	return cr.code
}

// Read LISP source code and return a Code instance.
func Read(src []byte, s *Scope) (code Code) {
	cr := reader{
		mode:     valueMode,
		nextMode: valueMode,
	}
	cr.scoped(s)
	cr.read(src)

	return cr.code
}

// ReadOne LISP source code and return a Code instance.
func ReadOne(src []byte, s *Scope) (code Code, pos int) {
	cr := reader{
		mode:     valueMode,
		nextMode: valueMode,
		one:      true,
	}
	cr.scoped(s)
	cr.read(src)

	return cr.code, cr.pos
}

const readBlockSize = 65536

// ReadStream reads LISP source code from a stream and return a Code instance.
func ReadStream(r io.Reader, s *Scope, one ...bool) (Code, int) {
	// Note: Using a separate chan for reading from disk was tried but since a
	// new buffer had to be created each time the overall performance was
	// slightly worse. That would be different with slower disks but that is
	// more a issue left behind years ago.
	cr := reader{
		more:     true,
		mode:     valueMode,
		nextMode: valueMode,
		one:      (0 < len(one) && one[0]),
	}
	cr.scoped(s)
	var pos int
	buf := make([]byte, readBlockSize)
	for {
		cnt, err := r.Read(buf)
		src := buf[:cnt]
		cr.tokenStart = 0
		if err != nil {
			if errors.Is(err, io.EOF) {
				cr.more = false
			} else {
				panic(err)
			}
		}
		cr.read(src)
		pos += cr.pos
		if cr.one && 0 < len(cr.code) {
			break
		}
		if !cr.more {
			break
		}
	}
	return cr.code, pos
}

// ReadStreamPush reads LISP source code from a stream and pushes
// s-expressions read onto a channel.
func ReadStreamPush(r io.Reader, s *Scope, channel chan Object) {
	cr := reader{
		more:     true,
		mode:     valueMode,
		nextMode: valueMode,
	}
	cr.scoped(s)
	buf := make([]byte, readBlockSize)
	for {
		cnt, err := r.Read(buf)
		src := buf[:cnt]
		cr.tokenStart = 0
		if err != nil {
			if errors.Is(err, io.EOF) {
				cr.more = false
			} else {
				panic(err)
			}
		}
		cr.read(src)
		if 0 < len(cr.code) {
			for _, obj := range cr.code {
				channel <- obj
			}
			cr.code = cr.code[:0]
		}
		if !cr.more {
			break
		}
	}
}

// ReadStreamEach reads LISP source code from a stream and calls the callback
// for each s-expressions read.
func ReadStreamEach(r io.Reader, s *Scope, caller Caller) {
	cr := reader{
		more:     true,
		mode:     valueMode,
		nextMode: valueMode,
	}
	cr.scoped(s)
	buf := make([]byte, readBlockSize)
	for {
		cnt, err := r.Read(buf)
		src := buf[:cnt]
		cr.tokenStart = 0
		if err != nil {
			if errors.Is(err, io.EOF) {
				cr.more = false
			} else {
				panic(err)
			}
		}
		cr.read(src)
		if 0 < len(cr.code) {
			for _, obj := range cr.code {
				_ = caller.Call(s, List{obj}, 0)
			}
			cr.code = cr.code[:0]
		}
		if !cr.more {
			break
		}
	}
}

// CompileString LISP string source code and return an Object.
func CompileString(src string, s *Scope) Object {
	return Compile([]byte(src), s)
}

// Compile LISP source code and return an Object.
func Compile(src []byte, s *Scope) (result Object) {
	cr := reader{
		mode:     valueMode,
		nextMode: valueMode,
	}
	cr.scoped(s)
	cr.read(src)

	cr.code.Compile()
	if 0 < len(cr.code) {
		result = cr.code[len(cr.code)-1]
	}
	return
}

func (r *reader) runeAppendByte(b byte) {
	r.rn = r.rn<<4 | rune(b)
	r.rcnt--
	if r.rcnt == 0 {
		if len(r.rb) < 8 {
			r.rb = make([]byte, 8)
		}
		n := utf8.EncodeRune(r.rb, r.rn)
		r.buf = append(r.buf, r.rb[:n]...)
		r.mode = r.nextMode
	}
}

func (r *reader) read(src []byte) {
	var b byte
	// If src is empty then the for loop will not set r.pos so initialize to
	// -1 to keep r.pos where it should be.
	r.pos = -1
	for r.pos, b = range src {
	Retry:
		switch r.mode[b] {
		case skipNewline:
			r.line++
			r.lineStart = r.pos
		case skipByte:
			// skip
		case commentByte:
			r.mode = commentMode
		case commentDone:
			r.mode = valueMode

		case openParen:
			r.starts = append(r.starts, len(r.stack))
			r.stack = append(r.stack, nil)
		case closeParen:
			r.closeList()

		case tokenStart:
			if r.mode != tokenMode {
				r.tokenStart = r.pos
				r.mode = tokenMode
			}
		case tokenDone:
			r.pushToken(src)
			r.mode = valueMode
			goto Retry

		case doubleQuote:
			r.tokenStart = r.pos + 1
			r.buf = r.buf[:0]
			r.mode = stringMode
			r.nextMode = stringMode
		case pipeByte:
			r.tokenStart = r.pos + 1
			r.buf = r.buf[:0]
			r.mode = symbolMode
			r.nextMode = symbolMode
		case stringByte:
			if 0 < len(r.buf) {
				r.buf = append(r.buf, b)
			}
		case stringDone:
			var obj Object
			if 0 < len(r.buf) {
				obj = String(r.buf)
			} else {
				obj = String(src[r.tokenStart:r.pos])
			}
			if 0 < len(r.stack) {
				r.stack = append(r.stack, obj)
			} else {
				r.code = append(r.code, obj)
			}
			r.mode = valueMode
		case pipeDone:
			var obj Object
			if 0 < len(r.buf) {
				obj = Symbol(r.buf)
			} else {
				obj = Symbol(src[r.tokenStart:r.pos])
			}
			if 0 < len(r.stack) {
				r.stack = append(r.stack, obj)
			} else {
				r.code = append(r.code, obj)
			}
			r.mode = valueMode

		case escByte:
			if len(r.buf) == 0 && r.tokenStart < r.pos {
				r.buf = append(r.buf, src[r.tokenStart:r.pos]...)
			}
			r.mode = escMode
		case escOne:
			r.buf = append(r.buf, escByteMap[b])
			r.mode = r.nextMode
		case escUnicode4:
			r.rn = 0
			r.rcnt = 4
			r.mode = runeMode
		case escUnicode8:
			r.rn = 0
			r.rcnt = 8
			r.mode = runeMode
		case runeDigit:
			r.runeAppendByte(b - '0')
		case runeHexA:
			r.runeAppendByte(b - 'A' + 10)
		case runeHexa:
			r.runeAppendByte(b - 'a' + 10)

		case sharpByte:
			r.mode = sharpMode
		case charSlash:
			r.tokenStart = r.pos + 1
			r.mode = charMode
		case charDone:
			r.pushChar(src)
			r.mode = valueMode
			goto Retry

		case vectorByte:
			r.starts = append(r.starts, len(r.stack))
			r.stack = append(r.stack, vectorMarker)
			r.mode = valueMode

		case binaryByte:
			r.tokenStart = r.pos + 1
			r.mode = intMode
			r.base = 2
		case octByte:
			r.tokenStart = r.pos + 1
			r.mode = intMode
			r.base = 8
		case hexByte:
			r.tokenStart = r.pos + 1
			r.mode = intMode
			r.base = 16
		case intDone:
			r.pushInteger(src)
			r.mode = valueMode
			goto Retry

		case sharpIntByte:
			r.mode = sharpNumMode
			r.sharpNum = int(b - '0')
		case sharpNumByte:
			r.sharpNum = r.sharpNum*10 + int(b-'0')
		case radixByte:
			r.tokenStart = r.pos + 1
			r.mode = intMode
			r.base = r.sharpNum

		case sharpComplex:
			r.starts = append(r.starts, len(r.stack))
			r.stack = append(r.stack, complexMarker)
			r.mode = mustArrayMode

		case arrayByte:
			r.starts = append(r.starts, len(r.stack))
			switch r.sharpNum {
			case 0:
				r.stack = append(r.stack, &Array{})
			case 1:
				r.stack = append(r.stack, vectorMarker)
			default:
				if ArrayMaxRank < r.sharpNum {
					r.raise("%d exceeds the maximum Array rank of %d dimensions.", r.sharpNum, ArrayMaxRank)
				}
				r.stack = append(r.stack, &Array{dims: make([]int, r.sharpNum), sizes: make([]int, r.sharpNum)})
			}
			r.mode = mustArrayMode
		case swallowOpen:
			r.mode = valueMode

		case singleQuote:
			r.stack = append(r.stack, quoteMarker)
		case sharpQuote:
			r.stack = append(r.stack, sharpQuoteMarker)
			r.mode = valueMode

		case backquote:
			r.stack = append(r.stack, backquoteMarker)
		case comma:
			if !r.inBackquote() {
				r.raise("comma not inside a backquote")
			}
			r.stack = append(r.stack, commaMarker)
		case commaAt:
			if 0 < len(r.stack) && r.stack[len(r.stack)-1] == commaMarker {
				r.stack[len(r.stack)-1] = commaAtMarker
			} else if r.mode != tokenMode {
				r.tokenStart = r.pos
				r.mode = tokenMode
			}

		case blockStart:
			r.mode = blockCommentMode
		case blockEnd0:
			r.mode = blockEndMode

		case bitVectorByte:
			r.tokenStart = r.pos + 1
			r.mode = bitVectorMode
		case bitVectorDone:
			token := r.makeToken(src)
			if 0 < len(r.stack) {
				r.stack = append(r.stack, ReadBitVector(token))
			} else {
				r.code = append(r.code, ReadBitVector(token))
			}
			r.mode = valueMode
			goto Retry

		default:
			switch r.mode {
			case sharpMode:
				r.raise("illegal sharp macro character: #%c", b)
			case intMode:
				r.raise("illegal base %d digit: #%c", r.base, b)
			default:
				r.raise("unexpected character: '%c' (0x%02x)", b, b)
			}
		}
		if r.one && 0 < len(r.code) {
			if b == ')' {
				r.pos++
			}
			return
		}
	}
	r.pos++
	if r.more {
		r.carry = append(r.carry, src[r.tokenStart:r.pos]...)
	} else {
		switch r.mode {
		case tokenMode:
			r.pushToken(src)
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
			r.pushInteger(src)
		}
		if 0 < len(r.stack) {
			r.partial("list not terminated")
		}
	}
}

func (r *reader) inBackquote() bool {
	for _, v := range r.stack {
		if backquoteMarker == v {
			return true
		}
	}
	return false
}

func (r *reader) raise(format string, args ...any) {
	f := make([]byte, 0, len(format)+9)
	f = append(f, format...)
	f = append(f, " at %d:%d"...)
	args = append(args, r.line, r.pos-r.lineStart)
	PanicParse(string(f), args...)
}

func (r *reader) partial(format string, args ...any) {
	f := make([]byte, 0, len(format)+9)
	f = append(f, format...)
	f = append(f, " at %d:%d"...)
	args = append(args, r.line, r.pos-r.lineStart)
	panic(NewPartial(len(r.starts), string(f), args...))
}

func (r *reader) closeList() {
	if len(r.starts) == 0 {
		r.raise("unmatched close parenthesis")
	}
	start := r.starts[len(r.starts)-1]
	size := len(r.stack) - start - 1
	list := make(List, size)
	copy(list, r.stack[start+1:])
	// TBD does the stack need to be cleared (set to nil) before shrinking?
	r.stack = r.stack[:start+1]
	var obj Object
	switch to := r.stack[start].(type) {
	case *Vector:
		obj = NewVector(len(list), TrueSymbol, nil, list, true)
	case *Array:
		to.calcAndSet(list)
		obj = to
	case Complex:
		obj = newComplex(list)
	default:
		if 3 <= len(list) && list[len(list)-2] == Symbol(".") {
			if list[len(list)-1] == nil {
				list[len(list)-2] = nil
			} else {
				list[len(list)-2] = Tail{Value: list[len(list)-1]}
			}
			list = list[:len(list)-1]
			obj = list
		} else {
			obj = list
		}
		if 0 < start {
			switch r.stack[start-1] {
			case quoteMarker:
				if newQuote == nil {
					newQuote = CLPkg.GetFunc("quote").Create
				}
				obj = newQuote(List{obj})
				start--
				r.stack[start] = nil
				r.stack = r.stack[:start+1]
			case sharpQuoteMarker:
				if newSharpQuote == nil {
					newSharpQuote = CLPkg.GetFunc("function").Create
				}
				obj = newSharpQuote(List{obj})
				start--
				r.stack[start] = nil
				r.stack = r.stack[:start+1]
			case backquoteMarker:
				if newBackquote == nil {
					newBackquote = CLPkg.GetFunc("backquote").Create
				}
				obj = newBackquote(List{obj})
				start--
				r.stack[start] = nil
				r.stack = r.stack[:start+1]
			case commaMarker:
				if newComma == nil {
					newComma = CLPkg.GetFunc("comma").Create
				}
				obj = newComma(List{obj})
				start--
				r.stack[start] = nil
				r.stack = r.stack[:start+1]
			case commaAtMarker:
				if newCommaAt == nil {
					newCommaAt = CLPkg.GetFunc("comma-at").Create
				}
				obj = newCommaAt(List{obj})
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
	token = r.makeToken(src)
	if size == 3 && bytes.EqualFold([]byte("nil"), token) {
		obj = nil
		goto Push
	}
	if 0 < len(r.stack) {
		switch r.stack[len(r.stack)-1] {
		case quoteMarker:
			if newQuote == nil {
				newQuote = CLPkg.GetFunc("quote").Create
			}
			if len(r.stack) == 1 {
				r.code = append(r.code, newQuote(List{Symbol(token)}))
				r.stack[len(r.stack)-1] = nil
				r.stack = r.stack[:0]
			} else {
				r.stack[len(r.stack)-1] = newQuote(List{Symbol(token)})
			}
			return
		case sharpQuoteMarker:
			if newSharpQuote == nil {
				newSharpQuote = CLPkg.GetFunc("function").Create
			}
			if len(r.stack) == 1 {
				r.code = append(r.code, newSharpQuote(List{Symbol(token)}))
				r.stack[len(r.stack)-1] = nil
				r.stack = r.stack[:0]
			} else {
				r.stack[len(r.stack)-1] = newSharpQuote(List{Symbol(token)})
			}
			return
		case backquoteMarker:
			if newBackquote == nil {
				newBackquote = CLPkg.GetFunc("backquote").Create
			}
			if len(r.stack) == 1 {
				r.code = append(r.code, newBackquote(List{Symbol(token)}))
				r.stack[len(r.stack)-1] = nil
				r.stack = r.stack[:0]
			} else {
				r.stack[len(r.stack)-1] = newBackquote(List{Symbol(token)})
			}
			return
		case commaMarker:
			if newComma == nil {
				newComma = CLPkg.GetFunc("comma").Create
			}
			if len(r.stack) == 1 {
				r.code = append(r.code, newComma(List{Symbol(token)}))
				r.stack[len(r.stack)-1] = nil
				r.stack = r.stack[:0]
			} else {
				r.stack[len(r.stack)-1] = newComma(List{Symbol(token)})
			}
			return
		case commaAtMarker:
			if newCommaAt == nil {
				newCommaAt = CLPkg.GetFunc("comma-at").Create
			}
			if len(r.stack) == 1 {
				r.code = append(r.code, newCommaAt(List{Symbol(token)}))
				r.stack[len(r.stack)-1] = nil
				r.stack = r.stack[:0]
			} else {
				r.stack[len(r.stack)-1] = newCommaAt(List{Symbol(token)})
			}
			return
		}
	}
	obj = r.resolveToken(token)
Push:
	if 0 < len(r.stack) {
		r.stack = append(r.stack, obj)
	} else {
		r.code = append(r.code, obj)
	}
}

func (r *reader) resolveToken(token []byte) Object {
	buf := bytes.ToLower(token)
	switch {
	case buf[0] == '@':
		// This is an extension to common lisp to make time easier to deal with.
		s := string(token[1:])
		for _, layout := range []string{
			time.RFC3339Nano,
			time.RFC3339,
			"2006-01-02T15:04:05",
			"2006-01-02",
		} {
			if t, err := time.ParseInLocation(layout, s, time.UTC); err == nil {
				return Time(t)
			}
		}
	case r.intRx.Match(buf):
		buf = bytes.TrimRight(buf, ".")
		if num, err := strconv.ParseInt(string(buf), r.rbase, 64); err == nil {
			return Fixnum(num)
		}
		bi := big.NewInt(0)
		if _, ok := bi.SetString(string(buf), r.rbase); ok {
			return (*Bignum)(bi)
		}
	case decimalRegex.Match(buf) || eFloatRegex.Match(buf):
		if r.floatType == LongFloatSymbol {
			cnt := len(buf)
			epos := bytes.IndexByte(buf, 'e')
			if 0 < epos {
				cnt = epos
			}
			if buf[0] == '-' || buf[0] == '+' {
				cnt--
			}
			if f, _, err := big.ParseFloat(string(buf), 10, uint(prec10t2*float64(cnt)), big.ToNearestAway); err == nil {
				return (*LongFloat)(f)
			}
		} else if f, err := strconv.ParseFloat(string(buf), 64); err == nil {
			switch r.floatType {
			case SingleFloatSymbol, ShortFloatSymbol:
				return SingleFloat(f)
			default: // double-float
				return DoubleFloat(f)
			}
		}
	case doubleFloatRegex.Match(buf):
		buf[bytes.IndexByte(buf, 'd')] = 'e'
		if f, err := strconv.ParseFloat(string(buf), 64); err == nil {
			return DoubleFloat(f)
		}
	case shortFloatRegex.Match(buf):
		buf[bytes.IndexByte(buf, 's')] = 'e'
		if f, err := strconv.ParseFloat(string(buf), 32); err == nil {
			return SingleFloat(f)
		}
	case singleFloatRegex.Match(buf):
		buf[bytes.IndexByte(buf, 'f')] = 'e'
		if f, err := strconv.ParseFloat(string(buf), 32); err == nil {
			return SingleFloat(f)
		}
	case longFloatRegex.Match(buf):
		cnt := bytes.IndexByte(buf, 'l')
		buf[cnt] = 'e'
		if buf[0] == '-' || buf[0] == '+' {
			cnt--
		}
		if f, _, err := big.ParseFloat(string(buf), 10, uint(prec10t2*float64(cnt)), big.ToNearestAway); err == nil {
			return (*LongFloat)(f)
		}
	case r.ratioRx.Match(buf):
		i := bytes.IndexByte(buf, '/')
		if num, err := strconv.ParseInt(string(buf[:i]), r.rbase, 64); err == nil {
			var den int64
			if den, err = strconv.ParseInt(string(buf[i+1:]), r.rbase, 64); err == nil {
				if 0 < den {
					return NewRatio(num, den)
				}
			}
		}
		// Didn't pass as fixnum ratio so try bignum.
		var (
			num big.Int
			den big.Int
		)
		if _, ok := num.SetString(string(buf[:i]), r.rbase); ok {
			if _, ok = den.SetString(string(buf[i+1:]), r.rbase); ok {
				if 0 < den.Sign() {
					return NewBigRatio(&num, &den)
				}
			}
		}
	}
	return Symbol(token)
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

func (r *reader) pushInteger(src []byte) {
	token := string(r.makeToken(src))
	var obj Object
	if i, err := strconv.ParseInt(token, r.base, 64); err == nil {
		obj = Fixnum(i)
	} else {
		bi := big.NewInt(0)
		if _, ok := bi.SetString(token, r.base); ok {
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
// defmacro calls and converts unquoted lists to functions.
func (c Code) Compile() {
	scope := NewScope()
	for i, obj := range c {
		list, ok := obj.(List)
		if !ok || len(list) == 0 {
			continue
		}
		var sym Symbol
		switch tv := list[0].(type) {
		case Symbol:
			sym = tv
		case List:
			if 0 < len(tv) {
				if s2, ok2 := tv[0].(Symbol); ok2 {
					if strings.EqualFold("lambda", string(s2)) {
						break
					}
				}
			}
			PanicParse("%s is not a function", tv)
		default:
			PanicParse("%s is not a function", tv)
		}
		var f Object
		switch strings.ToLower(string(sym)) {
		case "defun", "defmacro", "defvar", "defparameter", "defconstant":
			f = ListToFunc(scope, list, 0)
			c[i] = f
		}
		if f != nil {
			name := f.Eval(scope, 0)
			if newQuote == nil {
				newQuote = CLPkg.GetFunc("quote").Create
			}
			c[i] = newQuote(List{name})
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
func (c Code) Eval(scope *Scope, w io.Writer) (result Object) {
	for _, obj := range c {
		if obj != nil {
			result = obj.Eval(scope, 0)
			if w != nil {
				_, _ = fmt.Fprintf(w, ";;  %s\n", ObjectString(result))
			}
		}
	}
	return
}

func (r *reader) makeToken(src []byte) []byte {
	token := make([]byte, len(r.carry)+(r.pos-r.tokenStart))
	copy(token, r.carry)
	copy(token[len(r.carry):], src[r.tokenStart:r.pos])
	r.carry = r.carry[:0]

	return token
}
