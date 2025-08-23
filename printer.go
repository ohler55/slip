// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"fmt"
	"io"
	"math"
	"math/big"
	"strconv"
	"strings"
	"unicode"
	"unsafe"

	"github.com/ohler55/ojg"
)

const (
	bold               = "\x1b[1m"
	underline          = "\x1b[4m"
	colorOff           = "\x1b[m"
	indentSpaces       = "                                                                                "
	DefaultRightMargin = 120

	upcaseKey     = Symbol(":upcase")
	downcaseKey   = Symbol(":downcase")
	capitalizeKey = Symbol(":capitalize")

	//   0123456789abcdef0123456789abcdef
	needPipeMap = "" +
		"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" + // 0x00
		"xxxxxxxxxx..x..x...........x...." + // 0x20
		"...........................xxx.." + // 0x40
		"x..........................xxx.x" + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................" //   0xe0
)

type node struct {
	value    Object
	elements []*node
	size     int
	special  string
	funky    bool

	buf []byte
}

// Printer of Objects. The values of it's members determine how Objects will
// be encoded for printing.
type Printer struct {

	// ANSI backs *print-ansi*.
	ANSI bool

	// Array backs *print-array*.
	Array bool

	// Base backs *print-base*.
	Base uint

	// Case backs *print-case*. A value of nil indicates no transformation.
	Case Object

	// Circle backs *print-circle*.
	Circle bool

	// Escape backs *print-escape*.
	Escape bool

	// Gensym backs *print-gensym*.
	Gensym bool

	// Lambda should be printed readably *print-lambda* .
	Lambda bool

	// Length backs *print-length*.
	Length uint

	// Level backs *print-level*.
	Level uint

	// Lines backs *print-lines*.
	Lines uint

	// MiserWidth *print-miser-width*.
	MiserWidth uint

	// Pretty backs *print-pretty*.
	Pretty bool

	// Radix backs *print-radix*.
	Radix bool

	// Readably *print-readably* .
	Readably bool

	// ReadablyError if true non-readably objects will generate an error.
	ReadablyError bool

	// RightMargin *print-right-margin*.
	RightMargin uint

	// Prec backs *print-precision*.
	Prec int
}

var (
	printer = Printer{
		ANSI:          true,
		Array:         false,
		Base:          10,
		Case:          downcaseKey,
		Circle:        false,
		Escape:        true,
		Gensym:        true,
		Lambda:        false,
		Length:        math.MaxInt,
		Level:         math.MaxInt,
		Lines:         math.MaxInt,
		Prec:          -1,
		MiserWidth:    0,
		Pretty:        true,
		Radix:         false,
		Readably:      false,
		ReadablyError: true,
		RightMargin:   DefaultRightMargin,
	}
)

// DefaultPrinter returns the default Printer.
func DefaultPrinter() *Printer {
	return &printer
}

// Write an Object to *standard-output*.
func Write(obj Object) {
	printer.Write(obj)
}

// Append an Object to a byte array using the global print variables.
func Append(b []byte, obj Object) []byte {
	return printer.Append(b, obj, 0)
}

// Write an Object to *standard-output* using the Printer variables.
func (p *Printer) Write(obj Object) {
	if _, err := StandardOutput.(io.Writer).Write(p.Append([]byte{}, obj, 0)); err != nil {
		StreamPanic(NewScope(), 0, StandardOutput.(Stream), "%s", err)
	}
}

// Append an Object to a byte array using the Printer variables.
func (p *Printer) Append(b []byte, obj Object, level int) []byte {
Top:
	switch to := obj.(type) {
	case nil:
		b = append(b, p.caseName("nil")...)
	case Character:
		if p.Escape {
			// The Common Lisp docs are a bit vague and leave options open for
			// when the #\ escape sequences is used.
			b = to.Append(b)
		} else {
			b = append(b, string([]rune{rune(to)})...)
		}
	case Fixnum:
		if p.Radix {
			switch p.Base {
			case 2:
				b = append(b, "#b"...)
				b = strconv.AppendInt(b, int64(to), int(p.Base))
			case 8:
				b = append(b, "#o"...)
				b = strconv.AppendInt(b, int64(to), int(p.Base))
			case 16:
				b = append(b, "#x"...)
				b = strconv.AppendInt(b, int64(to), int(p.Base))
			case 10:
				b = strconv.AppendInt(b, int64(to), 10)
				b = append(b, '.')
			default:
				b = append(b, '#')
				b = strconv.AppendInt(b, int64(p.Base), 10)
				b = append(b, 'r')
				b = strconv.AppendInt(b, int64(to), int(p.Base))
			}
		} else {
			b = strconv.AppendInt(b, int64(to), int(p.Base))
		}
	case *Bignum:
		if p.Radix {
			switch p.Base {
			case 2:
				b = append(b, "#b"...)
				b = (*big.Int)(to).Append(b, int(p.Base))
			case 8:
				b = append(b, "#o"...)
				b = (*big.Int)(to).Append(b, int(p.Base))
			case 16:
				b = append(b, "#x"...)
				b = (*big.Int)(to).Append(b, int(p.Base))
			case 10:
				b = (*big.Int)(to).Append(b, int(p.Base))
				b = append(b, '.')
			default:
				b = append(b, '#')
				b = strconv.AppendInt(b, int64(p.Base), 10)
				b = append(b, 'r')
				b = (*big.Int)(to).Append(b, int(p.Base))
			}
		} else {
			b = (*big.Int)(to).Append(b, int(p.Base))
		}
	case Octet:
		obj = Fixnum(to)
		goto Top
	case *Ratio:
		if (*big.Rat)(to).IsInt() {
			obj = (*Bignum)((*big.Rat)(to).Num())
			goto Top
		}
		if p.Radix {
			switch p.Base {
			case 2:
				b = append(b, "#b"...)
			case 8:
				b = append(b, "#o"...)
			case 16:
				b = append(b, "#x"...)
			default:
				b = append(b, '#')
				b = strconv.AppendInt(b, int64(p.Base), 10)
				b = append(b, 'r')
			}
		}
		b = (*big.Rat)(to).Num().Append(b, int(p.Base))
		b = append(b, '/')
		b = (*big.Rat)(to).Denom().Append(b, int(p.Base))
	case SingleFloat:
		// Use the LISP exponent nomenclature by forming the buffer and
		// then replacing the 'e'.
		var tmp []byte
		switch {
		case p.Readably:
			// float32 precision is 7.
			if p.Prec < 7 {
				tmp = strconv.AppendFloat([]byte{}, float64(to), 'e', p.Prec, 32)
			} else {
				tmp = strconv.AppendFloat([]byte{}, float64(to), 'e', -1, 32)
			}
			b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'s'})...)
		case p.Prec < 7:
			b = strconv.AppendFloat(b, float64(to), 'g', p.Prec, 32)
		default:
			tmp = strconv.AppendFloat([]byte{}, float64(to), 'g', -1, 32)
			b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'s'})...)
		}
	case DoubleFloat:
		// Use the LISP exponent nomenclature by forming the buffer and
		// then replacing the 'e'.
		var tmp []byte
		switch {
		case p.Readably:
			// float64 precision is 16.
			if p.Prec < 16 {
				tmp = strconv.AppendFloat([]byte{}, float64(to), 'e', p.Prec, 64)
			} else {
				tmp = strconv.AppendFloat([]byte{}, float64(to), 'e', -1, 64)
			}
			b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'d'})...)
		case p.Prec < 16:
			b = strconv.AppendFloat(b, float64(to), 'g', p.Prec, 64)
		default:
			tmp = strconv.AppendFloat([]byte{}, float64(to), 'g', -1, 64)
			b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'d'})...)
		}
	case *LongFloat:
		prec := -1
		if 0 < p.Prec {
			prec = int(float64((*big.Float)(to).Prec()) / prec10t2)
			if p.Prec < prec {
				prec = p.Prec
			}
		}
		if p.Readably {
			// Use the LISP exponent nomenclature by forming the buffer and
			// then replacing the 'e'.
			tmp := (*big.Float)(to).Append([]byte{}, 'e', prec)
			b = append(b, bytes.ReplaceAll(tmp, []byte{'e'}, []byte{'L'})...)
		} else {
			b = (*big.Float)(to).Append(b, 'g', prec)
		}
	case Symbol:
		if len(to) == 0 {
			b = append(b, "||"...)
			break
		}
		if to[0] == ':' {
			b = append(b, p.caseName(string(to))...)
			break
		}
		for _, c := range []byte(to) {
			if needPipeMap[c] == 'x' {
				b = append(b, '|')
				b = append(b, p.caseName(string(to))...)
				b = append(b, '|')
				break Top
			}
		}
		b = append(b, p.caseName(string(to))...)
	case String:
		if p.Readably {
			b = ojg.AppendJSONString(b, string(to), false)
		} else {
			b = append(b, '"')
			b = append(b, to...)
			b = append(b, '"')
		}
	case List:
		if len(to) == 0 {
			return append(b, p.caseName("nil")...)
		}
		if int(p.Level) <= level {
			return append(b, '#')
		}
		if p.Pretty {
			b = p.appendTree(b, p.createTree(to, 0), 0, 0)
		} else {
			l2 := level + 1
			b = append(b, '(')
			for i, element := range to {
				if 0 < i {
					b = append(b, ' ')
				}
				if int(p.Length) <= i {
					b = append(b, "..."...)
					break
				}
				b = p.Append(b, element, l2)
			}
			b = append(b, ')')
		}
	case *Array:
		if p.Array {
			obj = to.AsList()
			switch len(to.dims) {
			case 0:
				b = append(b, "#0A"...)
			case 1:
				b = append(b, '#')
			default:
				b = append(b, '#')
				b = p.Append(b, Fixnum(len(to.dims)), 0)
				b = append(b, 'A')
			}
			goto Top
		} else {
			switch len(to.dims) {
			case 0:
				b = append(b, "#<(ARRAY T NIL)>"...)
			default:
				b = append(b, "#<(ARRAY T ("...)
				for i, d := range to.dims {
					if 0 < i {
						b = append(b, ' ')
					}
					b = p.Append(b, Fixnum(d), 0)
				}
				b = append(b, "))>"...)
			}
		}
	case *Vector:
		if p.Array {
			if 0 < to.Length() {
				obj = to.AsList()
				b = append(b, '#')
				goto Top
			}
			b = append(b, '#', '(', ')')
		} else {
			b = append(b, "#<(VECTOR "...)
			b = p.Append(b, Fixnum(to.dims[0]), 0)
			b = append(b, ")>"...)
		}
	case Octets:
		if p.Array {
			if 0 < to.Length() {
				obj = to.AsList()
				b = append(b, '#')
				goto Top
			}
			b = append(b, '#', '(', ')')
		} else {
			b = append(b, "#<(VECTOR "...)
			b = p.Append(b, Fixnum(len(to)), 0)
			b = append(b, ")>"...)
		}
	case Tail:
		b = append(b, '.', ' ')
		obj = to.Value
		goto Top
	case *Lambda:
		if p.Lambda {
			list := make(List, 0, len(to.Forms)+2)
			list = append(list, Symbol("lambda"))
			args := make(List, 0, len(to.Doc.Args))
			for _, ad := range to.Doc.Args {
				args = append(args, Symbol(ad.Name))
			}
			list = append(list, args)
			list = append(list, to.Forms...)
			obj = list
			goto Top
		} else {
			b = append(b, "#<"...)
			b = p.Append(b, Symbol("function"), 0)
			b = append(b, " ("...)
			b = p.Append(b, Symbol("lambda"), 0)
			b = append(b, " ("...)
			for i, ad := range to.Doc.Args {
				if 0 < i {
					b = append(b, ' ')
				}
				b = append(b, ad.Name...)
			}
			b = append(b, ')')
			b = append(b, ") {"...)
			b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(to))), 16)
			b = append(b, "}>"...)
		}
	case SpecialSyntax:
		b = append(b, to.SpecialPrefix()...)
		obj = to.GetArgs()[0]
		goto Top
	case Funky:
		name := to.GetName()
		obj = append(List{Symbol(name)}, to.GetArgs()...)
		goto Top
	case *Package:
		b = append(b, "#<"...)
		b = append(b, p.caseName("package")...)
		b = append(b, ' ')
		b = append(b, to.Name...)
		b = append(b, '>')
	default:
		buf := to.Append(nil)
		if p.Readably && p.ReadablyError && bytes.HasPrefix(buf, []byte("#<")) {
			PanicPrintNotReadable(to, "%s can not be written readably", to)
		}
		b = append(b, buf...)
	}
	/* TBD
	if bytes.ContainsRune(b, '\n') {
		b = append(b, '\n')
	}
	*/
	return b
}

// ScopedUpdate updates the printer based on values of the values in the
// provided Scope.
func (p *Printer) ScopedUpdate(s *Scope) {
	if s != nil {
		for k, f := range map[string]func(v Object){
			"*print-ansi*":  func(v Object) { p.ANSI = v != nil },
			"*print-array*": func(v Object) { p.Array = v != nil },
			"*print-base*": func(v Object) {
				if base, ok := v.(Fixnum); ok && 2 <= base && base <= 36 {
					p.Base = uint(base)
				} else {
					PanicType("*print-base*", v, "fixnum between 2 and 36 inclusive")
				}
			},
			"*print-case*": func(v Object) {
				if v == nil {
					p.Case = nil
				} else {
					key, _ := v.(Symbol)
					key = Symbol(strings.ToLower(string(key)))
					switch key {
					case downcaseKey, upcaseKey, capitalizeKey:
						p.Case = key
					default:
						PanicType("*print-case*", v, ":downcase", ":upcase", ":capitalize")
					}
				}
			},
			"*print-circle*": func(v Object) { p.Circle = v != nil },
			"*print-escape*": func(v Object) { p.Escape = v != nil },
			"*print-gensym*": func(v Object) { p.Gensym = v != nil },
			"*print-length*": func(v Object) { p.Length = uintVarValue(v, "*print-length*") },
			"*print-level*":  func(v Object) { p.Level = uintVarValue(v, "*print-level*") },
			"*print-lines*":  func(v Object) { p.Lines = uintVarValue(v, "*print-lines*") },
			"*print-miser-width*": func(v Object) {
				if miserWidth, ok := v.(Fixnum); ok && 0 <= miserWidth {
					p.MiserWidth = uint(miserWidth)
				} else {
					PanicType("*print-miser-width*", v, "non-negative fixnum")
				}
			},
			"*print-pretty*":       func(v Object) { p.Pretty = v != nil },
			"*print-radix*":        func(v Object) { p.Radix = v != nil },
			"*print-readably*":     func(v Object) { p.Readably = v != nil },
			"*print-right-margin*": func(v Object) { p.RightMargin = uintVarValue(v, "*print-right-margin*") },
		} {
			if v, has := s.localGet(k); has {
				f(v)
			}
		}
	}
}

func uintVarValue(v Object, varName string) (vv uint) {
	if v == nil {
		vv = math.MaxInt
	} else if num, ok := v.(Fixnum); ok && 0 <= num {
		vv = uint(num)
	} else {
		PanicType(varName, v, "non-negative fixnum")
	}
	return
}

func (p *Printer) createTree(obj Object, level int) *node {
	n := node{value: obj}
Top:
	switch to := obj.(type) {
	case List:
		if int(p.Level) <= level {
			obj = Symbol("#")
			goto Top
		}
		if 0 < len(to) {
			l2 := level + 1
			n.size = 1 + len(to)
			for i, element := range to {
				if int(p.Length) <= i {
					n.elements = append(n.elements, &node{value: Symbol("..."), size: 3, buf: []byte("...")})
					n.size += 3
					break
				}
				if tail, ok := element.(Tail); ok { // cons
					n.elements = append(n.elements, &node{value: Symbol("."), size: 1, buf: []byte{'.'}})
					element = tail.Value
				}
				t := p.createTree(element, l2)
				n.elements = append(n.elements, t)
				n.size += t.size
			}
		} else {
			n.size = 2
		}
	case Symbol:
		n.buf = []byte(p.caseName(string(to)))
		n.size = len(n.buf)
	case SpecialSyntax:
		obj = to.GetArgs()[0]
		n.special = to.SpecialPrefix()
		n.funky = true
		goto Top
	case Funky:
		name := to.GetName()
		obj = append(List{Symbol(name)}, to.GetArgs()...)
		goto Top

	default:
		n.buf = p.Append([]byte{}, obj, 0)
		n.size = len(n.buf)
	}
	return &n
}

var spaces = []byte{'\n'}

func (p *Printer) appendTree(b []byte, n *node, offset, closes int) []byte {
	if 0 < len(n.special) {
		b = append(b, n.special...)
	}
	if 0 < len(n.buf) {
		return append(b, n.buf...)
	}
	b = append(b, '(')
	switch len(n.elements) {
	case 0:
		// nothing to do
	case 1:
		b = p.appendTree(b, n.elements[0], offset+1, closes+1)
	default:
		off := offset + 1
		t := 0
		if len(n.elements) == 2 {
			t = closes + 1
		}
		if off+n.elements[0].size+n.elements[1].size+t+1 <= int(p.RightMargin) {
			off += n.elements[0].size + 1
		}
		if len(spaces)-1 < off {
			spaces = append(spaces, bytes.Repeat([]byte{' '}, off-len(spaces)+1)...)
		}
		pos := offset + 1
		for i, e := range n.elements {
			t := 0
			if len(n.elements)-1 == i {
				t = closes + 1
			}
			if i == 0 {
				b = p.appendTree(b, e, off, t)
				pos += e.size
				continue
			}
			if pos+e.size+t+1 <= int(p.RightMargin) {
				b = append(b, ' ')
				b = p.appendTree(b, e, 0, t)
				pos += e.size + t + 1
			} else {
				if p.Lines < math.MaxInt && int(p.Lines)-1 <= bytes.Count(b, []byte{'\n'}) {
					b = append(b, " .."...)
					break
				}
				b = append(b, spaces[:off+1]...)
				b = p.appendTree(b, e, off, t)
				pos = off + e.size + 1
			}
		}
	}
	return append(b, ')')
}

func (p *Printer) caseName(name string) string {
	switch p.Case {
	case upcaseKey:
		name = strings.ToUpper(name)
	case downcaseKey:
		name = strings.ToLower(name)
	case capitalizeKey:
		name = strings.ToLower(name)
		rn := []rune(name)
		rn[0] = unicode.ToUpper(rn[0])
		name = string(rn)
	}
	return name
}

// get *print-ansi*
func getPrintANSI() (value Object) {
	if printer.ANSI {
		value = True
	}
	return
}

// set *print-ansi*
func setPrintANSI(value Object) {
	printer.ANSI = value != nil
}

// get *print-array*
func getPrintArray() (value Object) {
	if printer.Array {
		value = True
	}
	return
}

// set *print-array*
func setPrintArray(value Object) {
	printer.Array = value != nil
}

// get *print-base*
func getPrintBase() Object {
	return Fixnum(printer.Base)
}

// set *print-base*
func setPrintBase(value Object) {
	if base, ok := value.(Fixnum); ok && 2 <= base && base <= 36 {
		printer.Base = uint(base)
	} else {
		PanicType("*print-base*", value, "fixnum between 2 and 36 inclusive")
	}
}

// get *print-case*
func getPrintCase() Object {
	return printer.Case
}

// set *print-case*
func setPrintCase(value Object) {
	if value == nil {
		printer.Case = nil
	} else {
		key, _ := value.(Symbol)
		key = Symbol(strings.ToLower(string(key)))
		switch key {
		case downcaseKey, upcaseKey, capitalizeKey:
			printer.Case = key
		default:
			PanicType("*print-case*", value, ":downcase", ":upcase", ":capitalize")
		}
	}
}

// get *print-circle*
func getPrintCircle() (value Object) {
	if printer.Circle {
		value = True
	}
	return
}

// set *print-circle*
func setPrintCircle(value Object) {
	printer.Circle = value != nil
}

// get *print-escape*
func getPrintEscape() (value Object) {
	if printer.Escape {
		value = True
	}
	return
}

// set *print-escape*
func setPrintEscape(value Object) {
	printer.Escape = value != nil
}

// get *print-gensym*
func getPrintGensym() (value Object) {
	if printer.Gensym {
		value = True
	}
	return
}

// set *print-gensym*
func setPrintGensym(value Object) {
	printer.Gensym = value != nil
}

// get *print-lambda*
func getPrintLambda() (value Object) {
	if printer.Lambda {
		value = True
	}
	return
}

// set *print-lambda*
func setPrintLambda(value Object) {
	printer.Lambda = value != nil
}

// get *print-length*
func getPrintLength() Object {
	if printer.Length == math.MaxInt {
		return nil
	}
	return Fixnum(printer.Length)
}

// set *print-length*
func setPrintLength(value Object) {
	if value == nil {
		printer.Length = math.MaxInt
	} else if length, ok := value.(Fixnum); ok && 0 <= length {
		printer.Length = uint(length)
	} else {
		PanicType("*print-length*", value, "non-negative fixnum")
	}
}

// get *print-level*
func getPrintLevel() Object {
	if printer.Level == math.MaxInt {
		return nil
	}
	return Fixnum(printer.Level)
}

// set *print-level*
func setPrintLevel(value Object) {
	if value == nil {
		printer.Level = math.MaxInt
	} else if level, ok := value.(Fixnum); ok && 0 <= level {
		printer.Level = uint(level)
	} else {
		PanicType("*print-level*", value, "non-negative fixnum")
	}
}

// get *print-lines*
func getPrintLines() Object {
	if printer.Lines == math.MaxInt {
		return nil
	}
	return Fixnum(printer.Lines)
}

// set *print-lines*
func setPrintLines(value Object) {
	if value == nil {
		printer.Lines = math.MaxInt
	} else if lines, ok := value.(Fixnum); ok && 0 <= lines {
		printer.Lines = uint(lines)
	} else {
		PanicType("*print-lines*", value, "non-negative fixnum")
	}
}

// get *print-miser-width*
func getPrintMiserWidth() Object {
	return Fixnum(printer.MiserWidth)
}

// set *print-miser-width*
func setPrintMiserWidth(value Object) {
	if miserWidth, ok := value.(Fixnum); ok && 0 <= miserWidth {
		printer.MiserWidth = uint(miserWidth)
	} else {
		PanicType("*print-miser-width*", value, "non-negative fixnum")
	}
}

// get *print-pretty*
func getPrintPretty() (value Object) {
	if printer.Pretty {
		value = True
	}
	return
}

// set *print-pretty*
func setPrintPretty(value Object) {
	printer.Pretty = value != nil
}

// get *print-radix*
func getPrintRadix() (value Object) {
	if printer.Radix {
		value = True
	}
	return
}

// set *print-radix*
func setPrintRadix(value Object) {
	printer.Radix = value != nil
}

// get *print-readably*
func getPrintReadably() (value Object) {
	if printer.Readably {
		value = True
	}
	return
}

// set *print-readably*
func setPrintReadably(value Object) {
	printer.Readably = value != nil
}

// get *print-prec*
func getPrintPrec() Object {
	return Fixnum(printer.Prec)
}

// set *print-prec*
func setPrintPrec(value Object) {
	if prec, ok := value.(Fixnum); ok {
		printer.Prec = int(prec)
	} else {
		PanicType("*print-prec*", value, "fixnum greater that 0")
	}
}

// get *print-right-margin*
func getPrintRightMargin() Object {
	if printer.RightMargin == math.MaxInt {
		return nil
	}
	return Fixnum(printer.RightMargin)
}

// set *print-right-margin*
func setPrintRightMargin(value Object) {
	if value == nil {
		printer.RightMargin = math.MaxInt
	} else if rightMargin, ok := value.(Fixnum); ok && 0 <= rightMargin {
		printer.RightMargin = uint(rightMargin)
	} else {
		PanicType("*print-right-margin*", value, "non-negative fixnum")
	}
}

// Warn outputs a warning.
func Warn(format string, args ...any) {
	var b []byte
	if Interactive {
		if printer.ANSI {
			b = append(b, "\x1b[31mWarning: "...) // red
			b = append(b, fmt.Sprintf(format, args...)...)
			b = append(b, "\x1b[m\n"...)
			_, _ = StandardOutput.(io.Writer).Write(b)
		} else {
			b = append(b, "Warning: "...)
			b = append(b, fmt.Sprintf(format, args...)...)
			b = append(b, '\n')
			_, _ = StandardOutput.(io.Writer).Write(b)
		}
	} else {
		b = append(b, "Warning: "...)
		b = append(b, fmt.Sprintf(format, args...)...)
		b = append(b, '\n')
		_, _ = ErrorOutput.(io.Writer).Write(b)
	}
}

// AppendDoc appends text after formatting and converting _ and __ either to
// ANSI underline and bold or is not ANSI removing them.
func AppendDoc(b []byte, text string, indent, right int, ansi bool, firstIndent ...int) []byte {
	var (
		lastSpace int
		spaceCol  int
		col       = indent
		emp       int
		fs        bool // font style active
		ret       bool
	)
	if 0 < len(firstIndent) {
		b = append(b, indentSpaces[:firstIndent[0]]...)
	} else {
		b = append(b, indentSpaces[:indent]...)
	}
	ba := []byte(text)
	last := len(ba) - 1
	for i, c := range ba {
		if c == '_' {
			if ansi {
				emp++
			}
			continue
		}
		if 0 < emp {
			if fs {
				b = append(b, colorOff...)
				fs = false
			} else {
				if emp == 1 {
					b = append(b, underline...)
				} else {
					b = append(b, bold...)
				}
				fs = true
			}
			emp = 0
		}
		switch c {
		case ' ':
			if ret {
				b[len(b)-1] = '\n'
				b = append(b, indentSpaces[:indent]...)
				lastSpace = 0
				spaceCol = indent
				col = indent
				ret = false
			} else {
				lastSpace = len(b)
				spaceCol = col
				col++
				b = append(b, ' ')
			}
		case '\n':
			if ret {
				if 0 < len(b) && b[len(b)-1] != '\n' {
					b[len(b)-1] = '\n'
				}
				if i < last {
					b = append(b, indentSpaces[:indent]...)
				}
				lastSpace = 0
				spaceCol = indent
				col = indent
			} else {
				lastSpace = len(b)
				spaceCol = col
				if i < last {
					b = append(b, ' ')
				}
				col++
				ret = true
			}
		default:
			ret = false
			b = append(b, c)
			col++
		}
		if right < col && 0 < lastSpace {
			if lastSpace < len(b) {
				// Append indent to the end just as a way to expand b.
				b = append(b, indentSpaces[:indent]...)
				copy(b[lastSpace+1+indent:], b[lastSpace+1:])
				copy(b[lastSpace+1:], indentSpaces[:indent])
				b[lastSpace] = '\n'
			} else {
				b = append(b, indentSpaces[:indent]...)
				b = append(b, '\n')
			}
			col = indent + col - spaceCol
			lastSpace = 0
			spaceCol = 0
		}
	}
	if fs {
		b = append(b, colorOff...)
	}
	return b
}
