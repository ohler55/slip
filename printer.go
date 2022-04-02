// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
	"unicode"
)

const (
	upcaseKey     = Symbol(":upcase")
	downcaseKey   = Symbol(":downcase")
	capitalizeKey = Symbol(":capitalize")
)

type node struct {
	value    Object
	elements []*node
	size     int
	quote    bool
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

	// RightMargin *print-right-margin*.
	RightMargin uint
}

var (
	printer = Printer{
		ANSI:        true,
		Array:       false,
		Base:        10,
		Case:        downcaseKey,
		Circle:      false,
		Escape:      true,
		Gensym:      true,
		Length:      math.MaxInt,
		Level:       math.MaxInt,
		Lines:       math.MaxInt,
		MiserWidth:  0,
		Pretty:      true,
		Radix:       false,
		Readably:    false,
		RightMargin: 120,
	}
)

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
		panic(err)
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
			// The Common Lisp docs are a big vague and leave options open for
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
	case *Ratio:
		if to.Denominator == 1 {
			obj = Fixnum(to.Numerator)
			goto Top
		}
		if p.Radix {
			switch p.Base {
			case 2:
				b = append(b, "#b"...)
				b = strconv.AppendInt(b, to.Numerator, int(p.Base))
				b = append(b, '/')
				b = strconv.AppendUint(b, to.Denominator, int(p.Base))
			case 8:
				b = append(b, "#o"...)
				b = strconv.AppendInt(b, to.Numerator, int(p.Base))
				b = append(b, '/')
				b = strconv.AppendUint(b, to.Denominator, int(p.Base))
			case 16:
				b = append(b, "#x"...)
				b = strconv.AppendInt(b, to.Numerator, int(p.Base))
				b = append(b, '/')
				b = strconv.AppendUint(b, to.Denominator, int(p.Base))
			default:
				b = append(b, '#')
				b = strconv.AppendInt(b, int64(p.Base), 10)
				b = append(b, 'r')
				b = strconv.AppendInt(b, to.Numerator, int(p.Base))
				b = append(b, '/')
				b = strconv.AppendUint(b, to.Denominator, int(p.Base))
			}
		} else {
			b = strconv.AppendInt(b, to.Numerator, int(p.Base))
			b = append(b, '/')
			b = strconv.AppendUint(b, to.Denominator, int(p.Base))
		}
	case Symbol:
		b = append(b, p.caseName(string(to))...)
	case List:
		if int(p.Level) <= level {
			return append(b, '#')
		}
		if p.Pretty {
			b = p.appendTree(b, p.createTree(to, 0), 0, 0)
		} else {
			l2 := level + 1
			b = append(b, '(')
			for i := 0; i < len(to); i++ {
				element := to[len(to)-i-1]
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
			case 1:
				b = append(b, "#<(VECTOR "...)
				b = p.Append(b, Fixnum(to.dims[0]), 0)
				b = append(b, ")>"...)
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
	case Funky:
		name := to.GetName()
		args := to.GetArgs()
		if name == "quote" {
			b = append(b, '\'')
			obj = args[0]
		} else {
			// This double assignment is just stupid but it silences the linter.
			args = append(args, Symbol(name))
			obj = args
		}
		goto Top
	default:
		b = to.Append(b)
		if p.Readably && bytes.HasPrefix(b, []byte("#<")) {
			panic(fmt.Sprintf("%s can not be written readably", to))
		}
	}
	if bytes.ContainsRune(b, '\n') {
		b = append(b, '\n')
	}
	return b
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
			n.elements = make([]*node, len(to))
			for i := 0; i < len(to); i++ {
				element := to[len(to)-i-1]
				if int(p.Length) <= i {
					n.elements[i] = &node{value: Symbol("..."), size: 3, buf: []byte("...")}
					n.size += 3
					break
				}
				n.elements[i] = p.createTree(element, l2)
				n.size += n.elements[i].size
			}
		} else {
			n.size = 2
		}
	case Symbol:
		n.buf = []byte(p.caseName(string(to)))
		n.size = len(n.buf)
	case Cons:
		obj = List{to.Car(), Symbol("."), to.Cdr()}
		goto Top

	case Funky:
		name := to.GetName()
		args := to.GetArgs()
		if name == "quote" {
			obj = args[0]
			n.quote = true
			n.funky = true
		} else {
			// This double assignment is just stupid but it silences the linter.
			args = append(args, Symbol(name))
			obj = args
		}
		goto Top
	default:
		n.buf = p.Append([]byte{}, obj, 0)
		n.size = len(n.buf)
	}
	return &n
}

var spaces = []byte{'\n'}

func (p *Printer) appendTree(b []byte, n *node, offset, closes int) []byte {
	if n.quote {
		b = append(b, '\'')
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

// Warning outputs a warning.
func Warning(format string, args ...interface{}) {
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
