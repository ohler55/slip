// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"io"
	"math"
	"strings"
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

	// Case backs *print-case*.
	Case Symbol

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
	if _, err := StandardOutput.(io.Writer).Write(printer.Append([]byte{}, obj)); err != nil {
		panic(err)
	}
}

// Append an Object to a byte array using the global print variables.
func Append(b []byte, obj Object) []byte {
	return printer.Append(b, obj)
}

// Write an Object to *standard-output* using the Printer variables.
func (p *Printer) Write(obj Object) {
	if _, err := StandardOutput.(io.Writer).Write(p.Append([]byte{}, obj)); err != nil {
		panic(err)
	}
}

// Append an Object to a byte array using the Printer variables.
func (p *Printer) Append(b []byte, obj Object) []byte {
	if obj == nil {
		return append(b, caseName("nil")...)
	}

	// TBD handle fancy stuff like level and lines

	return obj.Append(b)
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
	key, _ := value.(Symbol)
	key = Symbol(strings.ToLower(string(key)))
	switch key {
	case downcaseKey, upcaseKey, capitalizeKey:
		printer.Case = key
	default:
		PanicType("*print-case*", value, ":downcase", ":upcase", ":capitalize")
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
	if printer.Level == math.MaxInt {
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
	return Fixnum(printer.RightMargin)
}

// set *print-right-margin*
func setPrintRightMargin(value Object) {
	if rightMargin, ok := value.(Fixnum); ok && 0 <= rightMargin {
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
			b = append(b, "\x1b[m"...)
			_, _ = StandardOutput.(io.Writer).Write(b)
		} else {
			b = append(b, "Warning: "...)
			b = append(b, fmt.Sprintf(format, args...)...)
			_, _ = StandardOutput.(io.Writer).Write(b)
		}
	} else {
		b = append(b, "Warning: "...)
		b = append(b, fmt.Sprintf(format, args...)...)
		_, _ = ErrorOutput.(io.Writer).Write(b)
	}
}
