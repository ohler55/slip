// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "strings"

const (
	upcaseKey     = Symbol(":upcase")
	downcaseKey   = Symbol(":downcase")
	capitalizeKey = Symbol(":capitalize")
)

type formatter struct {
	ansi        bool   // *print-ansi*
	array       bool   // *print-array*
	base        uint   // *print-base*
	symCase     Symbol // *print-case*
	circle      bool   // *print-circle*
	escape      bool   // *print-escape*
	gensym      bool   // *print-gensym*
	length      uint   // *print-length*
	level       uint   // *print-level*
	lines       uint   // *print-lines*
	miserWidth  uint   // *print-miser-width*
	pretty      bool   // *print-pretty*
	radix       bool   // *print-radix*
	readably    bool   // *print-readably*
	rightMargin uint   // *print-right-margin*
}

func (f *formatter) appendObject(b []byte, obj Object, level int) []byte {

	// TBD

	return b
}

// get *print-ansi*
func ansiGet(w *World) (value Object) {
	if w.print.ansi {
		value = True
	}
	return
}

// set *print-ansi*
func ansiSet(w *World, value Object) {
	w.print.ansi = value != nil
}

// get *print-array*
func arrayGet(w *World) (value Object) {
	if w.print.array {
		value = True
	}
	return
}

// set *print-array*
func arraySet(w *World, value Object) {
	w.print.array = value != nil
}

// get *print-base*
func baseGet(w *World) Object {
	return Fixnum(w.print.base)
}

// set *print-base*
func baseSet(w *World, value Object) {
	if base, ok := value.(Fixnum); ok && 2 <= base && base <= 36 {
		w.print.base = uint(base)
	} else {
		PanicType("*print-base*", value, "fixnum between 2 and 36 inclusive")
	}
}

// get *print-case*
func caseGet(w *World) Object {
	return w.print.symCase
}

// set *print-case*
func caseSet(w *World, value Object) {
	key, _ := value.(Symbol)
	key = Symbol(strings.ToLower(string(key)))
	switch key {
	case downcaseKey, upcaseKey, capitalizeKey:
		w.print.symCase = key
	default:
		PanicType("*print-case*", value, ":downcase", ":upcase", ":capitalize")
	}
}

// get *print-circle*
func circleGet(w *World) (value Object) {
	if w.print.circle {
		value = True
	}
	return
}

// set *print-circle*
func circleSet(w *World, value Object) {
	w.print.circle = value != nil
}

// get *print-escape*
func escapeGet(w *World) (value Object) {
	if w.print.escape {
		value = True
	}
	return
}

// set *print-escape*
func escapeSet(w *World, value Object) {
	w.print.escape = value != nil
}

// get *print-gensym*
func gensymGet(w *World) (value Object) {
	if w.print.gensym {
		value = True
	}
	return
}

// set *print-gensym*
func gensymSet(w *World, value Object) {
	w.print.gensym = value != nil
}

// get *print-length*
func lengthGet(w *World) Object {
	return Fixnum(w.print.length)
}

// set *print-length*
func lengthSet(w *World, value Object) {
	if length, ok := value.(Fixnum); ok && 0 <= length {
		w.print.length = uint(length)
	} else {
		PanicType("*print-length*", value, "non-negative fixnum")
	}
}

// get *print-level*
func levelGet(w *World) Object {
	return Fixnum(w.print.level)
}

// set *print-level*
func levelSet(w *World, value Object) {
	if level, ok := value.(Fixnum); ok && 0 <= level {
		w.print.level = uint(level)
	} else {
		PanicType("*print-level*", value, "non-negative fixnum")
	}
}

// get *print-lines*
func linesGet(w *World) Object {
	return Fixnum(w.print.lines)
}

// set *print-lines*
func linesSet(w *World, value Object) {
	if lines, ok := value.(Fixnum); ok && 0 <= lines {
		w.print.lines = uint(lines)
	} else {
		PanicType("*print-lines*", value, "non-negative fixnum")
	}
}

// get *print-miser-width*
func miserWidthGet(w *World) Object {
	return Fixnum(w.print.miserWidth)
}

// set *print-miser-width*
func miserWidthSet(w *World, value Object) {
	if miserWidth, ok := value.(Fixnum); ok && 0 <= miserWidth {
		w.print.miserWidth = uint(miserWidth)
	} else {
		PanicType("*print-miser-width*", value, "non-negative fixnum")
	}
}

// get *print-pretty*
func prettyGet(w *World) (value Object) {
	if w.print.pretty {
		value = True
	}
	return
}

// set *print-pretty*
func prettySet(w *World, value Object) {
	w.print.pretty = value != nil
}

// get *print-radix*
func radixGet(w *World) (value Object) {
	if w.print.radix {
		value = True
	}
	return
}

// set *print-radix*
func radixSet(w *World, value Object) {
	w.print.radix = value != nil
}

// get *print-readably*
func readablyGet(w *World) (value Object) {
	if w.print.readably {
		value = True
	}
	return
}

// set *print-readably*
func readablySet(w *World, value Object) {
	w.print.readably = value != nil
}

// get *print-right-margin*
func rightMarginGet(w *World) Object {
	return Fixnum(w.print.rightMargin)
}

// set *print-right-margin*
func rightMarginSet(w *World, value Object) {
	if rightMargin, ok := value.(Fixnum); ok && 0 <= rightMargin {
		w.print.rightMargin = uint(rightMargin)
	} else {
		PanicType("*print-right-margin*", value, "non-negative fixnum")
	}
}
