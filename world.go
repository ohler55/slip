// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"os"
)

type World struct {
	Scope
	Out       func(out []byte)
	Term      bool
	constants map[string]Object

	funcGets map[string]func(*World) Object
	funcSets map[string]func(*World, Object)

	print formatter
}

// NewWorld creates a new World.
func NewWorld() *World {
	wd, _ := os.Getwd()
	w := World{
		Scope: Scope{
			returnValue: undef,
			vars: map[string]Object{
				"*default-pathname-defaults*": String(wd),
				"*error-output*":              (*FileStream)(os.Stderr),
				"*standard-input*":            (*FileStream)(os.Stdin),
				"*standard-output*":           (*FileStream)(os.Stdout),
			},
		},
		funcGets: map[string]func(*World) Object{
			"*print-ansi*":         ansiGet,
			"*print-array*":        arrayGet,
			"*print-base*":         baseGet,
			"*print-case*":         caseGet,
			"*print-circle*":       circleGet,
			"*print-escape*":       escapeGet,
			"*print-gensym*":       gensymGet,
			"*print-length*":       lengthGet,
			"*print-level*":        levelGet,
			"*print-lines*":        linesGet,
			"*print-miser-width*":  miserWidthGet,
			"*print-pretty*":       prettyGet,
			"*print-radix*":        radixGet,
			"*print-readably*":     readablyGet,
			"*print-right-margin*": rightMarginGet,
		},
		funcSets: map[string]func(*World, Object){
			"*print-ansi*":         ansiSet,
			"*print-array*":        arraySet,
			"*print-base*":         baseSet,
			"*print-case*":         caseSet,
			"*print-circle*":       circleSet,
			"*print-escape*":       escapeSet,
			"*print-gensym*":       gensymSet,
			"*print-length*":       lengthSet,
			"*print-level*":        levelSet,
			"*print-lines*":        linesSet,
			"*print-miser-width*":  miserWidthSet,
			"*print-pretty*":       prettySet,
			"*print-radix*":        radixSet,
			"*print-readably*":     readablySet,
			"*print-right-margin*": rightMarginSet,
		},
		Out:       func(out []byte) { fmt.Print(string(out)) },
		constants: map[string]Object{},
	}
	w.world = &w

	return &w
}

func (w *World) Print(obj Object) {
	w.Out(w.Append([]byte{}, obj))
}

func (w *World) Append(b []byte, obj Object) []byte {

	// TBD

	return b
}

func (w *World) get(name string) (Object, bool) {
	if value, has := w.vars[name]; has {
		return value, true
	}
	if value, has := w.constants[name]; has {
		return value, true
	}
	if value, has := constantValues[name]; has {
		return value, true
	}
	if f, has := w.funcGets[name]; has {
		return f(w), true
	}
	return nil, false
}

func (w *World) has(name string) bool {
	if _, has := w.vars[name]; has {
		return true
	}
	if _, has := w.constants[name]; has {
		return true
	}
	if _, has := constantValues[name]; has {
		return true
	}
	if _, has := w.funcGets[name]; has {
		return true
	}
	return false
}

func (w *World) set(name string, value Object) {
	if f, has := w.funcSets[name]; has {
		f(w, value)
		return
	}
	w.vars[name] = value
}
