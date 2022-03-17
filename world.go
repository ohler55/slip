// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"os"
)

type World struct {
	Scope
	Out       func(out string)
	Term      bool
	constants map[string]Object
	// map of pointers to data fields
	funcGets map[string]func(*World, Symbol)
	funcSets map[string]func(*World, Symbol, Object)

	// TBD Printer    Printer
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
		funcGets: map[string]func(*World, Symbol){
			// TBD functions that take a world and symbol, maybe one for get and one for set
			"*print-ansi*":         nil,
			"*print-array*":        nil,
			"*print-base*":         nil,
			"*print-case*":         nil,
			"*print-circle*":       nil,
			"*print-escape*":       nil,
			"*print-gensym*":       nil,
			"*print-length*":       nil,
			"*print-level*":        nil,
			"*print-lines*":        nil,
			"*print-miser-width*":  nil,
			"*print-pretty*":       nil,
			"*print-radix*":        nil,
			"*print-readably*":     nil,
			"*print-right-margin*": nil,
		},
		funcSets: map[string]func(*World, Symbol, Object){
			// TBD functions that take a world and symbol, maybe one for get and one for set
			"*print-ansi*":         nil,
			"*print-array*":        nil,
			"*print-base*":         nil,
			"*print-case*":         nil,
			"*print-circle*":       nil,
			"*print-escape*":       nil,
			"*print-gensym*":       nil,
			"*print-length*":       nil,
			"*print-level*":        nil,
			"*print-lines*":        nil,
			"*print-miser-width*":  nil,
			"*print-pretty*":       nil,
			"*print-radix*":        nil,
			"*print-readably*":     nil,
			"*print-right-margin*": nil,
		},
		Out:       func(out string) { fmt.Print(out) },
		constants: map[string]Object{},
	}
	w.world = &w

	return &w
}
