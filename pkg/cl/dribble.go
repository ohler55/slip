// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Dribble{Function: slip.Function{Name: "dribble", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "dribble",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "pathname",
					Type: "string",
					Text: "The path to the file to write to.",
				},
			},
			Return: "nil",
			Text: `__dribble__ starts dribbling _*standard-input*_ and _*standard-output*_ to a file
opened at _filepath_ or is _filepath_ is not provided the previous dribble file is closed and
_*standard-input*_ and _*standard-output*_ revert to their previous values.`,
			Examples: []string{
				`(dribble "debug") ;; start dribbling to a debug file`,
				`(format t "check it out~%") ;; writes "> check it out\n" to the debug file`,
				`(dribble) ;; reverts to the previous *standard-input* and *standard-output* value`,
			},
		}, &slip.CLPkg)
}

// Dribble represents the dribble function.
type Dribble struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Dribble) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 1)
	if 0 < len(args) {
		f.start(s, args[0], depth)
	} else {
		f.stop()
	}
	return nil
}

func (f *Dribble) start(s *slip.Scope, filepath slip.Object, depth int) {
	fp, ok := filepath.(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "filepath", filepath, "string")
	}
	file, err := os.Create(string(fp))
	if err != nil {
		slip.FilePanic(s, depth, filepath, "%s", err)
	}
	in := slip.CurrentPackage.JustGet("*standard-input*")
	if d, ok := in.(*Dribbler); ok {
		_ = d.file.Close()
		d.file = file
	} else {
		out := slip.CurrentPackage.JustGet("*standard-output*")
		d = &Dribbler{
			origIn:  in,
			input:   in.(io.Reader),
			origOut: out,
			output:  out.(io.Writer),
			file:    file,
		}
		_ = slip.CurrentPackage.Set("*standard-input*", d)
		_ = slip.CurrentPackage.Set("*standard-output*", d)
	}
}

func (f *Dribble) stop() {
	if d, ok := slip.CurrentPackage.JustGet("*standard-input*").(*Dribbler); ok {
		_ = slip.CurrentPackage.Set("*standard-input*", d.origIn)
		_ = slip.CurrentPackage.Set("*standard-output*", d.origOut)
		_ = d.file.Close()
	}
}
