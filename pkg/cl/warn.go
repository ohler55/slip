// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Warn{Function: slip.Function{Name: "warn", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "warn",
			Args: []*slip.DocArg{
				{
					Name: "datum",
					Type: "symbol|string",
					Text: "The type name of a warning if a symbol and a format control string if a string.",
				},
				{Name: "&rest"},
				{
					Name: "arguments",
					Type: "object",
					Text: `The supporting arguments according to the datum type.`,
				},
			},
			Return: "nil",
			Text: `__warn__ returns _nil_ after writing the warning to _*error-output*_.
If _datum_ is a string then it is assumed to be a format-control for a _simple-warning_ and the
_arguments_ are the format-arguments for the warning. If _datum_ is a _symbol_ then it must be
a designator for a warning class and the _arguments_ are then read as an initialization list
of key value pairs.`,
			Examples: []string{
				`(warn "numbers ~D and ~D" 2 3) => nil # output: numbers 2 and 3`,
				`(warn 'warning :message "danger") => nil # output: danger`,
			},
		}, &slip.CLPkg)
}

// Warn represents the warn function.
type Warn struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Warn) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	var cond slip.Condition
	switch ta := args[0].(type) {
	case slip.Symbol:
		cond = slip.MakeCondition(string(ta), args[1:])
		if _, ok := cond.(slip.Warning); !ok {
			slip.NewPanic("%s does not designate a condition class.", ta)
		}
	case slip.String:
		cond = NewSimpleWarning(s, string(ta), args[1:]...)
	default:
		slip.PanicType("datum", ta, "symbol", "string")
	}
	if _, err := fmt.Fprintf(slip.ErrorOutput.(io.Writer), "Warning: %s\n", cond.Error()); err != nil {
		ss, _ := slip.StandardOutput.(slip.Stream)
		slip.PanicStream(ss, "warn write failed. %s", err)
	}
	return nil
}
