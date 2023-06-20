// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AssertPanic{Function: slip.Function{Name: "assert-panic", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "assert-panic",
			Args: []*slip.DocArg{
				{
					Name: "form",
					Type: "form",
					Text: "The form to evaluate.",
				},
				{Name: "&optional"},
				{
					Name: "message",
					Type: "string",
					Text: "An additional message to append to an error if generated.",
				},
			},
			Return: "nil",
			Text:   `__assert-panic__ panics if _form_ does nat panic.`,
			Examples: []string{
				`(assert-panic (lambda () (panic 'done))) => nil`,
			},
		}, &slip.CLPkg)
}

// AssertPanic represents the assert-panic function.
type AssertPanic struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AssertPanic) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	if f.notPanic(s, args, depth) == nil {
		return nil
	}
	var (
		ansi = s.Get("*print-ansi*") != nil
		b    []byte
	)
	if ansi {
		b = append(b, ansiNormal...)
	}
	b = append(b, "expect: panic\nactual: "...)
	if ansi {
		b = append(b, ansiRed...)
	}
	b = append(b, "did not panic\n"...)
	if 1 < len(args) {
		if str, ok := args[1].(slip.String); ok {
			b = append(b, str...)
		} else {
			b = slip.ObjectAppend(b, args[1])
		}
		b = append(b, '\n')
	}
	panic(string(b))
}

func (f *AssertPanic) notPanic(s *slip.Scope, args slip.List, depth int) (err error) {
	defer func() {
		if rec := recover(); rec == nil {
			err = fmt.Errorf("did not panic")
		}
	}()
	result := slip.EvalArg(s, args, 0, depth+1)
	s.Eval(result, depth+1)

	return
}
