// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Time{Function: slip.Function{Name: "time", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "time",
			Args: []*slip.DocArg{
				{
					Name: "form",
					Type: "form",
					Text: "The form to evaluate.",
				},
			},
			Return: "nil",
			Text:   `__time__ evaluates the _form_ and prints timing information to _*trace-output*_.`,
			Examples: []string{
				`(time (+ 1 2)) => 3 ;; prints:`,
				`Evaluation took:`,
				`  0.000064042 seconds of real time`,
			},
		}, &slip.CLPkg)
}

// Time represents the time function.
type Time struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Time) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	t0 := time.Now()
	result = slip.EvalArg(s, args, 0, depth+1)
	dur := time.Since(t0)
	w := s.Get("*trace-output*").(io.Writer)
	fmt.Fprintf(w, "Evaluation took:\n  %d.%09d seconds of real time\n", dur/time.Second, dur%time.Second)
	return
}
