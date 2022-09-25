// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Panic{Function: slip.Function{Name: "panic", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "panic",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "object",
					Text: "The value throw or raise.",
				},
			},
			Text: `__panic__ raises an error which can be caught with a __recover__ function. It never returns.`,
			Examples: []string{
				`(panic "Something is broken") => ;; throws "Something is broken"`,
				"(panic 'a => ;; throws a",
			},
		}, &Pkg)
}

// Panic represents the panic function.
type Panic struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Panic) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	panic(args[0])
}
