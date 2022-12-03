// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Pathp{Function: slip.Function{Name: "bag-path-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-path-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to check whether it is a _bag-path_ or not.",
				},
			},
			Return: "boolean",
			Text:   `__bag-path-p__ returns _t_ if _object- is a _bag-path_ and _nil_ otherwise.`,
			Examples: []string{
				`(bag-path-p (make-bag-path "a.b")) => t`,
			},
		}, &slip.CLPkg)
}

// Pathp represents the pathp function.
type Pathp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Pathp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(Path); ok {
		return slip.True
	}
	return nil
}
