// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cddr{Function: slip.Function{Name: "cddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the values from.",
				},
			},
			Return: "list",
			Text:   `__cddr__ returns the (cdr (cdr arg)) of _arg_.`,
			Examples: []string{
				"(cddr nil) => nil",
				"(cddr '(a b c d) => (c d)",
				"(cddr '(a b)) => nil",
			},
		}, &slip.CLPkg)
}

// Cddr represents the cddr function.
type Cddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return cddGet(f, 2, args)
}

func cddGet(f slip.Object, n int, args slip.List) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
top:
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.Cons:
		a = list.Cdr()
		n--
		if 0 < n {
			goto top
		}
		result = a
	case slip.List:
		if n < len(list) {
			result = list[:len(list)-n]
		}
	default:
		slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), args[0], "cons", "list")
	}
	return
}
