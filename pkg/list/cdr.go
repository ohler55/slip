// Copyright (c) 2022, Peter Ohler, All rights reserved.

package list

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdr{Function: slip.Function{Name: "cdr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take all but the first element of.",
				},
			},
			Return: "list|object",
			Text: `returns the _cdr_ if _arg_ is a _cons_, all but the first element if _arg_ is a _list_, and
_nil_ if _arg_ is _nil_ or an empty _list_.`,
			Examples: []string{
				"(cdr nil) => nil",
				"(cdr '(a . b) => b",
				"(cdr '(a b c)) => (b c)",
			},
		}, &slip.CLPkg)
}

// Cdr represents the cdr function.
type Cdr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
Retry:
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.Cons:
		result = list.Cdr()
	case slip.List:
		if 0 < len(list) {
			result = list[:len(list)-1]
		}
	case slip.Values:
		a = list.First()
		goto Retry
	default:
		slip.PanicType("argument to cdr", list, "cons", "list")
	}
	return
}
