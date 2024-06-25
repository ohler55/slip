// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeList{Function: slip.Function{Name: "make-list", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-list",
			Args: []*slip.DocArg{
				{
					Name: "size",
					Type: "fixnum",
					Text: "The size of the list to make.",
				},
				{Name: "&key"},
				{
					Name: "initial-element",
					Type: "object",
					Text: `The initial element for each element of the list.`,
				},
			},
			Return: "list",
			Text: `__make-list__ makes a _list_ with _size_ _initial-element_s. The default
_initial-element_ is _nil_.`,
			Examples: []string{
				"(make-list 3 :initial-element 'slip) => (slip slip slip)",
			},
		}, &slip.CLPkg)
}

// MakeList represents the make-list function.
type MakeList struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeList) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	size, ok := args[0].(slip.Fixnum)
	if !ok || size < 0 {
		slip.PanicType("size", args[0], "fixnum")
	}
	ie, _ := slip.GetArgsKeyValue(args[1:], slip.Symbol(":initial-element"))
	list := make(slip.List, int(size))
	for i := len(list) - 1; 0 <= i; i-- {
		list[i] = ie
	}
	return list
}
