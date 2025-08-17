// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defRemoveMethod() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RemoveMethod{Function: slip.Function{Name: "remove-method", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "remove-method",
			Args: []*slip.DocArg{
				{
					Name: "generic-function",
					Type: "generic-function",
					Text: `A generic-function or designator.`,
				},
				{
					Name: "method",
					Type: "methid",
					Text: `A method of the generic function.`,
				},
			},
			Return: "generic-function",
			Text:   `__remove-method__ removes a method of the generic-function_.`,
			Examples: []string{
				`(defgeneric quux (x y) (:method :before ((x real) (y real)) (print "before"))`,
				`(remove-method 'quux (find-method 'quux '(:before) '(real))) => #<generic-function quux>`,
				// TBD show modified quux
			},
		}, &Pkg)
}

// RemoveMethod represents the remove-method function.
type RemoveMethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *RemoveMethod) Call(s *slip.Scope, args slip.List, depth int) (meth slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	var aux *Aux
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case slip.Symbol:
		a0 = slip.FindFunc(string(ta))
		goto top
	case *slip.FuncInfo:
		aux, _ = ta.Aux.(*Aux)
	}
	if aux == nil {
		slip.PanicType("generic-function", args[0], "symbol", "generic-function")
	}
	meth, ok := args[1].(*slip.Method)
	if !ok {
		slip.PanicType("method", args[1], "method")
	}

	// TBD look for a match on qualifiers and specializers
	//  specializers to form a key
	//  find aux method
	//  remove qualifier caller
	//  if no more then remove key entry as well

	return args[0]
}
