// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defMakeLoadForm() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeLoadForm{Function: slip.Function{Name: "make-load-form", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-load-form",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "class|condition|standard-object|instance|function",
					Text: `An object to build a create form for.`,
				},
			},
			Return: "form",
			Text: `__make-load-form__ returns a form that can be used to create the _object_.
If it is not possible to create a form that creates the _object_ a type error is raised.`,
			Examples: []string{
				`(defun quux (x) (1+ x))`,
				`(make-load-form 'quux) => (defun quux (x) (1+ x))`,
			},
		}, &Pkg)
}

// MakeLoadForm represents the make-load-form function.
type MakeLoadForm struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *MakeLoadForm) Call(s *slip.Scope, args slip.List, depth int) (form slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	return ObjectLoadForm(args[0], true)
}

// ObjectLoadForm returns the load form for and object.
func ObjectLoadForm(obj slip.Object, follow bool) (form slip.Object) {
Top:
	switch to := obj.(type) {
	case slip.Symbol:
		obj = nil
		if fi := slip.FindFunc(string(to)); fi != nil {
			obj = fi
		} else if class := slip.FindClass(string(to)); class != nil {
			obj = class
		} else if v, has := slip.GetVar(to); has {
			obj = v
		}
		goto Top
	case slip.Instance:
		form = slip.InstanceLoadForm(to)
	case slip.LoadFormer:
		form = to.LoadForm()
	default:
		slip.PrintNotReadablePanic(slip.NewScope(), 0, to, "Can not make a load form for %s.", to)
	}
	return
}
