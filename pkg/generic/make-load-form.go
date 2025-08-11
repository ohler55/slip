// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"sort"

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
	slip.ArgCountCheck(f, args, 1, 1)

	return ObjectLoadForm(args[0], true)
}

// ObjectLoadForm
func ObjectLoadForm(obj slip.Object, follow bool) (form slip.List) {
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
	case slip.DefLister:
		form = to.DefList()
	case slip.Instance:
		form = instanceLoadForm(to)
	default:
		slip.PanicType("object", obj, "class", "condition", "standard-object", "instance", "function")
	}

	// TBD handle built in
	// add to switch
	return
}

func instanceLoadForm(obj slip.Instance) (form slip.List) {
	names := obj.SlotNames()
	sort.Strings(names)
	form = slip.List{
		slip.Symbol("let"),
		slip.List{
			slip.List{
				slip.Symbol("inst"),
				slip.List{
					slip.Symbol("make-instance"),
					slip.List{
						slip.Symbol("quote"),
						slip.Symbol(obj.Class().Name()),
					},
				},
			},
		},
	}
	for _, name := range names {
		iv, _ := obj.SlotValue(slip.Symbol(name))
		form = append(form,
			slip.List{
				slip.Symbol("setf"),
				slip.List{
					slip.Symbol("slot-value"),
					slip.Symbol("inst"),
					slip.List{
						slip.Symbol("quote"),
						slip.Symbol(name),
					},
				},
				iv, // TBD handle more complex values
			},
		)
	}
	form = append(form, slip.Symbol("inst"))

	return
}
