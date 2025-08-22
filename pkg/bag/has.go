// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Has{Function: slip.Function{Name: "bag-has", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-has",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to has a value from.",
				},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to determine if a value exists.
The path must follow the JSONPath format.`,
				},
			},
			Return: "bag",
			Text: `__bag-has__ returns true if a _value_ at the location described by _path_ exists.

This is the same as the _:has_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`,
				`(bag-has bag "a") => t`,
			},
		}, &Pkg)
}

// Has represents the has function.
type Has struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Has) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}
	return hasBag(s, obj, args[1], depth)
}

func hasBag(s *slip.Scope, obj *flavors.Instance, path slip.Object, depth int) slip.Object {
	var x jp.Expr
	switch p := path.(type) {
	case nil:
	case slip.String:
		x = jp.MustParseString(string(p))
	case Path:
		x = jp.Expr(p)
	default:
		slip.TypePanic(s, depth, "path", p, "string")
	}
	if x == nil || x.Has(obj.Any) {
		return slip.True
	}
	return nil
}
