// Copyright (c) 2023, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Remove{Function: slip.Function{Name: "bag-remove", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-remove",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to remove a value from.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to remove.
The path must follow the JSONPath format.`,
				},
			},
			Return: "bag",
			Text: `__bag-remove__ removes the value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is removed and the bag value set to _nil_.

This is the same as the _:remove_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}"))`,
				`(bag-remove bag "a") => #<bag-flavor 12345> ;; content is now {b:8}`,
			},
		}, &Pkg)
}

// Remove represents the remove function.
type Remove struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Remove) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}
	if 1 < len(args) {
		removeBag(s, obj, args[1], depth)
	} else {
		removeBag(s, obj, nil, depth)
	}
	return obj
}

func removeBag(s *slip.Scope, obj *flavors.Instance, path slip.Object, depth int) {
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
	if x == nil {
		obj.Any = nil
	} else {
		obj.Any = x.MustRemove(obj.Any)
	}
}
