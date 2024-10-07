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
			f := Get{Function: slip.Function{Name: "bag-get", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-get",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to get a value from.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to get the _value_ from.
The path must follow the JSONPath format.`,
				},
				{
					Name: "as-bag",
					Type: "boolean",
					Text: `If not nil then the returned value is a _bag_ otherwise a new LISP value is returned.`,
				},
			},
			Return: "bag",
			Text: `__bag-get__ gets a _value_ at the location described by _path_.
If no _path_ is provided the entire contents of the bag is returned.

This is the same as the _:get_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`,
				`(bag-get bag "a") => 3`,
			},
		}, &Pkg)
}

// Get represents the get function.
type Get struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Get) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 3 < len(args) {
		slip.PanicArgCount(f, 1, 3)
	}
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.PanicType("bag", args[0], "bag")
	}
	switch len(args) {
	case 1:
		result = getBag(obj, nil, false)
	case 2:
		result = getBag(obj, args[1], false)
	case 3:
		result = getBag(obj, args[1], args[2] != nil)
	}
	return
}

func getBag(obj *flavors.Instance, path slip.Object, asBag bool) slip.Object {
	var x jp.Expr
	switch p := path.(type) {
	case nil:
	case slip.String:
		x = jp.MustParseString(string(p))
	case Path:
		x = jp.Expr(p)
	default:
		slip.PanicType("path", p, "string", "bag-path")
	}
	var value any
	if x == nil {
		value = obj.Any
	} else {
		value = x.First(obj.Any)
	}
	if value == nil {
		return nil
	}
	if asBag {
		obj = flavor.MakeInstance().(*flavors.Instance)
		obj.Any = value

		return obj
	}
	return slip.SimpleObject(value)
}
