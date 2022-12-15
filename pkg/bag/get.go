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
					Name: "as-lisp",
					Type: "boolean",
					Text: `If not nil then the returned value is a LISP value otherwise a new _bag_ is returned.`,
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
	pos := len(args) - 1
	obj, ok := args[pos].(*flavors.Instance)
	if !ok || obj.Flavor != flavor {
		slip.PanicType("bag", args[pos], "bag")
	}
	pos--
	var lisp bool
	if 0 <= pos {
		lisp = 0 < pos && args[0] != nil
		result = getBag(obj, args[pos], lisp)
	} else {
		result = getBag(obj, nil, false)
	}
	return
}

func getBag(obj *flavors.Instance, path slip.Object, lisp bool) slip.Object {
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
	if lisp {
		return slip.SimpleObject(value)
	}
	obj = flavor.MakeInstance()
	obj.Any = value

	return obj
}
