// Copyright (c) 2026, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetAll{Function: slip.Function{Name: "bag-get-all", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-get-all",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to get values from.",
				},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to get the values from.
The path must follow the JSONPath format.`,
				},
				{Name: "&optional"},
				{
					Name: "return-type",
					Type: "keyword",
					Text: `Dictates the return type:
  __:native__ returns a list of Lisp objects
  __:bag__ returns a bag instance
  __:bag-list__ returns a list of __bag__ instances (the default)
`,
				},
			},
			Return: "list|bag",
			Text: `__bag-get-all__ gets all the values at the location described by _path_.

This is the same as the _:get-all_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:3}"))`,
				`(bag-get-all bag "*") => (7 3)`,
			},
		}, &Pkg)
}

// GetAll represents the get-all function.
type GetAll struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *GetAll) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}
	var retType slip.Object
	if len(args) == 3 {
		retType = args[2]
	}
	return getAllBag(s, obj, args[1], retType, depth)
}

func getAllBag(s *slip.Scope, obj *flavors.Instance, path, retType slip.Object, depth int) (result slip.Object) {
	var x jp.Expr
	switch p := path.(type) {
	case nil:
	case slip.String:
		x = jp.MustParseString(string(p))
	case Path:
		x = jp.Expr(p)
	default:
		slip.TypePanic(s, depth, "path", p, "string", "bag-path")
	}
	got := x.Get(obj.Any)
	if 0 < len(got) {
		switch retType {
		case nil, slip.Symbol(":bag-list"):
			list := make(slip.List, len(got))
			for i, v := range got {
				ov := flavor.MakeInstance().(*flavors.Instance)
				ov.Any = v
				list[i] = ov
			}
			result = list
		case slip.Symbol(":bag"):
			obj = flavor.MakeInstance().(*flavors.Instance)
			obj.Any = got
			result = obj
		case slip.Symbol(":native"):
			list := make(slip.List, len(got))
			for i, v := range got {
				list[i] = slip.SimpleObject(v)
			}
			result = list
		default:
			slip.TypePanic(s, depth, "return-type", retType, ":bag-list", ":native", ":bag")
		}
	}
	return
}
