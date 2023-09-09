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
			f := Walk{Function: slip.Function{Name: "bag-walk", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-walk",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to walk.",
				},
				{
					Name: "function",
					Type: "function",
					Text: "The function to apply to each node in bag matching the _path_.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to walk.
The path must follow the JSONPath format. Default: ".."`,
				},
				{
					Name: "as-bag",
					Type: "boolean",
					Text: `If not nil then the value to the _function_ is a _bag_ value otherwise a new LISP object.`,
				},
			},
			Return: "bag",
			Text: `__bag-walk__ walks the values at the location described by _path_.

This is the same as the _:walk_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}") result '())`,
				`(bag-walk bag (lambda (x) (setq result cons x result)) "*") => nil`,
				`result => (7 8)`,
			},
		}, &Pkg)
}

// Walk represents the walk function.
type Walk struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Walk) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 4)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Flavor != flavor {
		slip.PanicType("bag", args[0], "bag")
	}
	walkBag(s, obj, args[1:], depth)

	return nil
}

func walkBag(s *slip.Scope, obj *flavors.Instance, args slip.List, depth int) {
	fn := args[0]
	path := jp.D()
	var asBag bool
	if 1 < len(args) {
		switch p := args[1].(type) {
		case nil:
		case slip.String:
			path = jp.MustParseString(string(p))
		case Path:
			path = jp.Expr(p)
		default:
			slip.PanicType("path", p, "string", "bag-path")
		}
		asBag = 2 < len(args) && args[2] != nil
	}
	d2 := depth + 1
CallFunc:
	switch tf := fn.(type) {
	case *slip.Lambda:
		if asBag {
			for _, v := range path.Get(obj.Any) {
				arg := flavor.MakeInstance().(*flavors.Instance)
				arg.Any = v
				_ = tf.Call(s, slip.List{arg}, d2)
			}
		} else {
			for _, v := range path.Get(obj.Any) {
				arg := slip.SimpleObject(v)
				_ = tf.Call(s, slip.List{arg}, d2)
			}
		}
	case *slip.FuncInfo:
		if asBag {
			for _, v := range path.Get(obj.Any) {
				arg := flavor.MakeInstance().(*flavors.Instance)
				arg.Any = v
				_ = tf.Apply(s, slip.List{arg}, d2)
			}
		} else {
			for _, v := range path.Get(obj.Any) {
				arg := slip.SimpleObject(v)
				tf.Apply(s, slip.List{arg}, d2)
			}
		}
	case slip.Symbol:
		fn = slip.FindFunc(string(tf))
		goto CallFunc
	case slip.List:
		fn = s.Eval(tf, d2)
		goto CallFunc
	default:
		slip.PanicType("function", tf, "function")
	}
}
