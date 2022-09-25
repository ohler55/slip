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
					Name: "as-lisp",
					Type: "boolean",
					Text: `If not nil then the value to the function is a LISP value otherwise a new _bag_.`,
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
		}, &slip.CLPkg)
}

// Walk represents the walk function.
type Walk struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Walk) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 2 || 4 < len(args) {
		slip.PanicArgCount(f, 2, 4)
	}
	pos := len(args) - 1
	obj, ok := args[pos].(*flavors.Instance)
	if !ok || obj.Flavor != flavor {
		slip.PanicType("bag", args[pos], "bag")
	}
	pos--
	walkBag(s, obj, args, pos, depth)

	return nil
}

func walkBag(s *slip.Scope, obj *flavors.Instance, args slip.List, pos, depth int) {
	fn := args[pos]
	pos--
	path := jp.D()
	var lisp bool
	if 0 <= pos {
		switch p := args[pos].(type) {
		case nil:
		case slip.String:
			path = jp.MustParseString(string(p))
		case Path:
			path = jp.Expr(p)
		default:
			slip.PanicType("path", p, "string", "bag-path")
		}
		pos--
		if 0 <= pos {
			lisp = args[pos] != nil
		}
	}
	d2 := depth + 1
CallFunc:
	switch tf := fn.(type) {
	case *slip.Lambda:
		if lisp {
			for _, v := range path.Get(obj.Any) {
				arg := slip.SimpleObject(v)
				_ = tf.Call(s, slip.List{arg}, d2)
			}
		} else {
			for _, v := range path.Get(obj.Any) {
				arg := flavor.MakeInstance()
				arg.Any = v
				_ = tf.Call(s, slip.List{arg}, d2)
			}
		}
	case *slip.FuncInfo:
		if lisp {
			for _, v := range path.Get(obj.Any) {
				arg := slip.SimpleObject(v)
				tf.Apply(s, slip.List{arg}, d2)
			}
		} else {
			for _, v := range path.Get(obj.Any) {
				arg := flavor.MakeInstance()
				arg.Any = v
				_ = tf.Apply(s, slip.List{arg}, d2)
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
