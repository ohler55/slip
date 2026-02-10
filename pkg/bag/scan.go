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
			f := Scan{Function: slip.Function{Name: "bag-scan", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-scan",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to scan.",
				},
				{
					Name: "function",
					Type: "function",
					Text: "The function to call for each node with then path and value at that node.",
				},
				{Name: "&key"},
				{
					Name: "leaves-only",
					Type: "boolean",
					Text: `If true only leaves trigger the calling of _function_.`,
				},
			},
			Return: "nil",
			Text: `__bag-scan__ scans nodes in a bag and calls _function_ on each node. If the
node is a leaf node then the value passed to the _function_ is a built-in LISP value. If the
node is a branch then then value is a bag.


This is the same as the _:scan_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}") result '())`,
				`(bag-scan bag (lambda (p v) (setq result cons (list p v) result))) => nil`,
				`result => (("$" #<bag-flavor 12345>) ("$.a" 7) ("$.b" 8))`,
			},
		}, &Pkg)
}

// Scan represents the scan function.
type Scan struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Scan) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 4)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}
	scanBag(s, obj, args[1:], depth)

	return nil
}

func scanBag(s *slip.Scope, obj *flavors.Instance, args slip.List, depth int) {
	fn := args[0]
	var leavesOnly bool
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":leaves-only")); has && v != nil {
		leavesOnly = true
	}
	d2 := depth + 1
CallFunc:
	switch tf := fn.(type) {
	case *slip.Lambda:
		jp.Walk(obj.Any, func(path jp.Expr, value any) {
			_ = tf.Call(s, slip.List{slip.String(path.String()), nativeOrBag(value)}, d2)
		}, leavesOnly)
	case *slip.FuncInfo:
		jp.Walk(obj.Any, func(path jp.Expr, value any) {
			_ = tf.Apply(s, slip.List{slip.String(path.String()), nativeOrBag(value)}, d2)
		}, leavesOnly)
	case slip.Symbol:
		fn = slip.MustFindFunc(string(tf))
		goto CallFunc
	case slip.List:
		fn = s.Eval(tf, d2)
		goto CallFunc
	default:
		slip.TypePanic(s, depth, "function", tf, "function")
	}
}

func nativeOrBag(value any) (sv slip.Object) {
	switch tv := value.(type) {
	case nil:
		// leave as nil
	case map[string]any, []any:
		bg := flavor.MakeInstance().(*flavors.Instance)
		bg.Any = tv
		sv = bg
	default:
		sv = slip.SimpleObject(value)
	}
	return
}
