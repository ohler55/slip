// Copyright (c) 2023, Peter Ohler, All rights reserved.

package bag

import (
	"strings"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Modify{Function: slip.Function{Name: "bag-modify", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-modify",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to modify a value in.",
				},
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call to modify the value at _path_.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to modify.
The path must follow the JSONPath format.`,
				},
				{Name: "&key"},
				{
					Name: "as-bag",
					Type: "boolean",
					Text: `If set to true then the value provided to _function_ will be a bag
instance otherwise it will be a lisp construct.`,
				},
			},
			Return: "bag",
			Text: `__bag-modify__ modifys the value at the location described by _path_.
If no _path_ is provided the entire contents of the bag is modified.

This is the same as the _:modify_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:[1 2 3]}"))`,
				`(bag-modify bag "b" 'reverse) => #<bag-flavor 12345> ;; content is now {a:7 b:[3 2 1]}`,
			},
		}, &Pkg)
}

// Modify represents the modify function.
type Modify struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Modify) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 5)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}
	modifyBag(s, obj, args[1:], depth+1)

	return obj
}

func modifyBag(s *slip.Scope, obj *flavors.Instance, args slip.List, depth int) {
	caller := cl.ResolveToCaller(s, args[0], depth)
	var (
		x     jp.Expr
		asBag bool
	)
	if 1 < len(args) {
		switch p := args[1].(type) {
		case nil:
		case slip.String:
			x = jp.MustParseString(string(p))
		case Path:
			x = jp.Expr(p)
		default:
			slip.TypePanic(s, depth, "path", p, "string")
		}
		if 2 < len(args) {
			for pos := 2; pos < len(args); pos += 2 {
				sym, ok := args[pos].(slip.Symbol)
				if !ok {
					slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
				}
				if len(args)-1 <= pos {
					slip.ErrorPanic(s, depth, "keyword %s is missing a value", sym)
				}
				if strings.EqualFold(string(sym), ":as-bag") {
					asBag = args[pos+1] != nil
				} else {
					slip.TypePanic(s, depth, "keyword", sym, ":as-bag")
				}
			}
		}
	}
	if x == nil {
		obj.Any = modifyValue(s, obj.Any, caller, asBag, depth)
	} else {
		obj.Any = x.MustModify(obj.Any, func(element any) (altered any, changed bool) {
			return modifyValue(s, element, caller, asBag, depth), true
		})
	}
}

func modifyValue(s *slip.Scope, value any, caller slip.Caller, asBag bool, depth int) any {
	if asBag {
		bg := flavor.MakeInstance().(*flavors.Instance)
		bg.Any = value
		obj := caller.Call(s, slip.List{bg}, depth)
		if bg, _ := obj.(*flavors.Instance); bg != nil && bg.Type == flavor {
			return bg.Any
		}
		return slip.Simplify(obj)
	}
	obj := slip.SimpleObject(value)
	obj = caller.Call(s, slip.List{obj}, depth)
	if bg, _ := obj.(*flavors.Instance); bg != nil && bg.Type == flavor {
		return bg.Any
	}
	return slip.Simplify(obj)
}
