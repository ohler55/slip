// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"strings"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Set{Function: slip.Function{Name: "bag-set", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-set",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to set a value in.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The _value_ to set in _bag_ according to the path.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|bag-path",
					Text: `The path to the location in the bag to set the _value_.
The path must follow the JSONPath format.`,
				},
			},
			Return: "bag",
			Text: `__bag-set__ sets a _value_ at the location described by _path_.
If no _path_ is provided the entire contents of the bag is replaced.

This is the same as the _:set_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`,
				`(bag-set bag 3 "a") => #<bag-flavor 12345> ;; content is now {a:3}`,
			},
		}, &slip.CLPkg)
}

// Set represents the set function.
type Set struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Set) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 2 || 3 < len(args) {
		slip.PanicArgCount(f, 2, 3)
	}
	pos := len(args) - 1
	obj, ok := args[pos].(*flavors.Instance)
	if !ok || obj.Flavor != flavor {
		slip.PanicType("bag", args[pos], "bag")
	}
	pos--
	if 0 < pos {
		setBag(obj, args[pos], args[0])
	} else {
		setBag(obj, args[pos], nil)
	}
	return obj
}

func setBag(obj *flavors.Instance, value, path slip.Object) {
	var x jp.Expr
	switch p := path.(type) {
	case nil:
	case slip.String:
		x = jp.MustParseString(string(p))
	case Path:
		x = jp.Expr(p)
	default:
		slip.PanicType("path", p, "string")
	}
	v := objectToBag(value)
	if x == nil {
		obj.Any = v
	} else {
		x.MustSet(obj.Any, v)
	}
}

func objectToBag(obj slip.Object) (v any) {
	switch val := obj.(type) {
	case nil:
		// leave v as nil
	case slip.Symbol:
		if strings.EqualFold(":false", string(val)) {
			v = false
		} else {
			v = string(val)
		}
	case slip.List:
		if len(val) == 0 {
			v = nil
			break
		}
		// If an assoc list then assume a map and if it fails then panic.
		if _, ok := val[0].(slip.Cons); ok {
			m := map[string]any{}
			for _, e := range val {
				var c slip.Cons
				if c, ok = e.(slip.Cons); !ok {
					slip.PanicType("assoc list item", e, "cons")
				}
				var key string
				switch tk := c.Car().(type) {
				case slip.Symbol:
					key = string(tk)
				case slip.String:
					key = string(tk)
				default:
					slip.PanicType("assoc list item car", tk, "symbol", "string")
				}
				m[key] = objectToBag(c.Cdr())
			}
			v = m
			break
		}
		list := make([]any, len(val))
		for i, o := range val {
			if o == nil {
				list[len(val)-i-1] = nil
			} else {
				list[len(val)-i-1] = objectToBag(o)
			}
		}
		v = list
	case *flavors.Instance:
		if val.Flavor != flavor {
			slip.PanicType("value", val, "nil", "t", ":false", "integer", "float", "string", "symbol", "gi::time",
				"list", "hash-table", "bag-instance")
		}
		v = val.Any
	default:
		v = val.Simplify()
	}
	return
}
