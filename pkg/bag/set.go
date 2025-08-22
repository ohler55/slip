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
		}, &Pkg)
}

// Set represents the set function.
type Set struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Set) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 2 || 3 < len(args) {
		slip.PanicArgCount(f, 2, 3)
	}
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}
	if 2 < len(args) {
		setBag(s, obj, args[1], args[2], depth)
	} else {
		setBag(s, obj, args[1], nil, depth)
	}
	return obj
}

func setBag(s *slip.Scope, obj *flavors.Instance, value, path slip.Object, depth int) {
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
	v := ObjectToBag(s, value, depth)
	if x == nil {
		obj.Any = v
	} else {
		x.MustSet(obj.Any, v)
	}
}

// ObjectToBag is the same as slip.Simplify except for assoc lists which are
// converted to map[string]any.
func ObjectToBag(s *slip.Scope, obj slip.Object, depth int) (v any) {
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
		if pair, ok := val[0].(slip.List); ok && len(pair) == 2 {
			if _, ok = pair[1].(slip.Tail); ok {
				m := map[string]any{}
				for _, e := range val {
					if pair, ok = e.(slip.List); !ok || len(pair) != 2 {
						slip.TypePanic(s, depth, "assoc list item", e, "cons")
					}
					var key string
					switch tk := pair[0].(type) {
					case slip.Symbol:
						key = string(tk)
					case slip.String:
						key = string(tk)
					default:
						slip.TypePanic(s, depth, "assoc list item car", tk, "symbol", "string")
					}
					cdr := pair[1]
					var tail slip.Tail
					if tail, ok = cdr.(slip.Tail); ok {
						cdr = tail.Value
					}
					m[key] = ObjectToBag(s, cdr, depth)
				}
				v = m
				break
			}
		}
		list := make([]any, len(val))
		for i, o := range val {
			if o == nil {
				list[i] = nil
			} else {
				list[i] = ObjectToBag(s, o, depth)
			}
		}
		v = list
	case *flavors.Instance:
		if val.Type != flavor {
			slip.TypePanic(s, depth, "value", val, "nil", "t", ":false", "integer", "float", "string", "symbol", "gi::time",
				"list", "hash-table", "bag-instance")
		}
		v = val.Any
	default:
		v = val.Simplify()
	}
	return
}
