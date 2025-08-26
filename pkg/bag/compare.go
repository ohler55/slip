// Copyright (c) 2023, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Compare{Function: slip.Function{Name: "bag-compare", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-compare",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "A _bag_ to compare the _other_ _bag_.",
				},
				{
					Name: "other",
					Type: "bag",
					Text: "A _bag_ to compare to the first _bag_.",
				},
				{Name: "&optional"},
				{
					Name: "ignores",
					Type: "list",
					Text: `A list of location designators to ignore during the comparison. The locations
can be either _string_, _bag-path_, or a list of _string_, _fixnum_, or _nil_. The _string_ path elements
must follow the JSONPath format and are limited to child, nth, and wildcard selectors. e.g., one[2].*.`,
				},
			},
			Return: "list",
			Text: `__bag-compare__ compares two _bag_s and returns a list that represents the path to the
first difference between the two _bag_s. If they are the same then _nil_ is returned.

This is the same as the _:compare_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq bag-1 (make-instance 'bag-flavor :parse "{a:7 b:[1 2 3]}"))`,
				`(setq bag-2 (make-instance 'bag-flavor :parse "{a:7 b:[1 0 3]}"))`,
				`(bag-compare bag-1 bag-2) => (b 1)`,
				`(bag-compare bag-1 bag-2 '((b 1))) => nil`,
			},
		}, &Pkg)
}

// Compare represents the compare function.
type Compare struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Compare) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Type != flavor {
		slip.TypePanic(s, depth, "bag", args[0], "bag")
	}
	return compareBag(s, obj, args[1:], depth)
}

func compareBag(s *slip.Scope, obj *flavors.Instance, args slip.List, depth int) (result slip.Object) {
	other, ok := args[0].(*flavors.Instance)
	if !ok || other.Type != flavor {
		return slip.List{nil}
	}
	var ignores []alt.Path
	if 1 < len(args) {
		var ilist slip.List
		if ilist, ok = args[1].(slip.List); !ok {
			slip.TypePanic(s, depth, "ignores", args[1], "list")
		}
		for _, p := range ilist {
		whichPath:
			switch tp := p.(type) {
			case slip.String:
				p = Path(jp.MustParseString(string(tp)))
				goto whichPath
			case slip.List:
				ign := make(alt.Path, len(tp))
				for i, e := range tp {
					switch te := e.(type) {
					case slip.String:
						ign[i] = string(te)
					case slip.Symbol:
						ign[i] = string(te)
					case slip.Fixnum:
						ign[i] = int(te)
					case nil:
						ign[i] = nil
					default:
						slip.TypePanic(s, depth, "ignores path element", te, "string", "symbol", "fixnum", "nil")
					}
				}
				ignores = append(ignores, ign)
			case Path:
				ign := make(alt.Path, 0, len(tp))
				for _, f := range tp {
					switch tf := f.(type) {
					case jp.Child:
						ign = append(ign, string(tf))
					case jp.Nth:
						ign = append(ign, int(tf))
					case jp.Wildcard:
						ign = append(ign, nil)
					case jp.Root, jp.At:
						// ignore
					default:
						slip.ErrorPanic(s, depth, "ignores path fragment must be child, nth, and wildcard and not %s (%T)",
							jp.Expr{tf}, tf)
					}
				}
				ignores = append(ignores, ign)
			default:
				slip.TypePanic(s, depth, "ignores element", tp, "list", "string", "bag-path")
			}
		}
	}
	if diff := alt.Compare(obj.Any, other.Any, ignores...); 0 < len(diff) {
		path := make(slip.List, len(diff))
		for i, p := range diff {
			switch tp := p.(type) {
			case string:
				path[i] = slip.String(tp)
			case int:
				path[i] = slip.Fixnum(tp)
			}
		}
		result = path
	}
	return
}
