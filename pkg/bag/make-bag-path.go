// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeBagPath{Function: slip.Function{Name: "make-bag-path", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-bag-path",
			Args: []*slip.DocArg{
				{
					Name: "path",
					Type: "string",
					Text: "A string that describes the path.",
				},
			},
			Return: "bag-path",
			Text:   `__make-bag-path__ makes a bag-path.`,
			Examples: []string{
				`(make-bag-path "users[?(@.given == 'Fred')]") => #<bag-path users[?(@.given == 'Fred')]>`,
			},
		}, &Pkg)
}

// MakeBagPath represents the makeBagPath function.
type MakeBagPath struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeBagPath) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var path Path
	switch ta := args[0].(type) {
	case slip.String:
		path = Path(jp.MustParseString(string(ta)))
	case slip.List:
		path = make(Path, len(ta))
		for i, e := range ta {
			switch te := e.(type) {
			case nil:
				path[i] = jp.Wildcard('*')
			case slip.String:
				path[i] = jp.Child(te)
			case slip.Symbol:
				path[i] = jp.Child(te)
			case slip.Integer:
				path[i] = jp.Nth(te.Int64())
			default:
				slip.TypePanic(s, depth, "path fragment", e, "string", "symbol", "integer", "nil")
			}
		}
	default:
		slip.TypePanic(s, depth, "path", args[0], "string", "list")
	}
	return path
}
