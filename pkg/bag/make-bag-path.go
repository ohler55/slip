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
	slip.ArgCountCheck(f, args, 1, 1)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("path", args[0], "string")
	}
	return Path(jp.MustParseString(string(str)))
}
