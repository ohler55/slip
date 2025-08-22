// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WildPathnameP{Function: slip.Function{Name: "wild-pathname-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "wild-pathname-p",
			Args: []*slip.DocArg{
				{
					Name: "pathname",
					Type: "string",
					Text: "A pathname.",
				},
			},
			Return: "boolean",
			Text: `__wild-pathname-p__ returns _true_ if _pathname_ includes a wildcard character
which are _#\*_, _#\?_, or _#\[_.`,
			Examples: []string{
				`(wild-pathname-p "/User/someone/*.lisp") => t`,
				`(wild-pathname-p "/User/someone/quux.lisp") => nil`,
			},
		}, &slip.CLPkg)
}

// WildPathnameP represents the wild-pathname-p function.
type WildPathnameP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WildPathnameP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "pathname", args[0], "string")
	}
	if strings.ContainsAny(string(path), "*?[") {
		result = slip.True
	}
	return result
}
