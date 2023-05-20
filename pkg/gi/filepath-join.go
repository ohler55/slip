// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FilepathJoin{Function: slip.Function{Name: "filepath-join", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "filepath-join",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "element",
					Type: "string",
					Text: "The elements to join to for a filepath.",
				},
			},
			Return: "string",
			Text:   `__filepath-join__ joins the _elements_ to form a filepath.`,
			Examples: []string{
				`(filepath-join "one" "two" "three.lisp") => "one/two/three.lisp"`,
			},
		}, &slip.CLPkg)
}

// FilepathJoin represents the filepath-join function.
type FilepathJoin struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FilepathJoin) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if 0 < len(args) {
		elements := make([]string, len(args))
		for i, a := range args {
			if sa, ok := a.(slip.String); ok {
				elements[i] = string(sa)
			} else {
				slip.PanicType("element", a, "string")
			}
		}
		result = slip.String(filepath.Join(elements...))
	}
	return
}
