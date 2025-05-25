// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PrettyPrint{Function: slip.Function{Name: "pretty-print", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pretty-print",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to pretty print.",
				},
				{
					Name: "destination",
					Type: "output-stream|t|nil",
					Text: "The destination for the output.",
				},
			},
			Return: "string|nil",
			Text: `__pretty-print__

the _data_ and returned the uncompressed data along with a property list
of the header fields if there are not empty.`,
			Examples: []string{
				`(let ((*print-right-margin* 20))`,
				`  (pretty-print '(one two three four five six) nil))`,
				` => "(one two three four\n five six)"`,
			},
		}, &Pkg)
}

// PrettyPrint represents the pretty-print function.
type PrettyPrint struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PrettyPrint) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)

	p := *slip.DefaultPrinter()
	p.ScopedUpdate(s)

	tree := buildPnode(args[0], &p)
	mx := tree.depth()

	fmt.Printf("*** depth: %d\n", mx)

	// TBD iterate layouts from 0 to mx then 0 to -mx
	// TBD consider destination for output and return

	return
}
