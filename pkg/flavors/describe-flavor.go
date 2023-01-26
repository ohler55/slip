// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DescribeFlavor{Function: slip.Function{Name: "describe-flavor", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "describe-flavor",
			Args: []*slip.DocArg{
				{
					Name: "flavor",
					Type: "symbol",
					Text: "The name of the flavor.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Text: `__describe-flavor__ writes a description of the _flavor_ to the provided _output-stream_.
If the _output-stream_ is not provided then the _*standard-output*_ is used.`,
			Examples: []string{
				"(describe-flavor 'vanilla-flavor) ;; prints the documentation for the vanila-flavor",
			},
		}, &Pkg)
}

// DescribeFlavor represents the describeFlavor function.
type DescribeFlavor struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *DescribeFlavor) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	pos := len(args) - 1
	var cf *Flavor
	switch ta := args[pos].(type) {
	case slip.Symbol:
		cf = allFlavors[string(ta)]
	case *Flavor:
		cf = ta
	default:
		slip.PanicType("flavor argument to describe-flavor", ta, "symbol", "flavor")
	}
	if cf == nil {
		panic(fmt.Sprintf("%s is not a defined flavor.", args[pos]))
	}
	w := s.Get("*standard-output*").(io.Writer)
	pos--
	if pos == 0 {
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType("describe-flavor output-stream", args[0], "output-stream")
		}
	}
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	_, _ = w.Write(cf.Describe(nil, 0, right, ansi))

	return slip.Novalue
}
