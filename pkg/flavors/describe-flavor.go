// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
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
	slip.ArgCountCheck(f, args, 1, 2)
	var cf *Flavor
	switch ta := args[0].(type) {
	case slip.Symbol:
		if cf = allFlavors[string(ta)]; cf == nil {
			slip.PanicClassNotFound(ta, "%s is not a defined flavor.", ta)
		}
	case *Flavor:
		cf = ta
	default:
		slip.PanicType("flavor argument to describe-flavor", ta, "symbol", "flavor")
	}
	w := s.Get("*standard-output*").(io.Writer)
	if 1 < len(args) {
		var ok bool
		if w, ok = args[1].(io.Writer); !ok {
			slip.PanicType("describe-flavor output-stream", args[1], "output-stream")
		}
	}
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	_, _ = w.Write(cf.Describe(nil, 0, right, ansi))

	return slip.Novalue
}
