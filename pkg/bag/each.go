// Copyright (c) 2026, Peter Ohler, All rights reserved.

package bag

import (
	"io"
	"os"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Each{Function: slip.Function{Name: "each-bag", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "each-bag",
			Args: []*slip.DocArg{
				{
					Name: "input",
					Type: "string|stream",
					Text: "A stream or filepath to read JSON or SEN document from.",
				},
				{
					Name: "function",
					Type: "function",
					Text: "The function to apply to each __bag__ read.",
				},
			},
			Return: "nil",
			Text:   `__each-bag__ reads JSON or SEN documents from a file or stream that contains a multiple documents. For each document a __bag__ is created. The _function_ is called with the new __bag__ as an argument.`,
			Examples: []string{
				`(let (found)`,
				`  (each-bag "sample.json" (lambda (b) (setq found (add found b))))`,
				`  found) => (#<bag-flavor 12345> #<bag-flavor 12346>)`,
			},
		}, &Pkg)
}

// Each represents the each-bag function.
type Each struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Each) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	var r io.Reader
	switch ta := args[0].(type) {
	case slip.String:
		f, err := os.Open(string(ta))
		if err != nil {
			slip.FilePanic(s, depth, args[0], "open failed. %s", err)
		}
		defer func() { _ = f.Close() }()
		r = f
	case io.Reader:
		r = ta
	default:
		slip.TypePanic(s, depth, "input", args[0], "stream", "string")
	}
	caller := cl.ResolveToCaller(s, args[1], depth)

	sen.MustParseReader(r, func(j any) bool {
		bg := flavor.MakeInstance().(*flavors.Instance)
		bg.Any = j
		_ = caller.Call(s, slip.List{bg}, depth)
		return false
	})
	return nil
}
