// Copyright (c) 2026, Peter Ohler, All rights reserved.

package bag

import (
	"os"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Load{Function: slip.Function{Name: "load-bag", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "load-bag",
			Args: []*slip.DocArg{
				{
					Name: "filename",
					Type: "string",
					Text: "The filepath to a JSON or SEN file to load.",
				},
			},
			Return: "bag",
			Text: `__load-bag__ reads a JSON or SEN file that contains a single document and
creates a __bag__ instance with the content.`,
			Examples: []string{
				`(load-bag "sample.json") => #<bag-flavor 12345>`,
			},
		}, &Pkg)
}

// Load represents the load-bag function.
type Load struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Load) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	path := slip.MustBeString(args[0], "filename")

	contents, err := os.ReadFile(path)
	if err != nil {
		slip.FilePanic(s, depth, args[0], "read failed. %s", err)
	}
	self := flavor.MakeInstance().(*flavors.Instance)
	self.Init(s, slip.List{}, depth)
	self.Any = sen.MustParse(contents)

	return self
}
