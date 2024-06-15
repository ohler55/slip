// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DefSystem{Function: slip.Function{Name: "defsystem", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "defsystem",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The name of the system.",
				},
				{Name: "&key"},
				{
					Name: "author",
					Type: "string",
				},
				{
					Name: "maintainer",
					Type: "string",
				},
				{
					Name: "license",
					Type: "string",
				},
				{
					Name: "version",
					Type: "string",
				},
				{
					Name: "homepage",
					Type: "string",
				},
				{
					Name: "bug-tracker",
					Type: "string",
				},
				{
					Name: "source-control",
					Type: "string",
				},
				{
					Name: "description",
					Type: "string",
				},
				{
					Name: "depends-on",
					Type: "string",
					Text: systemDependsOnDoc,
				},
				{
					Name: "components",
					Type: "string",
					Text: systemComponentsDoc,
				},
				{
					Name: "in-order-to",
					Type: "string",
					Text: systemInOrderToDoc,
				},
				{
					Name: "cache",
					Type: "string",
					Text: systemCacheDoc,
				},
			},
			Return: "instance",
			Text:   `__defsystem__ creates an instance of the System flavor.`,
			Examples: []string{
				`(defsystem "sister" :components '("one.lisp")) => #<system 12345>`,
			},
		}, &Pkg)
}

// DefSystem represents the defsystem function.
type DefSystem struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DefSystem) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 25)
	self := system.MakeInstance().(*flavors.Instance)
	xargs := make(slip.List, len(args)+1)
	xargs[0] = slip.Symbol(":name")
	copy(xargs[1:], args)

	self.Init(s, xargs, depth)

	return self
}
