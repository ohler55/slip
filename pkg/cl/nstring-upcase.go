// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NstringUpcase{
				StringUpcase: StringUpcase{Function: slip.Function{Name: "nstring-upcase", Args: args}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nstring-upcase",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to upcase.",
				},
			},
			Return: "string",
			Text: `__nstring-upcase__ returns _string_ upcased. All lowercase chacters converted
to uppercase in a copy of the _string_.`,
			Examples: []string{
				`(nstring-upcase "Abc" Def) => "ABC DEF"`,
			},
		}, &slip.CLPkg)
}

// NstringUpcase represents the nstring-upcase function.
type NstringUpcase struct {
	StringUpcase
}
