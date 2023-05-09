// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringUpcase{
				stringModify: stringModify{
					Function: slip.Function{Name: "string-upcase", Args: args},
					modify:   strings.ToUpper,
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "string-upcase",
			Args:   stringModifyDocArgs,
			Return: "string",
			Text: `__string-upcase__ returns _string_ upcased. All lowercase characters converted
to uppercase in a copy of the _string_.`,
			Examples: []string{
				`(string-upcase "Abc Def") => "ABC DEF"`,
			},
		}, &slip.CLPkg)
}

// StringUpcase represents the string-upcase function.
type StringUpcase struct {
	stringModify
}
