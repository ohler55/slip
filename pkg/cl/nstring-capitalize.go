// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
	"golang.org/x/text/cases"
	"golang.org/x/text/language"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NstringCapitalize{
				stringModify: stringModify{
					Function: slip.Function{Name: "nstring-capitalize", Args: args},
					modify: func(str string) string {
						return cases.Title(language.Und).String(str)
					},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "nstring-capitalize",
			Args:   stringModifyDocArgs,
			Return: "string",
			Text: `__nstring-capitalize__ returns _string_ with each word in the string capitalized.
Word delimiter are any non-alphanumeric character. As an extension to Common LISP unicode character
are considered as non-punctuation unless they are actually punctuation.`,
			Examples: []string{
				`(nstring-capitalize "abc DeF") => "Abc Def"`,
			},
		}, &slip.CLPkg)
}

// NstringCapitalize represents the string-capitalize function.
type NstringCapitalize struct {
	stringModify
}
