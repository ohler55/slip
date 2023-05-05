// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NstringCapitalize{
				StringCapitalize: StringCapitalize{Function: slip.Function{Name: "nstring-capitalize", Args: args}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nstring-capitalize",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to capitalize.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The index of the start of the portion of the string to modify.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "The index of the end of the portion of the string to modify.",
				},
			},
			Return: "string",
			Text: `__string-capitalize__ returns _string_ with each word in the string capitalized.
Word delimiter are any non-alphanumeric character. As an extension to Common LISP unicode character
are considered as non-punctuation unless they are actually punctuation.`,
			Examples: []string{
				`(nstring-capitalize "abc" DeF) => "Abc Def"`,
			},
		}, &slip.CLPkg)
}

// NstringCapitalize represents the string-capitalize function.
type NstringCapitalize struct {
	StringCapitalize
}
