// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringEqual{Function: slip.Function{Name: "string-equal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-equal",
			Args: []*slip.DocArg{
				{
					Name: "string1",
					Type: "string",
					Text: "The first string to comapre.",
				},
				{
					Name: "string2",
					Type: "string",
					Text: "The second string to comapre.",
				},
				{Name: "&key"},
				{
					Name: "start1",
					Type: "fixnum",
					Text: "The index of the start of the portion of string1 to compare.",
				},
				{
					Name: "end1",
					Type: "fixnum",
					Text: "The index of the end of the portion of string1 to compare.",
				},
				{
					Name: "start2",
					Type: "fixnum",
					Text: "The index of the start of the portion of string2 to compare.",
				},
				{
					Name: "end2",
					Type: "fixnum",
					Text: "The index of the end of the portion of string2 to compare.",
				},
			},
			Return: "string",
			Text: `__string-equal__ returns _string_ equald. All uppercase chacters converted
to lowercase in a copy of the _string_.`,
			Examples: []string{
				`(string-equal "Abc" Def) => "abc def"`,
			},
		}, &slip.CLPkg)
}

// StringEqual represents the string-equal function.
type StringEqual struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringEqual) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	// TBD
	//  maybe a StringCompare type with a compare function pointer
	return
}
