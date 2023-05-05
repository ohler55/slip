// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringUpcase{Function: slip.Function{Name: "string-upcase", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-upcase",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to upcase.",
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
			Text: `__string-upcase__ returns _string_ upcased. All lowercase chacters converted
to uppercase in a copy of the _string_.`,
			Examples: []string{
				`(string-upcase "Abc" Def) => "ABC DEF"`,
			},
		}, &slip.CLPkg)
}

// StringUpcase represents the string-upcase function.
type StringUpcase struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringUpcase) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 5)
	var str string
	if ss, ok := args[0].(slip.String); ok {
		str = string(ss)
	} else {
		slip.PanicType("string", args[0], "string")
	}
	if 1 < len(args) {
		ra := []rune(str)
		start, end := getStartEndKeywords(args[1:], len(ra))
		buf := make([]rune, 0, len(ra))
		buf = append(buf, ra[:start]...)
		buf = append(buf, []rune(strings.ToUpper(string(ra[start:end])))...)
		buf = append(buf, ra[end:]...)
		result = slip.String(buf)
	} else {
		result = slip.String(strings.ToUpper(str))
	}
	return
}
