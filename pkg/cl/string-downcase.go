// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringDowncase{Function: slip.Function{Name: "string-downcase", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-downcase",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to downcase.",
				},
			},
			Return: "string-downcase",
			Text: `__string-downcase__ returns _string_ downcased. All uppercase chacters converted
to lowercase in a copy of the _string_.`,
			Examples: []string{
				`(string-downcase "Abc" Def) => "abc def"`,
			},
		}, &slip.CLPkg)
}

// StringDowncase represents the stringDowncase function.
type StringDowncase struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringDowncase) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
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
		buf = append(buf, []rune(strings.ToLower(string(ra[start:end])))...)
		buf = append(buf, ra[end:]...)
		result = slip.String(buf)
	} else {
		result = slip.String(strings.ToLower(str))
	}
	return
}
