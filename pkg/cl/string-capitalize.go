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
			f := StringCapitalize{Function: slip.Function{Name: "string-capitalize", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-capitalize",
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
				`(string-capitalize "abc" DeF) => "Abc Def"`,
			},
		}, &slip.CLPkg)
}

// StringCapitalize represents the string-capitalize function.
type StringCapitalize struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringCapitalize) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 5)
	var str string
	if ss, ok := args[0].(slip.String); ok {
		str = string(ss)
	} else {
		slip.PanicType("string", args[0], "string")
	}
	caser := cases.Title(language.Und)
	if 1 < len(args) {
		ra := []rune(str)
		start, end := getStartEndKeywords(args[1:], len(ra))
		buf := make([]rune, 0, len(ra))
		buf = append(buf, ra[:start]...)
		buf = append(buf, []rune(caser.String(string(ra[start:end])))...)
		buf = append(buf, ra[end:]...)
		result = slip.String(buf)
	} else {
		result = slip.String(caser.String(str))
	}
	return
}
