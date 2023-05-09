// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringLeftTrim{Function: slip.Function{Name: "string-left-trim", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-left-trim",
			Args: []*slip.DocArg{
				{
					Name: "cutset",
					Type: "string|list",
					Text: "The characters to trim from _string_.",
				},
				{
					Name: "string",
					Type: "string",
					Text: "The string to trim.",
				},
			},
			Return: "string",
			Text:   `__string-trim__ return a copy of _string_ with the _cutset_ characters trimmed the left.`,
			Examples: []string{
				`(string-left-trim " " "  abc ") => "abc "`,
				`(string-left-trim '(#\Space) " abc ") => "abc "`,
			},
		}, &slip.CLPkg)
}

// StringLeftTrim represents the string-left-trim function.
type StringLeftTrim struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringLeftTrim) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	var str string
	if ss, ok := args[1].(slip.String); ok {
		str = string(ss)
	} else {
		slip.PanicType("string", args[1], "string")
	}
	switch ta := args[0].(type) {
	case nil:
		// nothing to trim so no change
	case slip.String:
		str = strings.TrimLeft(str, string(ta))
	case slip.List:
		var cutset []rune
		for _, v := range ta {
			if r, ok := v.(slip.Character); ok {
				cutset = append(cutset, rune(r))
			} else {
				slip.PanicType("cutset list element", ta, "character")
			}
		}
		str = strings.TrimLeft(str, string(cutset))
	default:
		slip.PanicType("cutset", args[0], "string", "list")
	}
	return slip.String(str)
}
