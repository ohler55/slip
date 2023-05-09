// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringRightTrim{Function: slip.Function{Name: "string-right-trim", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-right-trim",
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
			Text:   `__string-trim__ return a copy of _string_ with the _cutset_ characters trimmed the right.`,
			Examples: []string{
				`(string-right-trim " " "  abc ") => "  abc"`,
				`(string-right-trim '(#\Space) " abc ") => "  abc"`,
			},
		}, &slip.CLPkg)
}

// StringRightTrim represents the string-right-trim function.
type StringRightTrim struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringRightTrim) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
		str = strings.TrimRight(str, string(ta))
	case slip.List:
		var cutset []rune
		for _, v := range ta {
			if r, ok := v.(slip.Character); ok {
				cutset = append(cutset, rune(r))
			} else {
				slip.PanicType("cutset list element", ta, "character")
			}
		}
		str = strings.TrimRight(str, string(cutset))
	default:
		slip.PanicType("cutset", args[0], "string", "list")
	}
	return slip.String(str)
}
