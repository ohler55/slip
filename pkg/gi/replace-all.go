// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"regexp"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReplaceAll{Function: slip.Function{Name: "replace-all", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "replace-all",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to apply the replacements to.",
				},
				{
					Name: "old",
					Type: "string",
					Text: "Pattern to replace in the _string_.",
				},
				{
					Name: "new",
					Type: "string",
					Text: "String to replace _old_ in the string.",
				},
				{Name: "&key"},
				{
					Name: "regex",
					Type: "boolean",
					Text: "If true treat _old_ as a regular expression.",
				},
			},
			Return: "string",
			Text:   `__replace-all__ returns a string with all occurrences of _old_ replaced with _new_.`,
			Examples: []string{
				`(replace-all "Like a duck." "duck" "quux") => "Like a quux."`,
			},
		}, &Pkg)
}

// ReplaceAll represents the replace-all function.
type ReplaceAll struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReplaceAll) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 3, 5)
	str := slip.MustBeString(args[0], "string")
	old := slip.MustBeString(args[1], "old")
	nu := slip.MustBeString(args[2], "new")
	if v, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":regex")); has && v != nil {
		rx := regexp.MustCompile(old)
		return slip.String(rx.ReplaceAllString(str, nu))
	}
	return slip.String(strings.ReplaceAll(str, old, nu))
}
