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
			f := ReplaceFirst{Function: slip.Function{Name: "replace-first", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "replace-first",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to apply the replacement to.",
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
			Text:   `__replace-first__ returns a string with first occurrence of _old_ replaced with _new_.`,
			Examples: []string{
				`(replace-first "Like a duck." "duck" "quux") => "Like a quux."`,
			},
		}, &Pkg)
}

// ReplaceFirst represents the replace-first function.
type ReplaceFirst struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReplaceFirst) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 3, 5)
	str := slip.MustBeString(args[0], "string")
	old := slip.MustBeString(args[1], "string")
	nu := slip.MustBeString(args[2], "string")
	if v, has := slip.GetArgsKeyValue(args[3:], slip.Symbol(":regex")); has && v != nil {
		rx := regexp.MustCompile(old)
		replaced := false
		return slip.String(rx.ReplaceAllStringFunc(str, func(orig string) string {
			if replaced {
				return orig
			}
			replaced = true
			return nu
		}))
	}
	return slip.String(strings.Replace(str, old, nu, 1))
}
