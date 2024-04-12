// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"regexp"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RegexFind{Function: slip.Function{Name: "regex-find", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "regex-find",
			Args: []*slip.DocArg{
				{
					Name: "regex",
					Type: "string",
					Text: "The regular expression to find against _string_.",
				},
				{
					Name: "string",
					Type: "string",
					Text: "The string find a match for _regex_.",
				},
			},
			Return: "string|nil",
			Text: `__regex-find__ returns the first or left most match for _regex_ in _string_
and returns that substring. If not found then _nil_ is returned.`,
			Examples: []string{
				`(regex-find "a.c" "xabcx") => "abc"`,
				`(regex-find "a.c" "abbc") => nil`,
			},
		}, &Pkg)
}

// RegexFind represents the regex-find function.
type RegexFind struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RegexFind) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)

	rx := regexp.MustCompile(slip.MustBeString(args[0], "regex"))
	str := slip.MustBeString(args[1], "string")

	if found := rx.FindString(str); 0 < len(found) {
		return slip.String(found)
	}
	return nil
}
