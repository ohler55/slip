// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"regexp"

	"github.com/ohler55/slip"
)

func defRegexSubmatch() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RegexSubmatch{Function: slip.Function{Name: "regex-submatch", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "regex-submatch",
			Args: []*slip.DocArg{
				{
					Name: "regex",
					Type: "string",
					Text: "The regular expression to match against _string_.",
				},
				{
					Name: "string",
					Type: "string",
					Text: "The string match with _regex_.",
				},
			},
			Return: "boolean",
			Text:   `__regex-submatch__ returns true if _regex_ matches _string_ otherwise _nil_ is returned.`,
			Examples: []string{
				`(regex-submatch "^ab" "abc") => t`,
				`(regex-submatch "^ab" "_abc") => nil`,
			},
		}, &Pkg)
}

// RegexSubmatch represents the regex-submatch function.
type RegexSubmatch struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RegexSubmatch) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)

	rx := regexp.MustCompile(slip.MustBeString(args[0], "regex"))
	str := slip.MustBeString(args[1], "string")

	matches := rx.FindStringSubmatch(str)
	var result slip.List
	if 0 < len(matches) {
		result = make(slip.List, len(matches))
		for i, m := range matches {
			result[i] = slip.String(m)
		}
	}
	return result
}
