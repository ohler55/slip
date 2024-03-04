// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"regexp"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RegexMatch{Function: slip.Function{Name: "regex-match", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "regex-match",
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
			Text:   `__regex-match__ returns true if _regex_ matches _string_ otherwise _nil_ is returned.`,
			Examples: []string{
				`(regex-match "^ab" "abc") => t`,
				`(regex-match "^ab" "_abc") => nil`,
			},
		}, &slip.CLPkg)
}

// RegexMatch represents the regex-match function.
type RegexMatch struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RegexMatch) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)

	rx := regexp.MustCompile(slip.MustBeString(args[0], "regex"))
	str := slip.MustBeString(args[1], "string")

	if rx.MatchString(str) {
		return slip.True
	}
	return nil
}
