// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"regexp"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RegexFindAll{Function: slip.Function{Name: "regex-find-all", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "regex-find-all",
			Args: []*slip.DocArg{
				{
					Name: "regex",
					Type: "string",
					Text: "The regular expression to findAll against _string_.",
				},
				{
					Name: "string",
					Type: "string",
					Text: "The string findAll a match for _regex_.",
				},
				{Name: "&key"},
				{
					Name: "limit",
					Type: "fixnum",
					Text: "Limit the number of matches returned. Default is -1 which means no limit.",
				},
			},
			Return: "list",
			Text:   `__regex-find-all__ returns the all the matches for _regex_ in _string_ as a list.`,
			Examples: []string{
				`(regex-find-all "a.c" "xabcxazc") => ("abc" "azc")`,
				`(regex-find-all "a.c" "abbc") => nil`,
			},
		}, &Pkg)
}

// RegexFindAll represents the regex-find-all function.
type RegexFindAll struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RegexFindAll) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 4)

	rx := regexp.MustCompile(slip.MustBeString(args[0], "regex"))
	str := slip.MustBeString(args[1], "string")
	limit := -1
	if v, has := slip.GetArgsKeyValue(args[2:], slip.Symbol(":limit")); has {
		if num, ok := v.(slip.Fixnum); ok {
			limit = int(num)
		} else {
			slip.PanicType(":limit", v, "fixnum")
		}
	}
	var found slip.List
	for _, match := range rx.FindAllString(str, limit) {
		found = append(found, slip.String(match))
	}
	return found
}
