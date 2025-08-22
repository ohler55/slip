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
			f := Split{Function: slip.Function{Name: "split", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "split",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to apply the replacements to.",
				},
				{
					Name: "separator",
					Type: "string",
					Text: "Separator to split on.",
				},
				{Name: "&key"},
				{
					Name: "limit",
					Type: "fixnum",
					Text: "Maximum number of substrings to return. Default: -1 (all)",
				},
				{
					Name: "omit-empty",
					Type: "boolean",
					Text: "If true omit empty strings.",
				},
				{
					Name: "regex",
					Type: "boolean",
					Text: "If true treat _separator_ as a regular expression.",
				},
			},
			Return: "list",
			Text: `__split__ returns a list strings resulting from splitting _string_ on
occurrences of _separator_. The _limit_ specifies the maximum number of list members where a
negative _limit_ indicates all.`,
			Examples: []string{
				`(split "Like a duck." " ") => ("Like" "a" "duck.")`,
			},
		}, &Pkg)
}

// Split represents the split function.
type Split struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Split) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 8)
	str := slip.MustBeString(args[0], "string")
	sep := slip.MustBeString(args[1], "separator")
	limit := -1
	omit := false
	var parts []string
	if v, has := slip.GetArgsKeyValue(args[2:], slip.Symbol(":limit")); has {
		if num, ok := v.(slip.Fixnum); ok {
			limit = int(num)
		} else {
			slip.TypePanic(s, depth, ":limit", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args[2:], slip.Symbol(":omit-empty")); has && v != nil {
		omit = true
	}
	if v, has := slip.GetArgsKeyValue(args[2:], slip.Symbol(":regex")); has && v != nil {
		rx := regexp.MustCompile(sep)
		parts = rx.Split(str, limit)
	} else {
		parts = strings.SplitN(str, sep, limit)
	}
	var list slip.List
	if 0 < len(parts) {
		if omit {
			for _, part := range parts {
				if 0 < len(part) {
					list = append(list, slip.String(part))
				}
			}
		} else {
			list = make(slip.List, len(parts))
			for i, part := range parts {
				list[i] = slip.String(part)
			}
		}
	}
	return list
}
