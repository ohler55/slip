// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringRepeat{Function: slip.Function{Name: "string-repeat", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-repeat",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to repeat.",
				},
				{
					Name: "count",
					Type: "fixnum",
					Text: "Number of times to repeat.",
				},
			},
			Return: "string",
			Text:   `__string-repeat__ make a new string that is a repeat of _string_ by _count_ times.`,
			Examples: []string{
				`(string-repeat "Abc" 3) => "AbcAbcAbc"`,
			},
		}, &Pkg)
}

// StringRepeat represents the string-repeat function.
type StringRepeat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringRepeat) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	var (
		str   string
		count int
	)
	if ss, ok := args[0].(slip.String); ok {
		str = string(ss)
	} else {
		slip.PanicType("string", args[0], "string")
	}
	if num, ok := args[1].(slip.Fixnum); ok {
		count = int(num)
	} else {
		slip.PanicType("count", args[1], "fixnum")
	}
	return slip.String(strings.Repeat(str, count))
}
