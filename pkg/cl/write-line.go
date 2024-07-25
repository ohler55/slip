// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WriteLine{Function: slip.Function{Name: "write-line", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "write-line",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to write.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The start of the section of the string to write.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "The end of the section of the string to write.",
				},
			},
			Return: "string",
			Text:   `__write-line__ writes _line_ to _output-stream_ followed by a newline. The _string_ is returned.`,
			Examples: []string{
				`(write-line "abcdef" *standard-output* :start 1 :end 3) => "abcdef" ;; bc\n is written`,
			},
		}, &slip.CLPkg)
}

// WriteLine represents the write-line function.
type WriteLine struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WriteLine) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 6)
	str, ra, w, ss := parseWriteStringArgs(s, args)
	ra = append(ra, '\n')
	if _, err := w.Write([]byte(string(ra))); err != nil {
		slip.PanicStream(ss, "write-string failed. %s", err)
	}
	return str
}
