// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ShowHistory{Function: slip.Function{Name: "show-history", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "show-history",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "destination",
					Type: "t|nil|output-stream",
					Text: "Destination to write history to.",
				},
				{Name: "&key"},
				{
					Name: "annotate",
					Type: "boolean",
					Text: "If true annotate the output with the history index (1 based).",
				},
				{
					Name: "tight",
					Type: "boolean",
					Text: "If true do not separate entries with a newline.",
				},
				{
					Name: "raw",
					Type: "boolean",
					Text: "If true replace newlines in a form with tabs.",
				},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The start index of the history to show.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "The end index of the history to show.",
				},
			},
			Return: "nil|string",
			Text: `__show-history__ writes history to _destination_. If _destination_ is true,
the default value history is written to _*standard-output*_. If _nil_ history is returned as
a _string_. If an _output-stream_ history is written to the stream.`,
		}, &Pkg)
}

// ShowHistory represents the show-history function.
type ShowHistory struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ShowHistory) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return showStashCall(f, &TheHistory.Stash, s, args)
}
