// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"io"

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
	slip.ArgCountCheck(f, args, 0, 11)
	w := s.Get("*standard-output*").(io.Writer)
	start := 0
	end := -1
	var (
		annotate bool
		tight    bool
		raw      bool
	)
	if 0 < len(args) {
		switch ta := args[0].(type) {
		case nil:
			w = nil
			args = args[1:]
		case io.Writer:
			w = ta
			args = args[1:]
		case slip.Symbol:
			break
		default:
			if ta == slip.True {
				// leave w as is
				args = args[1:]
				break
			}
			slip.PanicType("destination", ta, "output-stream", "t", "nil")
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":annotate")); has && v != nil {
		annotate = true
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":tight")); has && v != nil {
		tight = true
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":raw")); has && v != nil {
		raw = true
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":start")); has {
		if num, ok := v.(slip.Fixnum); ok {
			start = int(num)
		} else {
			slip.PanicType(":start", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":end")); has {
		if num, ok := v.(slip.Fixnum); ok {
			end = int(num)
		} else {
			slip.PanicType(":start", v, "fixnum")
		}
	}
	buf := TheHistory.Append(nil, annotate, tight, raw, start, end)
	if w == nil {
		return slip.String(buf)
	}
	if _, err := w.Write(buf); err != nil {
		panic(err)
	}
	return nil
}
