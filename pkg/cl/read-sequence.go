// Copyright (c) 2026, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReadSequence{Function: slip.Function{Name: "read-sequence", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read-sequence",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "octets",
					Text: "The sequence to read into. The sequence must be octets.",
				},
				{
					Name: "stream",
					Type: "input-stream",
					Text: "The stream to read from.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The start of the section of the sequence to read into.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "The end of the section of the sequence to read into.",
				},
			},
			Return: "fixnum",
			Text: `__read-sequence__ reads from _stream_ and replaces octets in _sequence_. The position
in the sequence of one after the last octet replaced is returned.`,
			Examples: []string{
				`(let ((seq (coerce "abc" 'octets)))`,
				`  (with-input-from-string (s "ce")`,
				`    (read-sequence seq s :start 1))`,
				`  seq) => #(97 99 101)`,
			},
		}, &slip.CLPkg)
}

// ReadSequence represents the read-sequence function.
type ReadSequence struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReadSequence) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 6)
	octs, ok := args[0].(slip.Octets)
	if !ok {
		slip.TypePanic(s, depth, "sequence", args[0], "octets")
	}
	var (
		r  io.Reader
		ss slip.Stream
	)
	if r, ok = args[1].(io.Reader); ok {
		ss, _ = args[1].(slip.Stream)
	} else {
		slip.TypePanic(s, depth, "stream", args[1], "input-stream")
	}
	start := 0
	end := -1
	if 2 < len(args) {
		rest := args[2:]
		if value, has := slip.GetArgsKeyValue(rest, slip.Symbol(":start")); has {
			switch tv := value.(type) {
			case nil:
				start = 0
			case slip.Fixnum:
				start = int(tv)
			default:
				slip.TypePanic(s, depth, ":start", value, "non-negative fixnum")
			}
		}
		if value, has := slip.GetArgsKeyValue(rest, slip.Symbol(":end")); has {
			switch tv := value.(type) {
			case nil:
				end = -1
			case slip.Fixnum:
				end = int(tv)
				if end < 0 {
					slip.TypePanic(s, depth, ":end", value, "non-negative fixnum")
				}
			default:
				slip.TypePanic(s, depth, ":end", value, "non-negative fixnum")
			}
		}
	}
	if end < 0 {
		end = len(octs)
	}
	if len(octs) <= start {
		slip.ErrorPanic(s, depth, ":start (%d) out of range 0 to %d.", start, len(octs)-1)
	}
	if len(octs) < end {
		slip.ErrorPanic(s, depth, ":end (%d) out of range 0 to %d.", end, len(octs))
	}
	if 0 < start || end < len(octs) {
		octs = octs[start:end]
	}
	cnt, err := r.Read([]byte(octs))
	if err != nil {
		slip.StreamPanic(s, depth, ss, "read-sequence failed. %s", err)
	}
	return slip.Fixnum(cnt + start)
}
