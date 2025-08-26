// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WriteSequence{Function: slip.Function{Name: "write-sequence", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "write-sequence",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to write. It must be a string or characters or fixnum",
				},
				{
					Name: "stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "The start of the section of the sequence to write.",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "The end of the section of the sequence to write.",
				},
			},
			Return: "sequence",
			Text:   `__write-sequence__ writes _sequence_ to _output-stream_. The _sequence_ is returned.`,
			Examples: []string{
				`(write-sequence "abcdef" *standard-output* :start 1 :end 3) => "abcdef" ;; bc is written`,
				`(write-sequence '(65 #\B) *standard-output*) => '(65 #\B) ;; AB is written`,
			},
		}, &slip.CLPkg)
}

// WriteSequence represents the write-sequence function.
type WriteSequence struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WriteSequence) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 6)
	var ss slip.Stream
	w, ok := args[1].(io.Writer)
	if ok {
		ss, _ = args[1].(slip.Stream)
	} else {
		slip.TypePanic(s, depth, "stream", args[1], "output-stream")
	}
	var ra []rune
	switch ta := args[0].(type) {
	case slip.String:
		ra = []rune(ta)
	case slip.List:
		ra = make([]rune, len(ta))
		for i, v := range ta {
			switch tv := v.(type) {
			case slip.Character:
				ra[i] = rune(tv)
			case slip.Fixnum:
				ra[i] = rune(tv)
			default:
				slip.TypePanic(s, depth, "sequence element", v, "character", "fixnum")
			}
		}
	default:
		slip.TypePanic(s, depth, "sequence", ta, "sequence")
	}
	if 2 < len(args) {
		rest := args[2:]
		start := 0
		end := len(ra)
		if w, ok = args[1].(io.Writer); ok {
			ss, _ = args[1].(slip.Stream)
			rest = args[2:]
		}
		if value, has := slip.GetArgsKeyValue(rest, slip.Symbol(":start")); has {
			var num slip.Fixnum
			if num, ok = value.(slip.Fixnum); ok {
				start = int(num)
				if start < 0 || len(ra) <= start {
					slip.ErrorPanic(s, depth, ":start (%d) out of range 0 to %d.", start, len(ra)-1)
				}
			} else {
				slip.TypePanic(s, depth, "start", value, "fixnum")
			}
		}
		if value, has := slip.GetArgsKeyValue(rest, slip.Symbol(":end")); has {
			switch tv := value.(type) {
			case nil:
				end = len(ra)
			case slip.Fixnum:
				end = int(tv)
				if end < start || len(ra) <= end {
					slip.ErrorPanic(s, depth, ":end (%d) out of range %d to %d.", end, start, len(ra)-1)
				}
			default:
				slip.TypePanic(s, depth, "end", value, "fixnum")
			}
		}
		ra = ra[start:end]
	}
	if _, err := w.Write([]byte(string(ra))); err != nil {
		slip.StreamPanic(s, depth, ss, "write-sequence failed. %s", err)
	}
	return args[0]
}
