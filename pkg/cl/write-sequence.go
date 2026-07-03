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
	slip.CheckArgCount(s, depth, f, args, 2, 6)
	var ss slip.Stream
	w, ok := args[1].(io.Writer)
	if ok {
		ss, _ = args[1].(slip.Stream)
	} else {
		slip.TypePanic(s, depth, "stream", args[1], "output-stream")
	}
	var (
		ra      []rune
		ba      []byte
		isBytes bool
	)
	switch ta := args[0].(type) {
	case slip.String:
		ra = []rune(ta)
	case slip.Octets:
		isBytes = true
		ba = []byte(ta)
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
			default:
				slip.TypePanic(s, depth, ":end", value, "non-negative fixnum")
			}
		}
	}
	size := len(ra)
	if isBytes {
		size = len(ba)
	}
	if end < 0 {
		end = size
	}
	if size <= start {
		slip.ErrorPanic(s, depth, ":start (%d) out of range 0 to %d.", start, size-1)
	}
	if size < end {
		slip.ErrorPanic(s, depth, ":end (%d) out of range 0 to %d.", end, size)
	}
	if 0 < start || end < size {
		ra = ra[start:end]
	}
	var err error
	if isBytes {
		_, err = w.Write(ba)
	} else {
		_, err = w.Write([]byte(string(ra)))
	}
	if err != nil {
		slip.StreamPanic(s, depth, ss, "write-sequence failed. %s", err)
	}
	return args[0]
}
