// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WriteString{Function: slip.Function{Name: "write-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "write-string",
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
			Text:   `__write-string__ writes _string_ to _output-stream_. The _string_ is returned.`,
			Examples: []string{
				`(write-string "abcdef" *standard-output* :start 1 :end 3) => "abcdef" ;; bc is written`,
			},
		}, &slip.CLPkg)
}

// WriteString represents the write-string function.
type WriteString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WriteString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 6)
	str, ra, w, ss := parseWriteStringArgs(s, args)
	if _, err := w.Write([]byte(string(ra))); err != nil {
		slip.PanicStream(ss, "write-string failed. %s", err)
	}
	return str
}

func parseWriteStringArgs(s *slip.Scope, args slip.List) (str slip.String, ra []rune, w io.Writer, ss slip.Stream) {
	so := s.Get("*standard-output*")
	w = so.(io.Writer)
	ss, _ = so.(slip.Stream)

	var ok bool
	if str, ok = args[0].(slip.String); !ok {
		slip.PanicType("string", args[0], "string")
	}
	ra = []rune(str)
	if 1 < len(args) {
		rest := args[1:]
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
					slip.NewPanic(":start (%d) out of range 0 to %d.", start, len(ra)-1)
				}
			} else {
				slip.PanicType("start", value, "fixnum")
			}
		}
		if value, has := slip.GetArgsKeyValue(rest, slip.Symbol(":end")); has {
			switch tv := value.(type) {
			case nil:
				end = len(ra)
			case slip.Fixnum:
				end = int(tv)
				if end < start || len(ra) <= end {
					slip.NewPanic(":end (%d) out of range %d to %d.", end, start, len(ra)-1)
				}
			default:
				slip.PanicType("end", value, "fixnum")
			}
		}
		ra = ra[start:end]
	}
	return
}
