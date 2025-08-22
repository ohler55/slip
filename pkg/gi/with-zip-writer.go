// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"compress/gzip"
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithZipWriter{Function: slip.Function{Name: "with-zip-writer", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "with-zip-writer",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: `A list of symbol to be bound to the zip writer and a output-stream to write to.
The remaining elements of the args list are the same as for the __zip__ function.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "nil",
			Text: `__with-zip-writer__ creates a zip writer stream that compresses and writes to the provided
_output-stream_ which must be the second element of the _args_ list. The first elements of the _args_ list must
be a symbol which is then bound to the new zip _output-stream_. The remaining elements of the _args_ list are
treated the same as the arguments for the __zip__ function.`,
			Examples: []string{
				`(base64-encode`,
				` (with-output-to-string (s)`,
				`   (with-zip-writer (z s 5 :comment "test")`,
				`     (format z "some data")))) => "H4sIEAAAAAAA/3Rlc3QAKs7PTVVISSxJBAAAAP//AQAA//8e6cLZCQAAAA=="`,
			},
		}, &Pkg)
}

// WithZipWriter represents the with-zip-writer function.
type WithZipWriter struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WithZipWriter) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	forms := args[1:]
	if list, ok := args[0].(slip.List); ok {
		args = list
	} else {
		slip.TypePanic(s, depth, "args", args[0], "list")
	}
	slip.ArgCountCheck(f, args, 2, 13)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "args[0]", args[0], "symbol")
	}
	d2 := depth + 1
	args = args[1:]
	for i := range args {
		args[i] = slip.EvalArg(s, args, i, d2)
	}
	var w io.Writer
	if w, ok = args[0].(io.Writer); !ok {
		slip.TypePanic(s, depth, "args[1]", args[0], "output-stream")
	}
	args = args[1:]
	level := gzip.DefaultCompression
	if 0 < len(args) {
		switch ta := args[0].(type) {
		case nil:
			// leave as the default
			args = args[1:]
		case slip.Fixnum:
			level = int(ta)
			args = args[1:]
		case slip.Symbol:
			// not a level
		default:
			slip.TypePanic(s, depth, "level", ta, "fixnum", "nil")
		}
	}
	z, err := gzip.NewWriterLevel(w, level)
	if err != nil {
		panic(err)
	}
	setZipHeader(s, z, args, depth)
	s2 := s.NewScope()
	s2.Let(sym, &slip.OutputStream{Writer: z})
	for i := range forms {
		_ = slip.EvalArg(s2, forms, i, d2)
	}
	_ = z.Flush()
	_ = z.Close()

	return nil
}
