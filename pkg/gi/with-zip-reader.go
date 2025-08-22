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
			f := WithZipReader{Function: slip.Function{Name: "with-zip-reader", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "with-zip-reader",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: `A list of symbol to be bound to the zip reader and a input-stream to read from.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "octets,property-list",
			Text: `__with-zip-reader__ creates a zip reader stream that uncompresses data from the provided
_input-stream_ which must be the second element of the _args_ list. The first elements of the _args_ list must
be a symbol which is then bound to the new zip _input-stream_.`,
			Examples: []string{
				`(let ((src (base64-decode "H4sIEAAAAAAA/3Rlc3QAKs7PTVVISSxJBAAAAP//AQAA//8e6cLZCQAAAA==")))`,
				`  (with-input-from-octets (s src)`,
				`    (with-zip-reader (z s)`,
				`      (read-all z)))) => "some data"`,
			},
		}, &Pkg)
}

// WithZipReader represents the with-zip-reader function.
type WithZipReader struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WithZipReader) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	forms := args[1:]
	if list, ok := args[0].(slip.List); ok {
		args = list
	} else {
		slip.TypePanic(s, depth, "args", args[0], "list")
	}
	slip.ArgCountCheck(f, args, 2, 2)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "args[0]", args[0], "symbol")
	}
	d2 := depth + 1
	args[1] = slip.EvalArg(s, args, 1, d2)
	var r io.Reader
	if r, ok = args[1].(io.Reader); !ok {
		slip.TypePanic(s, depth, "args[1]", args[1], "input-stream")
	}
	z, err := gzip.NewReader(r)
	if err != nil {
		panic(err)
	}
	s2 := s.NewScope()
	s2.Let(sym, &slip.InputStream{Reader: z})
	for i := range forms {
		result = slip.EvalArg(s2, forms, i, d2)
	}
	_ = z.Close()

	return
}
