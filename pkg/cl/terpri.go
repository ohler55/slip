// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Terpri{Function: slip.Function{Name: "terpri", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "terpri",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text: `__terpri__ writes a string representation of the _object_ to the provided _output-stream_.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(terpri) => nil ;; a newline is written",
			},
		}, &slip.CLPkg)
}

// Terpri represents the terpri function.
type Terpri struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Terpri) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 0, 1)
	var (
		w  io.Writer
		ss slip.Stream
	)
	var arg0 slip.Object = slip.True
	if 0 < len(args) {
		arg0 = args[0]
	}
	switch ta := arg0.(type) {
	case nil:
		// leave w as nil
	case io.Writer:
		w = ta
		ss, _ = args[0].(slip.Stream)
	default:
		if ta == slip.True {
			so := s.Get("*standard-output*")
			ss, _ = so.(slip.Stream)
			if w, _ = so.(io.Writer); w != nil {
				break
			}
		}
		slip.TypePanic(s, depth, "destination", ta, "output-stream")
	}
	if w == nil {
		return slip.String("\n")
	}
	if _, err := w.Write([]byte{'\n'}); err != nil {
		slip.StreamPanic(s, depth, ss, "terpri failed. %s", err)
	}
	return nil
}
