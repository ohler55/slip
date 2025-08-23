// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"io"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pp"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PrettyPrint{Function: slip.Function{Name: "pretty-print", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "pretty-print",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to pretty print.",
				},
				{
					Name: "destination",
					Type: "output-stream|t|nil",
					Text: "The destination for the output.",
				},
			},
			Return: "string|nil",
			Text: `__pretty-print__

the _data_ and returned the uncompressed data along with a property list
of the header fields if there are not empty.`,
			Examples: []string{
				`(let ((*print-right-margin* 20))`,
				`  (pretty-print '(one two three four five six) nil))`,
				` => "(one two three four\n five six)"`,
			},
		}, &Pkg)
}

// PrettyPrint represents the pretty-print function.
type PrettyPrint struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PrettyPrint) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)

	b := pp.Append(nil, s, args[0])

	var (
		w  io.Writer
		ss slip.Stream
	)
	switch ta := args[1].(type) {
	case nil:
		return slip.String(b)
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
	if _, err := w.Write(b); err != nil {
		slip.StreamPanic(s, depth, ss, "write failed. %s", err)
	}
	return
}
