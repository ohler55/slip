// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PrettyPrint{Function: slip.Function{Name: "pretty-print", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
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
	slip.ArgCountCheck(f, args, 2, 2)

	p := *slip.DefaultPrinter()
	p.ScopedUpdate(s)

	tree := buildPnode(args[0], &p)
	mx := tree.depth() + 1
	var (
		ok        bool
		tightness int
	)
	rm := int(p.RightMargin)
	for tightness = 0; tightness < mx; tightness++ {
		width := tree.layout(rm, tightness)
		if width <= rm {
			ok = true
			break
		}
	}
	if !ok {
		for tightness = -1; -mx < tightness; tightness-- {
			width := tree.layout(rm, tightness)
			if width <= rm {
				break
			}
		}
	}
	b := tree.adjoin(nil, 0, rm, tightness)
	b = append(b, '\n')

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
		slip.PanicType("destination", ta, "output-stream")
	}
	if _, err := w.Write(b); err != nil {
		slip.PanicStream(ss, "write failed. %s", err)
	}
	return
}
