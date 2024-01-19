// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Tagbody{Function: slip.Function{Name: "tagbody", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "tagbody",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "statement",
					Type: "form|tag",
					Text: "The forms to evaluate and tags to go to..",
				},
			},
			Return: "nil",
			Text:   `__tagbody__ is a series of forms and tags that supports the __go__ function.`,
			Examples: []string{
				"(let ((x 0))",
				" (tagbody",
				"  (setq x (1+ x))",
				"  (go skip)",
				"  (setq x (1+ x))",
				"  skip",
				"  (setq x (1+ x)))",
				" x) => 2",
			},
		}, &slip.CLPkg)
}

// Tagbody represents the tagbody function.
type Tagbody struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Tagbody) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	ns := s.NewScope()
	ns.TagBody = true
	d2 := depth + 1
	for i := 0; i < len(args); i++ {
		if gt, _ := slip.EvalArg(ns, args, i, d2).(*GoTo); gt != nil {
			for i++; i < len(args); i++ {
				if args[i] == gt.Tag {
					break
				}
			}
		}
	}
	return nil
}
