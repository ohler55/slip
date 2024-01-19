// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Progx{Function: slip.Function{Name: "prog*", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "prog*",
			Args: []*slip.DocArg{
				{
					Name: "bindings",
					Type: "list",
					Text: "A list of variables with optional initial value form.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__prog*__ binds the binding variables and then evaluates the forms in
order. All bindings are performed in sequence unlike __prog__. A __prog*__ permits the
use of __return__ and __go__ statements. _nil_ is returned unless a __return__ statement
is encountered.`,
			Examples: []string{
				"(progx ()) => nil",
				"(progx ((x 1) y) (return (list x y))) => (1 nil)",
			},
		}, &slip.CLPkg)
}

// Progx represents the progx function.
type Progx struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Progx) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	ns := s.NewScope()
	ns.Block = true
	ns.TagBody = true
	d2 := depth + 1
	processBinding(ns, ns, args[0], d2)
	for i := 1; i < len(args); i++ {
		switch tr := slip.EvalArg(ns, args, i, d2).(type) {
		case *ReturnResult:
			if tr.Tag == nil {
				return tr.Result
			}
			if s.Block {
				return tr
			}
			slip.NewPanic("return from unknown block: %s", tr.Tag)
		case *GoTo:
			for i++; i < len(args); i++ {
				if args[i] == tr.Tag {
					break
				}
			}
		}
	}
	return nil
}
