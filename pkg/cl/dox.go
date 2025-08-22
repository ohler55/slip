// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Dox{Function: slip.Function{Name: "do*", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "do*",
			Args: []*slip.DocArg{
				{
					Name: "bindings",
					Type: "list",
					Text: `_((var [init-form [step-form]])*)_ Bindings are list of variables
with optional initial value form and step form. The _init-form_ evaluates to the initial
value for the _var_ variable. Subsequent iterations evaluate the _step-form_ and the result
is assigned to the _var_.`,
				},
				{
					Name: "test",
					Type: "list",
					Text: `_(end-test result-form*)_ A list of one or more forms. The first
form is the _end-test_ form that is evaluated on each iteration. If _end-form_ evaluates to
true (_t_) the iteration stops and the _result-forms_ are evaluated with the last result
returned from the __do__ call.`,
				},
				{Name: "&rest"},
				{
					Name: "statements",
					Type: "form|tag",
					Text: `Forms to evaluate on each iteraction. A __return__ can be called
to exit the loop early.`,
				},
			},
			Return: "object",
			Text: `__do*__ iterates over a set of forms until the test condition, _end-test_
returns true (_t_). The result of the __do*__ call is the last result of the _result-form_
evaluation. The initial bindings and steps are evaluated in sequence. The __do*__ function
supports __tagbody__ and __go__ in the _statements_ forms.`,
			Examples: []string{
				`(do* ((x 0 (1+ x))`,
				`     (y 0 (1- y)))`,
				`    ((> (- x y) 5) x)) => 3`,
			},
		}, &slip.CLPkg)
}

// Dox represents the do* function.
type Dox struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Dox) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, -1)
	ns := s.NewScope()
	d2 := depth + 1
	steps, test, rforms := setupDo(ns, ns, args, d2)
	for {
		if ns.Eval(test, d2) != nil {
			for _, rf := range rforms {
				result = ns.Eval(rf, d2)
			}
			break
		}
		for i := 2; i < len(args); i++ {
			switch args[i].(type) {
			case slip.List, slip.Funky:
				switch tr := slip.EvalArg(ns, args, i, d2).(type) {
				case *slip.ReturnResult:
					if tr.Tag == nil {
						return tr.Result
					}
					return tr
				case *GoTo:
					for i++; i < len(args); i++ {
						if args[i] == tr.Tag {
							break
						}
					}
				}
				// Anything other than ReturnResult or GoTo just continues.
			}
		}
		for _, sb := range steps {
			ns.UnsafeLet(sb.sym, ns.Eval(sb.step, d2))
		}
	}
	return
}
