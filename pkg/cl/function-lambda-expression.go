// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FunctionLambdaExpression{Function: slip.Function{Name: "function-lambda-expression", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.FunctionSymbol,
			Name: "function-lambda-expression",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "function",
					Text: "A function designator.",
				},
			},
			Return: "list,boolean,symbol",
			Text:   `__function-lambda-expression__ TBD.`,
			Examples: []string{
				"(defun quux (x) (+ x 3))",
				"(function-lambda-expression 'quux",
				";; prints:",
				"(defun quux (x)",
				"  (+ 3 x))",
			},
		}, &slip.CLPkg)
}

// FunctionLambdaExpression represents the function-lambda-expression function.
type FunctionLambdaExpression struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FunctionLambdaExpression) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	values := slip.Values{nil, nil, nil}
	a := args[0]
Top:
	switch ta := a.(type) {
	case slip.Symbol:
		if fi := slip.FindFunc(string(ta)); fi != nil {
			a = fi
			goto Top
		}
		slip.PanicType("fn", ta, "function designator")
	case *slip.Lambda:
		values[0] = append(slip.List{slip.Symbol("lambda"), f.lambdaArgs(ta.Doc)}, ta.Forms...)
		if ta.Closure != nil {
			values[1] = slip.True
		}
	case *slip.FuncInfo:
		fun := ta.Create(nil).(slip.Funky)
		if lam, ok := fun.Caller().(*slip.Lambda); ok {
			values[0] = slip.List{
				slip.Symbol("lambda"),
				f.lambdaArgs(ta.Doc),
				append(slip.List{slip.Symbol("block"), slip.Symbol(ta.Name)}, lam.Forms...),
			}
		}
		values[2] = slip.Symbol(ta.Name)
	default:
		slip.PanicType("fn", ta, "function designator")
	}
	return values
}

func (f *FunctionLambdaExpression) lambdaArgs(doc *slip.FuncDoc) slip.Object {
	largs := make(slip.List, len(doc.Args))
	for i, da := range doc.Args {
		if da.Default != nil {
			largs[i] = slip.List{slip.Symbol(da.Name), da.Default}
		} else {
			largs[i] = slip.Symbol(da.Name)
		}
	}
	return largs
}
