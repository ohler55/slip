// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defFunctionKeywords() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FunctionKeywords{Function: slip.Function{Name: "function-keywords", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "function-keywords",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "method",
					Text: `A method to get the keywords of.`,
				},
			},
			Return: "list,boolean",
			Text: `__function-keywords__ returns a list of the keywords for _method_ along with a
boolean indicating whether &allow-other-keys was specified.`,
			Examples: []string{
				`(defmethod quux ((a fixnum) &key (b 3) (c 4) d) (list a b c d))`,
				`(function-keywords (find-method 'quux '() '(fixnum))) => (:b :c :d)`,
			},
		}, &Pkg)
}

// FunctionKeywords represents the function-keywords function.
type FunctionKeywords struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FunctionKeywords) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	meth, ok := args[0].(*slip.Method)
	if !ok {
		slip.PanicType("method", args[0], "method")
	}
	var (
		keys       slip.List
		allowOther slip.Object
	)
	if meth.Doc != nil {
		var collect bool
		for _, da := range meth.Doc.Args {
			if collect {
				if da.Name == "&allow-other-keys" {
					allowOther = slip.True
				} else {
					keys = append(keys, slip.Symbol(":"+da.Name))
				}
			} else if da.Name == "&key" {
				collect = true
			}
		}
	}
	return slip.Values{keys, allowOther}
}
