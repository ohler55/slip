// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"sort"

	"github.com/ohler55/slip"
)

func defMethodQualifiers() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MethodQualifiers{Function: slip.Function{Name: "method-qualifiers", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "method-qualifiers",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "method",
					Text: `A method to get the qualifiers of.`,
				},
			},
			Return: "list",
			Text:   `__method-qualifiers__ returns then qualifiers for _method_.`,
			Examples: []string{
				`(defgeneric quux (x y) (:method :before ((x real) (y real)) (print "before"))`,
				`(method-qualifiers (find-method 'quux '(:before) '(real real))) => (:before)`,
			},
		}, &Pkg)
}

// MethodQualifiers represents the method-qualifiers function.
type MethodQualifiers struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *MethodQualifiers) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	meth, ok := args[0].(*slip.Method)
	if !ok {
		slip.TypePanic(s, depth, "method", args[0], "method")
	}
	qm := map[string]bool{}
	for _, c := range meth.Combinations {
		if c.Before != nil {
			qm[":before"] = true
		}
		if c.After != nil {
			qm[":after"] = true
		}
		if c.Wrap != nil {
			qm[":around"] = true
		}
	}
	keys := make([]string, 0, len(qm))
	for k := range qm {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	quals := make(slip.List, len(keys))
	for i, k := range keys {
		quals[i] = slip.Symbol(k)
	}
	return quals
}
