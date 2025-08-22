// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defRemoveMethod() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RemoveMethod{Function: slip.Function{Name: "remove-method", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "remove-method",
			Args: []*slip.DocArg{
				{
					Name: "generic-function",
					Type: "generic-function",
					Text: `A generic-function or designator.`,
				},
				{
					Name: "method",
					Type: "method",
					Text: `A method of the generic function.`,
				},
			},
			Return: "generic-function",
			Text:   `__remove-method__ removes the _method_ of the _generic-function_.`,
			Examples: []string{
				`(defgeneric quux (x y)`,
				`  (:method :before ((x real) (y real)) (print "before"))`,
				`  (:method :after ((x real) (y real)) (print "after")))`,
				`(remove-method 'quux (find-method 'quux '(:before) '(real real))) => #<generic-function quux>`,
				`(pretty-print quux nil) =>`,
				` (defgeneric quux (x y)`,
				`   (:method :after ((x real) (y real))`,
				`     (print "after")))`,
			},
		}, &Pkg)
}

// RemoveMethod represents the remove-method function.
type RemoveMethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *RemoveMethod) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	var aux *Aux
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case slip.Symbol:
		a0 = slip.FindFunc(string(ta))
		goto top
	case *slip.FuncInfo:
		aux, _ = ta.Aux.(*Aux)
	}
	if aux == nil {
		slip.TypePanic(s, depth, "generic-function", args[0], "symbol", "generic-function")
	}
	meth, ok := args[1].(*slip.Method)
	if !ok || len(meth.Combinations) == 0 {
		slip.TypePanic(s, depth, "method", args[1], "method")
	}
	if meth.Name != aux.docs.Name {
		return args[0]
	}
	var key []byte
	for i, da := range meth.Doc.Args {
		if da.Name[0] == '&' {
			break
		}
		if 0 < i {
			key = append(key, '|')
		}
		key = append(key, da.Type...)
	}
	aux.moo.Lock()
	defer aux.moo.Unlock()
	if gmeth := aux.methods[string(key)]; gmeth != nil && 0 < len(gmeth.Combinations) {
		comb := meth.Combinations[0]
		gcomb := gmeth.Combinations[0]
		// Just one of the daemon callers of meth should be set.
		if comb.Primary != nil {
			gcomb.Primary = nil
		}
		if comb.Before != nil {
			gcomb.Before = nil
		}
		if comb.After != nil {
			gcomb.After = nil
		}
		if comb.Wrap != nil {
			gcomb.Wrap = nil
		}
		// If no more daemons in the generic method combination then remove
		// the combination.
		if gcomb.Primary == nil && gcomb.Before == nil && gcomb.After == nil && gcomb.Wrap == nil {
			gmeth.Combinations = gmeth.Combinations[:len(gmeth.Combinations)-1]
			if len(gmeth.Combinations) == 0 {
				delete(aux.methods, string(key))
			}
		}
		if 0 < len(aux.cache) { // clear cache
			aux.cache = map[string]*slip.Method{}
		}
		aux.updateDefaultCaller()
	}
	return args[0]
}
