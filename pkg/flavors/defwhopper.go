// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defwhopper{Function: slip.Function{Name: "defwhopper", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defwhopper",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "list",
					Text: "A list of the flavor name and operation (method name) .",
				},
				{
					Name: "args",
					Type: "lambda-list",
					Text: `The arguments to the whopper. A standard lambda-list`,
				},
				{Name: slip.AmpRest},
				{
					Name: "forms",
					Type: "object",
					Text: `The forms that process the whopper.`,
				},
			},
			Return: "nil",
			Text:   `__defwhopper__ defines a whopper for a flavor method.`,
			Examples: []string{
				"(defflavor strawberry (size) ()) => #<flavor strawberry>",
				`(defwhopper (strawberry :size) () (format t "getting size") (continue-whopper))  => nil`,
			},
		}, &Pkg)
}

// Defwhopper represents the defwhopper function.
type Defwhopper struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defwhopper) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	pos := len(args) - 1
	if pos < 1 {
		slip.PanicArgCount(f, 2, -1)
	}
	ml, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("whopper designator for defwhopper", args[pos], "list")
	}
	var (
		flavor *Flavor
		method string
	)
	switch len(ml) {
	case 0, 1:
		panic(fmt.Sprintf("Too few elements in the whopper for defwhopper. Expected 2 but got %d.", len(ml)))
	case 2:
		// okay
	default:
		panic(fmt.Sprintf("Too many elements in the whopper for defwhopper. Expected 2 but got %d.", len(ml)))
	}
	if sym, ok2 := ml[1].(slip.Symbol); ok2 {
		flavor = allFlavors[string(sym)]
	}
	if flavor == nil {
		slip.PanicType("flavor for defwhopper", ml[1], "name of flavor")
	}
	if sym, ok2 := ml[0].(slip.Symbol); ok2 && 1 < len(sym) && sym[0] == ':' {
		method = string(sym)
	} else {
		slip.PanicType("method for defwhopper", ml[0], "keyword")
	}
	if 2 < pos { // method, method-args, docs, forms
		var str slip.String
		if str, ok = args[pos-2].(slip.String); ok {
			list := make(slip.List, len(args)-2)
			list[len(list)-1] = args[pos-1]
			copy(list[:len(list)-1], args[:pos-1])
			flavor.DefMethod(
				method,
				":whopper",
				&Daemon{
					caller: slip.DefLambda(method, s, list),
					docs:   string(str),
				})
			return
		}
	}
	flavor.DefMethod(method, ":whopper", slip.DefLambda(method, s, args[:pos]))

	return nil
}
