// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defmethod{Function: slip.Function{Name: "defmethod", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defmethod",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "list",
					Text: "A list of the flavor name, optional method type, and operation (method name) .",
				},
				{
					Name: "args",
					Type: "lambda-list",
					Text: `The arguments to the method. A standard lambda-list`,
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: `Documentation for the method.`,
				},
				{Name: slip.AmpRest},
				{
					Name: "forms",
					Type: "object",
					Text: `The forms that process the method.`,
				},
			},
			Return: "nil",
			Text:   `__defmethod__ defines a method for a flavor.`,
			Examples: []string{
				"(defflavor strawberry (size) ()) => #<flavor strawberry>",
				`(defmethod (strawberry :before :size) () (format t "getting size"))  => nil`,
			},
		}, &Pkg)
}

// Defmethod represents the defmethod function.
type Defmethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defmethod) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	pos := len(args) - 1
	if pos < 1 {
		slip.PanicArgCount(f, 2, -1)
	}
	ml, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("method designator for defmethod", args[pos], "list")
	}
	var (
		flavor *Flavor
		daemon string
		method string
	)
	switch len(ml) {
	case 0, 1:
		panic(fmt.Sprintf("Too few elements in the method for defmethod. Expected 2 or 3 but got %d.", len(ml)))
	case 2:
		if sym, ok2 := ml[1].(slip.Symbol); ok2 {
			flavor = allFlavors[string(sym)]
		}
	case 3:
		if sym, ok2 := ml[2].(slip.Symbol); ok2 {
			flavor = allFlavors[string(sym)]
		}
		if sym, ok2 := ml[1].(slip.Symbol); ok2 {
			daemon = string(sym)
		}
	default:
		panic(fmt.Sprintf("Too many elements in the method for defmethod. Expected 2 or 3 but got %d.", len(ml)))
	}
	if sym, ok2 := ml[0].(slip.Symbol); ok2 && 1 < len(sym) && sym[0] == ':' {
		method = string(sym)
	} else {
		slip.PanicType("method for defmethod", ml[0], "keyword")
	}
	if flavor == nil {
		slip.PanicType("flavor for defmethod", ml[len(ml)-1], "name of flavor")
	}
	if 2 < pos { // method, method-args, docs, forms
		var str slip.String
		if str, ok = args[pos-2].(slip.String); ok {
			list := make(slip.List, len(args)-2)
			list[len(list)-1] = args[pos-1]
			copy(list[:len(list)-1], args[:pos-1])
			flavor.DefMethod(
				method,
				daemon,
				&Daemon{
					caller: slip.DefLambda(method, s, list),
					docs:   string(str),
				})
			return
		}
	}
	flavor.DefMethod(method, daemon, slip.DefLambda(method, s, args[:pos]))

	return nil
}
