// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defDefgeneric() {
	Pkg.Define(
		func(args slip.List) slip.Object {
			f := Defgeneric{Function: slip.Function{Name: "defgeneric", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "defgeneric",
			Kind: slip.GenericFunctionSymbol,
			Args: []*slip.DocArg{
				{
					Name: "function-name",
					Type: "symbol",
					Text: "The name of the generic function to define.",
				},
				{
					Name: "gf-lambda-list",
					Type: "list",
					Text: `A list of the symbols that are argument names or
_&optional_, _&rest_, _&key_, or _&allow-other-keys_.`,
				},
				{Name: slip.AmpRest},
				{
					Name: "options",
					Type: "list",
					Text: `Options lists that begin with a keyword. Option keywords can be:
  __:documentation__ followed by a string that documents the generic function
  __:argument-precedence-order__ not supported
  __declare__ ignored
  __:method-combination__ not supported
  __:generic-function-class__ not supported
  __:method-class__ not supported
  __:method__ is the same as __defmethod__ with the remaining list elements of method-qualifiers,
specialize-lambda-list, optional documentation, and the forms that make up the method.
`,
				},
			},
			Return: "generic-function",
			Text:   `__defgeneric__ TBD`,
		},
	)
}

// Defgeneric represents the class-name function.
type Defgeneric struct {
	slip.Function
}

type genfun struct {
	slip.Function
	aux *Aux
}

// Call the the function with the arguments provided.
func (gf *genfun) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return gf.aux.Call(gf, s, args, depth)
}

// Call the the function with the arguments provided.
func (f *Defgeneric) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	slip.ArgCountCheck(f, args, 2, -1)
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("function-name", args[0], "symbol")
	}
	var ll slip.List
	if ll, ok = args[1].(slip.List); !ok {
		slip.PanicType("gf-lambda-list", args[1], "list")
	}
	fd := slip.FuncDoc{
		Name:   string(name),
		Kind:   slip.GenericFunctionSymbol,
		Args:   make([]*slip.DocArg, len(ll)),
		Return: "object",
	}
	for i, v := range ll {
		var sym slip.Symbol
		if sym, ok = v.(slip.Symbol); !ok {
			slip.PanicType("gf-lambda-list element", v, "symbol")
		}
		fd.Args[i] = &slip.DocArg{Name: string(sym)}
	}
	// TBD options
	// docs
	// method

	aux := NewAux(&fd)
	return slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := genfun{Function: slip.Function{Name: string(name), Args: args}, aux: aux}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
}
