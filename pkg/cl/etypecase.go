// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Etypecase{Function: slip.Function{Name: "etypecase", Args: args, SkipEval: []bool{false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "etypecase",
			Args: []*slip.DocArg{
				{
					Name: "keyform*",
					Type: "for",
					Text: `Form that evaluates to a key form.`,
				},
				{Name: "&rest"},
				{
					Name: "clause*",
					Type: "list",
					Text: `Each clause _(type-designator form*)_ takes the form of a type-designator and
zero of more forms to be evaluated if the key from the _keyform_ is a type that is a type-designator
or a list with a member that is of type-designator.`,
				},
			},
			Return: "object",
			Text: `__etypecase__ evaluates each _keyform_ until a _clause_ _type-designator_
matches one of the _keyform_. A type-designator of __otherwise__ or __t__ matches any key.
If there are no matches an error is raised.`,
			Examples: []string{
				"(etypecase 3 (float 'float) (fixnum 'integer)) => integer",
			},
		}, &slip.CLPkg)
}

// Etypecase represents the etypecase function.
type Etypecase struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Etypecase) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	key := args[0]
	d2 := depth + 1
	for _, a := range args[1:] {
		clause, ok := a.(slip.List)
		if !ok || len(clause) == 0 {
			slip.PanicType("clause", a, "list")
		}
		var sym slip.Symbol
		if sym, ok = clause[0].(slip.Symbol); ok {
			if strings.EqualFold("otherwise", string(sym)) {
				sym = slip.TrueSymbol
			}
		} else if clause[0] == slip.True {
			sym = slip.TrueSymbol
		} else {
			slip.PanicType("clause key", clause[0], "symbol", "t", "otherwise")
		}
		if !typecaseMatch(sym, key) {
			continue
		}
		for i := 1; i < len(clause); i++ {
			result = slip.EvalArg(s, clause, i, d2)
		}
		return
	}
	var wants []string
	for _, a := range args[1:] {
		et := a.(slip.List)[0]
		if sym, ok := et.(slip.Symbol); ok {
			wants = append(wants, string(sym))
		}
	}
	wants = append(wants, "t")
	panic(slip.NewTypeError(s, depth, "test-key", key, wants...))
}
