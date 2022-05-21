// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeInstance{Function: slip.Function{Name: "make-instance", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-instance",
			Args: []*slip.DocArg{
				{
					Name: "flavor",
					Type: "symbol",
					Text: "The name of the flavor to make an instance of.",
				},
				{Name: slip.AmpRest},
				{
					Name: "options",
					Type: "object",
					Text: `The remaining arguments must be pairs of an init-option and value. An init-value can be
the variable name prefixed with a colon or a plist option.`,
				},
			},
			Return: "instance",
			Text:   `of _flavor_ with the provided variable values and plist options.`,
			Examples: []string{
				"(make-instance 'strawberry :color 'red) => #<strawberry 123456>",
			},
		}, &FlavorsPkg)
}

// MakeInstance represents the makeInstance function.
type MakeInstance struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *MakeInstance) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	var cf *Flavor
	switch ta := args[len(args)-1].(type) {
	case slip.Symbol:
		cf = allFlavors[string(ta)]
	case *Flavor:
		cf = ta
	default:
		slip.PanicType("flavor argument to make-instance", ta, "symbol", "flavor")
	}
	if cf == nil {
		panic(fmt.Sprintf("%s is not a defined flavor.", args[len(args)-1]))
	}
	inst := cf.makeInstance()
	var plist slip.List
	for i := len(args) - 2; 0 < i; i-- {
		sym, ok := args[i].(slip.Symbol)
		if !ok || len(sym) < 2 || sym[0] != ':' {
			slip.PanicType("make-instance option keyword", args[i], "keyword")
		}
		key := strings.ToLower(string(sym[1:]))
		if key == "self" {
			panic("make-instance option keyword 'self' is not initable.")
		}
		i--
		val := args[i]
		if len(cf.initable) == 0 || cf.initable[key] {
			if _, has := cf.defaultVars[key]; has {
				inst.Let(slip.Symbol(key), val)
				continue
			}
		}
		// TBD if a plist keyword then add to init-plist
	}
	_ = inst.send("init", plist, depth+1)

	return inst
}
