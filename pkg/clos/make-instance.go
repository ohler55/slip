// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
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
					Name: "class",
					Type: "symbol",
					Text: "The name of the flavor or class to make an instance of.",
				},
				{Name: slip.AmpRest},
				{
					Name: "options",
					Type: "object",
					Text: `The remaining arguments must be pairs of an init-option and value.
An init-value can be the variable name prefixed with a colon or a plist option.`,
				},
			},
			Return: "instance",
			Text: `__make-instance__ makes an instance of _class_ with the provided
variable values and plist options.`,
			Examples: []string{
				"(make-instance 'strawberry :color 'red) => #<strawberry 123456>",
			},
		}, &Pkg)
}

// MakeInstance represents the makeInstance function.
type MakeInstance struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *MakeInstance) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	c := classFromArg0(f, s, args, "make-instance")
	if c.NoMake() {
		slip.NewPanic("Can not create an instance of class or flavor %s.", c.Name())
	}
	inst := c.MakeInstance().(*flavors.Instance)
	if _, ok := c.(*flavors.Flavor); ok {
		inst.Init(s, args[1:], depth)
	} else {
		for i := 1; i < len(args); i++ {
			sym, ok := args[i].(slip.Symbol)
			if !ok || len(sym) < 2 || sym[0] != ':' {
				slip.PanicType("initialization keyword", args[i], "keyword")
			}
			key := strings.ToLower(string(sym))
			if key == ":self" {
				slip.NewPanic("initialization keyword 'self' is not initable.")
			}
			i++
			val := args[i]
			vkey := key[1:]
			if inst.Has(slip.Symbol(vkey)) {
				inst.Let(slip.Symbol(vkey), val)
			} else {
				slip.NewPanic("initialization keyword '%s' is not valid for %s.", key, c.Name())
			}
		}
	}
	return inst
}

func classFromArg0(f slip.Object, s *slip.Scope, args slip.List, label string) (class slip.Class) {
	slip.ArgCountCheck(f, args, 1, -1)
	switch ta := args[0].(type) {
	case slip.Symbol:
		if cf := flavors.Find(string(ta)); cf == nil {
			if c := slip.FindClass(string(ta)); c == nil {
				slip.PanicClassNotFound(ta, "%s is not a defined class or flavor.", ta)
			} else {
				class = c
			}
		} else {
			class = cf
		}
	case *flavors.Flavor:
		class = ta
	case *Class:
		class = ta
	default:
		slip.PanicType(fmt.Sprintf("class argument to %s", label), ta, "symbol", "flavor", "class")
	}
	return
}
