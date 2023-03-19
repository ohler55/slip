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
					Text: `The remaining arguments must be pairs of an init-option and value.
An init-value can be the variable name prefixed with a colon or a plist option.`,
				},
			},
			Return: "instance",
			Text: `__make-instance__ makes an instance of _flavor_ with the provided
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
	inst, plist := allocateInstance(f, s, args, "make-instance")

	_ = inst.Receive(":init", slip.List{plist}, depth+1)

	return inst
}

func allocateInstance(f slip.Object, s *slip.Scope, args slip.List, label string) (*Instance, slip.List) {
	slip.ArgCountCheck(f, args, 1, -1)
	var cf *Flavor
	switch ta := args[0].(type) {
	case slip.Symbol:
		cf = allFlavors[string(ta)]
	case *Flavor:
		cf = ta
	default:
		slip.PanicType(fmt.Sprintf("flavor argument to %s", label), ta, "symbol", "flavor")
	}
	if cf == nil {
		panic(fmt.Sprintf("%s is not a defined flavor.", args[0]))
	}
	if cf.abstract {
		panic(fmt.Sprintf("Can not create an instance of abstract flavor %s.", cf.name))
	}
	inst := cf.MakeInstance()
	var plist slip.List
	keys := map[string]bool{}
	for i := 1; i < len(args); i++ {
		sym, ok := args[i].(slip.Symbol)
		if !ok || len(sym) < 2 || sym[0] != ':' {
			slip.PanicType(fmt.Sprintf("%s option keyword", label), args[i], "keyword")
		}
		key := strings.ToLower(string(sym))
		if key == ":self" {
			panic(fmt.Sprintf("%s option keyword 'self' is not initable.", label))
		}
		i++
		val := args[i]
		if len(cf.initable) == 0 || cf.initable[key] {
			vkey := key[1:]
			if _, has := cf.defaultVars[vkey]; has {
				inst.Let(slip.Symbol(vkey), val)
				continue
			}
		}
		keys[key] = true
		if cf.allowOtherKeys {
			plist = append(plist, val, sym)
		} else if _, has := cf.keywords[key]; has {
			plist = append(plist, val, sym)
		} else {
			panic(fmt.Sprintf("%s is not an init keyword for flavor %s.", sym, cf.name))
		}
	}
	for _, k := range cf.requiredKeywords {
		if !keys[k] {
			panic(fmt.Sprintf("Keyword %s missing from %s for flavor %s.", k, label, cf.name))
		}
	}
	return inst, plist
}
