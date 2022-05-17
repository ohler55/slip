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
			f := Defflavor{Function: slip.Function{Name: "defflavor", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "defflavor",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the flavor being defined.",
				},
				{
					Name: "vars",
					Type: "list",
					Text: `A list of the flavor's variables. _vars_ elements can be a symbol or a list of a symbol
and a default value.`,
				},
				{
					Name: "flavors",
					Type: "list",
					Text: `A list of the flavor's inherited flavors. _flavors_ elements must be symbols.`,
				},
				{Name: slip.AmpRest},
				{
					Name: "options",
					Type: "symbol|list",
					Text: `Options with arguments when needed. Options are:
 :gettable-instance-variables creates a getter method for each variable.
 :settable-instance-variables creates a setter method for each variable.
 :initable-instance-variables allows the listed variables to be given initial values. If none are listed then all
variables can be provided an initial value. (also spelled :inittable-instance-variables)
 :required-instance-variables lists the variables that must be present in any flavor inheriting this flavor.
 :required-method lists the methods that must be included in any flavor inheriting this flavor.
 :required-flavors lists the flavor names that must be included in any flavor inheriting this flavor.
 :included-flavors lists the flavor names that must be included. If not included they are automatically included.
 :no-vanilla-flavor will not inherit the vanilla-flavor.
 :default-handler is the function called when no method is found with a send call.
 :abstract-flavor indicates an instance can not be made from this flavor.
 :documentation with a string provides documentation for the flavor.
`,
				},
			},
			Return: "object",
			Text:   `defines a flavor with the given _name_ in the current package.`,
			Examples: []string{
				"(defflavor strawberry (temperature) ()) => #<flavor strawberry>",
			},
		}, &FlavorsPkg)
}

// Defflavor represents the defflavor function.
type Defflavor struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defflavor) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 3 {
		slip.PanicArgCount(f, 3, 14)
	}
	pos := len(args) - 1
	name, ok := args[pos].(slip.Symbol)
	if !ok {
		slip.PanicType("name argument to defflavor", args[pos], "symbol")
	}
	pos--
	var vars slip.List
	switch tl := args[pos].(type) {
	case slip.List:
		vars = tl
	case nil:
		// leave as empty list
	default:
		slip.PanicType("vars of defflavor", args[pos], "list")
	}
	pos--
	var inherit []slip.Symbol
	switch tl := args[pos].(type) {
	case slip.List:
		for i := len(tl) - 1; 0 <= i; i-- {
			if sym, ok2 := tl[i].(slip.Symbol); ok2 {
				inherit = append(inherit, sym)
			} else {
				slip.PanicType("flavors of defflavor", args[pos], "list of symbols")
			}
		}
	case nil:
		// leave as empty list
	default:
		slip.PanicType("flavors of defflavor", args[pos], "list")
	}
	nf := defFlavor(name, inherit...)
	for i := len(vars) - 1; 0 <= i; i-- {
		switch tv := vars[i].(type) {
		case slip.Symbol:
			nf.addVar(tv, nil)
		case slip.List:
			if len(tv) != 2 {
				slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
			}
			var sym slip.Symbol
			if sym, ok = tv[1].(slip.Symbol); !ok {
				slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
			}
			if tv[0] == nil {
				nf.addVar(sym, nil)
			} else {
				nf.addVar(sym, tv[0].Eval(s, depth+1))
			}
		default:
			slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
		}
	}
	f.processOptions(nf, args[:pos])

	// TBD if not abstract then add included
	// TBD validate

	return name
}

func (f *Defflavor) valsStringList(vals slip.List) (sa []string) {
	for i := len(vals) - 1; 0 <= i; i-- {
		if sym, ok := vals[i].(slip.Symbol); ok {
			sa = append(sa, string(sym))
		} else {
			panic(fmt.Sprintf("%s is not a flavor.", vals[i]))
		}
	}
	return
}

func (f *Defflavor) processOptions(nf *Flavor, options slip.List) {
	// Order of options doesn't matter so process the easy way.
	for _, opt := range options {
		var key slip.Symbol
		var vals slip.List
		switch to := opt.(type) {
		case slip.Symbol:
			key = to
		case slip.List:
			if len(to) < 1 {
				slip.PanicType("options element of defflavor", opt, "symbol", "list of symbol and values")
			}
			var ok bool
			if key, ok = to[len(to)-1].(slip.Symbol); !ok {
				slip.PanicType("options element of defflavor", opt, "symbol", "list of symbol and values")
			}
			vals = to[:len(to)-1]
		default:
			slip.PanicType("options element of defflavor", opt, "symbol", "list of symbol and values")
		}
		switch strings.ToLower(string(key)) {
		case ":abstract-flavor":
			nf.abstract = true
		case ":default-handler":
			if len(vals) == 1 {
				if sym, ok := vals[0].(slip.Symbol); ok {
					if fun, ok2 := slip.NewFunc(string(sym), slip.List{}).(slip.Funky); ok2 {
						nf.defaultHandler = fun.Caller()
					}
				}
			}
			slip.PanicType(":default-handler of defflavor", nil, "symbol")
		case ":documentation":
			if ss, ok := vals[0].(slip.String); ok {
				nf.docs = string(ss)
			} else {
				slip.PanicType("defflavor :documentation", vals[0], "string")
			}
		case ":gettable-instance-variables":
			for k := range nf.defaultVars {
				if k != "self" {
					nf.defMethod(":"+k, "", getter(k))
				}
			}
		case ":included-flavors":
			nf.included = f.valsStringList(vals)
		case ":initable-instance-variables", ":inittable-instance-variables":
			nf.initable = f.valsStringList(vals)
		case ":no-vanilla-flavor":
			f.removeVanilla(nf)
			// TBD remove vanilla and methods

		case ":required-flavors":
			nf.required = f.valsStringList(vals)
		case ":required-instance-variables":
			nf.requiredVars = f.valsStringList(vals)
		case ":required-method":
			nf.requiredMethods = f.valsStringList(vals)
		case ":settable-instance-variables":
			for k := range nf.defaultVars {
				if k != "self" {
					nf.defMethod(":set-"+k, "", setter(k))
				}
			}
		default:
			panic(fmt.Sprintf("%s is not an option to defflavor", key))
		}
	}
}

func (f *Defflavor) removeVanilla(nf *Flavor) {
	// The vanilla-flavor should always be the last flavor on the inherit list
	// so check and remove.
	if 0 < len(nf.inherit) && nf.inherit[len(nf.inherit)-1] == &vanilla {
		nf.inherit = nf.inherit[:len(nf.inherit)-1]
		for k, m := range nf.methods {
			if 0 < len(m.inherit) && m.inherit[len(m.inherit)-1].from == &vanilla {
				m.inherit = m.inherit[:len(m.inherit)-1]
			}
			if len(m.inherit) == 0 && m.primary == nil && m.before == nil && m.after == nil && m.wrap == nil {
				delete(nf.methods, k)
			}
		}
	}
}

type getter string

// Call returns the value of a variable in the instance.
func (g getter) Call(_ *slip.Scope, args slip.List, _ int) slip.Object {
	return args[len(args)-1].(*Instance).scope.Get(slip.Symbol(g))
}

type setter string

// Call sets the value of a variable in the instance.
func (s setter) Call(_ *slip.Scope, args slip.List, _ int) slip.Object {
	if len(args) < 3 {
		panic(fmt.Sprintf("no value given for %s.", s))
	}
	args[len(args)-1].(*Instance).scope.Set(slip.Symbol(s), args[0])

	return args[0]
}
