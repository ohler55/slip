// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
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
	key := strings.ToLower(string(name))
	if _, has := allFlavors[key]; has {
		panic(fmt.Sprintf("Flavor %s already defined.", name))
	}
	nf := &Flavor{
		name:           key,
		defaultVars:    map[string]slip.Object{},
		keywords:       map[string]slip.Object{},
		methods:        map[string][]*method{},
		initable:       map[string]bool{},
		defaultHandler: defHand(true),
	}
	switch tl := args[pos].(type) {
	case slip.List:
		for i := len(tl) - 1; 0 <= i; i-- {
			if sym, ok2 := tl[i].(slip.Symbol); ok2 {
				if cf := allFlavors[strings.ToLower(string(sym))]; cf != nil {
					nf.inheritFlavor(cf)
				} else {
					panic(fmt.Sprintf("Flavor %s not defined.", sym))
				}
			} else {
				slip.PanicType("flavors of defflavor", args[pos], "list of symbols")
			}
		}
	case nil:
		// leave as empty list
	default:
		slip.PanicType("flavors of defflavor", args[pos], "list")
	}
	for i := len(vars) - 1; 0 <= i; i-- {
		switch tv := vars[i].(type) {
		case slip.Symbol:
			nf.defaultVars[strings.ToLower(string(tv))] = nil
		case slip.List:
			if len(tv) != 2 {
				slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
			}
			var sym slip.Symbol
			if sym, ok = tv[1].(slip.Symbol); !ok {
				slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
			}
			if tv[0] == nil {
				nf.defaultVars[strings.ToLower(string(sym))] = nil
			} else {
				nf.defaultVars[strings.ToLower(string(sym))] = tv[0].Eval(s, depth+1)
			}
		default:
			slip.PanicType("xx vars element of defflavor", tv, "symbol", "list of symbol and value")
		}
	}
	f.processOptions(nf, args[:pos])
	if !nf.abstract {
		f.addIncludes(nf)
		f.validateFlavor(nf)
	}
	if !nf.noVanilla {
		nf.inheritFlavor(&vanilla)
	}
	allFlavors[nf.name] = nf
	FlavorsPkg.Set(string(name), nf)

	return name
}

func (f *Defflavor) addIncludes(nf *Flavor) {
	for _, fn := range nf.included {
		if !nf.inheritsFlavor(fn) {
			if cf := allFlavors[strings.ToLower(fn)]; cf != nil {
				nf.inheritFlavor(cf)
			} else {
				panic(fmt.Sprintf("Flavor %s not defined.", fn))
			}
		}
	}
	// Use an index since nf.inherit may grow during the iteration.
	for i := 0; i < len(nf.inherit); i++ {
		for _, fn := range nf.inherit[i].included {
			if !nf.inheritsFlavor(fn) {
				if cf := allFlavors[strings.ToLower(fn)]; cf != nil {
					nf.inheritFlavor(cf)
				} else {
					panic(fmt.Sprintf("Flavor %s not defined.", fn))
				}
			}
		}
	}
}

func (f *Defflavor) validateFlavor(nf *Flavor) {
	full := make([]*Flavor, len(nf.inherit)+1)
	full[0] = nf
	copy(full[1:], nf.inherit)
	for _, cf := range full {
		for _, fn := range cf.required {
			if !nf.inheritsFlavor(fn) {
				panic(fmt.Sprintf("%s does not inherit from required flavor %s.", nf.name, fn))
			}
		}
		for _, mn := range cf.requiredMethods {
			if _, has := nf.methods[mn]; !has {
				panic(fmt.Sprintf("%s does not include the required method %s.", nf.name, mn))
			}
		}
		for _, vn := range cf.requiredVars {
			if _, has := nf.defaultVars[vn]; !has {
				panic(fmt.Sprintf("%s does not include the required variable %s.", nf.name, vn))
			}
		}
	}
}

func (f *Defflavor) valsStringList(vals slip.List) (sa []string) {
	for i := len(vals) - 1; 0 <= i; i-- {
		if sym, ok := vals[i].(slip.Symbol); ok {
			sa = append(sa, string(sym))
		} else {
			panic(fmt.Sprintf("%s is not a symbol.", vals[i]))
		}
	}
	return
}

func (f *Defflavor) setDefaultHandler(nf *Flavor, val slip.Object) {
Top:
	switch tv := val.(type) {
	case slip.Symbol:
		if fun, ok := slip.NewFunc(string(tv), slip.List{}).(slip.Funky); ok {
			nf.defaultHandler = fun.Caller()
		}
	case *cl.Quote:
		val = tv.Args[0]
		goto Top
	case slip.Funky:
		nf.defaultHandler = tv.Caller()
	default:
		// TBD lambda
		slip.PanicType("defflavor default-handler", val, "symbol", "lambda")
	}
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
			if len(vals) != 1 {
				slip.PanicType(":default-handler of defflavor", nil, "symbol")
			}
			f.setDefaultHandler(nf, vals[0])
		case ":default-init-plist":
			for i := len(vals) - 1; 0 <= i; i-- {
				if plist, ok := vals[i].(slip.List); ok && len(plist) == 2 {
					if sym, ok := plist[1].(slip.Symbol); ok {
						if sym == slip.Symbol("allow-other-keys") {
							nf.allowOtherKeys = plist[0] == slip.True
						} else {
							nf.keywords[string(sym)] = plist[0]
						}
					} else {
						panic(fmt.Sprintf("In :default-init-plist list car, %s is not a symbol.", plist[1]))
					}
				} else {
					panic(fmt.Sprintf("In :default-init-plist, %s is not a list.", vals[i]))
				}
			}
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
			for i := len(vals) - 1; 0 <= i; i-- {
				if sym, ok := vals[i].(slip.Symbol); ok {
					nf.initable[string(sym)] = true
				} else {
					panic(fmt.Sprintf("%s is not a symbol.", vals[i]))
				}
			}
		case ":init-keywords":
			for i := len(vals) - 1; 0 <= i; i-- {
				if sym, ok := vals[i].(slip.Symbol); ok {
					nf.keywords[string(sym)] = nil
				} else {
					panic(fmt.Sprintf("%s is not a symbol.", vals[i]))
				}
			}
		case ":no-vanilla-flavor":
			nf.noVanilla = true
		case ":required-flavors":
			nf.required = f.valsStringList(vals)
		case ":required-init-keywords":
			nf.requiredKeywords = f.valsStringList(vals)
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

type getter string

// Call returns the value of a variable in the instance.
func (g getter) Call(scope *slip.Scope, args slip.List, _ int) slip.Object {
	return scope.Get(slip.Symbol(g))
}

type setter string

// Call sets the value of a variable in the instance.
func (s setter) Call(scope *slip.Scope, args slip.List, _ int) slip.Object {
	if len(args) < 1 {
		panic(fmt.Sprintf("no value given for set-%s.", s))
	}
	scope.Set(slip.Symbol(s), args[0])

	return args[0]
}

type defHand bool

// Call the default handler.
func (dh defHand) Call(scope *slip.Scope, args slip.List, _ int) slip.Object {
	inst := scope.Get(slip.Symbol("self")).(*Instance)
	panic(fmt.Sprintf("Flavor %s does not include the %s method.", inst.flavor.name, args[len(args)-1]))
}
