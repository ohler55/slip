// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
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
			Kind: slip.MacroSymbol,
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
 :default-init-plist is a list of supported keyword to make-instance for the flavor
 :default-handler is the function called when no method is found with a send call.
 :abstract-flavor indicates an instance can not be made from this flavor.
 :documentation with a string provides documentation for the flavor.
`,
				},
			},
			Return: "object",
			Text:   `__defflavor__ defines a flavor with the given _name_ in the current package.`,
			Examples: []string{
				"(defflavor strawberry (temperature) ()) => #<flavor strawberry>",
				"(defflavor blueberry ((temperature -7)) ()) => #<flavor blueberry>",
			},
		}, &Pkg)
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
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("name argument to defflavor", args[0], "symbol")
	}
	var vars slip.List
	switch tl := args[1].(type) {
	case slip.List:
		vars = tl
	case nil:
		// leave as empty list
	default:
		slip.PanicType("vars of defflavor", tl, "list")
	}

	var inherit []string
	switch tl := args[2].(type) {
	case slip.List:
		for _, a := range tl {
			if sym, ok2 := a.(slip.Symbol); ok2 {
				inherit = append(inherit, string(sym))
			} else {
				slip.PanicType("flavors of defflavor", tl, "list of symbols")
			}
		}
	case nil:
		// leave as empty list
	default:
		slip.PanicType("flavors of defflavor", tl, "list")
	}

	defVars := map[string]slip.Object{}
	for _, v := range vars {
		switch tv := v.(type) {
		case slip.Symbol:
			defVars[strings.ToLower(string(tv))] = nil
		case slip.List:
			if len(tv) != 2 {
				slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
			}
			var sym slip.Symbol
			if sym, ok = tv[0].(slip.Symbol); !ok {
				slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
			}
			if tv[1] == nil {
				defVars[strings.ToLower(string(sym))] = nil
			} else {
				defVars[strings.ToLower(string(sym))] = tv[1].Eval(s, depth+1)
			}
		default:
			slip.PanicType("vars element of defflavor", tv, "symbol", "list of symbol and value")
		}
	}
	_ = DefFlavor(string(name), defVars, inherit, args[3:], &Pkg)

	return name
}

// DefFlavor defines a new flavor.
func DefFlavor(
	name string,
	vars map[string]slip.Object,
	inherit []string,
	options slip.List,
	p *slip.Package) *Flavor {

	name = strings.ToLower(name)
	if _, has := allFlavors[name]; has {
		slip.NewPanic("Flavor %s already defined.", name)
	}
	nf := &Flavor{
		name:        name,
		defaultVars: vars,
		keywords:    map[string]slip.Object{},
		methods:     map[string][]*method{},
		initable:    map[string]bool{},
	}
	for _, fname := range inherit {
		if cf := allFlavors[strings.ToLower(fname)]; cf != nil && !cf.Final {
			nf.inheritFlavor(cf)
			if nf.defaultHandler == nil {
				nf.defaultHandler = cf.defaultHandler
			}
		} else {
			slip.PanicClassNotFound(slip.Symbol(fname), "%s is not a defined flavor.", fname)
		}
	}
	if nf.defaultHandler == nil {
		nf.defaultHandler = defHand(true)
	}
	processFlavorOptions(nf, options)
	if !nf.abstract {
		addIncludes(nf)
	}
	if !nf.noVanilla {
		nf.inheritFlavor(&vanilla)
	}
	if !nf.abstract {
		validateFlavor(nf)
	}
	allFlavors[nf.name] = nf
	_ = p.Set(nf.name, nf)
	p.Export(nf.name)

	return nf
}

func addIncludes(nf *Flavor) {
	for _, fn := range nf.included {
		if !nf.inheritsFlavor(fn) {
			if cf := allFlavors[strings.ToLower(fn)]; cf != nil {
				nf.inheritFlavor(cf)
			} else {
				slip.PanicClassNotFound(slip.Symbol(fn), "%s is not a defined flavor.", fn)
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
					slip.PanicClassNotFound(slip.Symbol(fn), "%s is not a defined flavor.", fn)
				}
			}
		}
	}
}

func validateFlavor(nf *Flavor) {
	full := make([]*Flavor, len(nf.inherit)+1)
	full[0] = nf
	copy(full[1:], nf.inherit)
	for _, cf := range full {
		for _, fn := range cf.required {
			if !nf.inheritsFlavor(fn) {
				slip.NewPanic("%s does not inherit from required flavor %s.", nf.name, fn)
			}
		}
		for _, mn := range cf.requiredMethods {
			if _, has := nf.methods[mn]; !has {
				slip.NewPanic("%s does not include the required method %s.", nf.name, mn)
			}
		}
		for _, vn := range cf.requiredVars {
			if _, has := nf.defaultVars[vn]; !has {
				slip.NewPanic("%s does not include the required variable %s.", nf.name, vn)
			}
		}
	}
}

func valsStringList(vals slip.List) (sa []string) {
	for _, v := range vals {
		if sym, ok := v.(slip.Symbol); ok {
			sa = append(sa, string(sym))
		} else {
			slip.NewPanic("%s is not a symbol.", v)
		}
	}
	return
}

func setDefaultHandler(nf *Flavor, val slip.Object) {
Top:
	switch tv := val.(type) {
	case slip.Symbol:
		if fun, ok := slip.NewFunc(string(tv), slip.List{}).(slip.Funky); ok {
			nf.defaultHandler = fun.Caller()
		}
	case *cl.Quote:
		val = tv.Args[0]
		goto Top
	case slip.List:
		if 1 < len(tv) {
			if sym, ok := tv[0].(slip.Symbol); ok {
				if strings.EqualFold("lambda", string(sym)) {
					s := slip.NewScope()
					lambdaDef := slip.ListToFunc(s, tv, 0)
					nf.defaultHandler = s.Eval(lambdaDef, 0).(*slip.Lambda)
				}
			}
		}
	default:
		slip.PanicType("defflavor default-handler", val, "symbol", "lambda")
	}
}

func processFlavorOptions(nf *Flavor, options slip.List) {
	// Order of options doesn't matter so process the easy way.
	for _, opt := range options {
		var key slip.Symbol
		var vals slip.List
		switch to := opt.(type) {
		case slip.Symbol:
			key = to
		case slip.List:
			if len(to) < 2 {
				slip.PanicType("options element of defflavor", opt, "symbol", "list of symbol and values")
			}
			var ok bool
			if key, ok = to[0].(slip.Symbol); !ok {
				slip.PanicType("options element of defflavor", opt, "symbol", "list of symbol and values")
			}
			vals = to[1:]
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
			setDefaultHandler(nf, vals[len(vals)-1])
		case ":default-init-plist":
			for _, v := range vals {
				if plist, ok := v.(slip.List); ok && len(plist) == 2 {
					if sym, ok := plist[0].(slip.Symbol); ok {
						if sym == slip.Symbol(":allow-other-keys") {
							nf.allowOtherKeys = plist[1] == slip.True
						} else {
							nf.keywords[string(sym)] = plist[1]
						}
					} else {
						slip.NewPanic("In :default-init-plist list car, %s is not a symbol.", plist[0])
					}
				} else {
					slip.NewPanic("In :default-init-plist, %s is not a list.", v)
				}
			}
		case ":documentation":
			if 0 < len(vals) {
				if ss, ok := vals[0].(slip.String); ok {
					nf.docs = string(ss)
					break
				}
				slip.PanicType("defflavor :documentation", vals[0], "string")
			}
		case ":gettable-instance-variables", ":readable-instance-variables":
			if 0 < len(vals) {
				for _, v := range vals {
					if sym, ok := v.(slip.Symbol); ok {
						nf.DefMethod(":"+string(sym), "", getter(sym))
					} else {
						slip.NewPanic("%s is not a symbol.", v)
					}
				}
			} else {
				for k := range nf.defaultVars {
					if k != "self" {
						nf.DefMethod(":"+k, "", getter(k))
					}
				}
			}
		case ":included-flavors":
			nf.included = valsStringList(vals)
		case ":initable-instance-variables", ":inittable-instance-variables":
			if 0 < len(vals) {
				for _, v := range vals {
					if sym, ok := v.(slip.Symbol); ok {
						nf.initable[":"+string(sym)] = true
					} else {
						slip.NewPanic("%s is not a symbol.", v)
					}
				}
			} else {
				for k := range nf.defaultVars {
					if k != "self" {
						nf.initable[":"+k] = true
					}
				}
			}
		case ":init-keywords":
			for _, v := range vals {
				if sym, ok := v.(slip.Symbol); ok {
					nf.keywords[string(sym)] = nil
				} else {
					slip.NewPanic("%s is not a symbol.", v)
				}
			}
		case ":no-vanilla-flavor":
			nf.noVanilla = true
		case ":required-flavors":
			nf.required = valsStringList(vals)
		case ":required-init-keywords":
			nf.requiredKeywords = valsStringList(vals)
		case ":required-instance-variables":
			nf.requiredVars = valsStringList(vals)
		case ":required-methods":
			nf.requiredMethods = valsStringList(vals)
		case ":settable-instance-variables", ":writable-instance-variables":
			if 0 < len(vals) {
				for _, v := range vals {
					if sym, ok := v.(slip.Symbol); ok {
						nf.DefMethod(":set-"+string(sym), "", setter(sym))
					} else {
						slip.NewPanic("%s is not a symbol.", v)
					}
				}
			} else {
				for k := range nf.defaultVars {
					if k != "self" {
						nf.DefMethod(":set-"+k, "", setter(k))
					}
				}
			}
		default:
			slip.NewPanic("%s is not an option to defflavor", key)
		}
	}
}

type getter string

// Call returns the value of a variable in the instance.
func (g getter) Call(scope *slip.Scope, args slip.List, _ int) slip.Object {
	return scope.Get(slip.Symbol(g))
}

// BoundCall the caller member.
func (g getter) BoundCall(scope *slip.Scope, _ int) slip.Object {
	return scope.Get(slip.Symbol(g))
}

type setter string

// Call sets the value of a variable in the instance.
func (s setter) Call(scope *slip.Scope, args slip.List, _ int) slip.Object {
	if len(args) < 1 {
		slip.NewPanic("no value given for set-%s.", s)
	}
	scope.Set(slip.Symbol(s), args[0])

	return args[0]
}

// BoundCall the caller member.
func (s setter) BoundCall(scope *slip.Scope, _ int) (value slip.Object) {
	for _, value := range scope.Vars {
		scope.Set(slip.Symbol(s), value)
		return value
	}
	defer slip.NewPanic("no value given for set-%s.", s)
	return nil
}

type defHand bool

// Call the default handler.
func (dh defHand) Call(scope *slip.Scope, args slip.List, _ int) slip.Object {
	inst := scope.Get(slip.Symbol("self")).(*Instance)
	method, _ := args[0].(slip.Symbol)
	defer slip.PanicUnboundSlot(inst, method,
		"Flavor %s does not include the %s method.", inst.Flavor.name, args[0])
	return nil
}
