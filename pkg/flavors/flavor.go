// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

// FlavorSymbol is the symbol with a value of "flavor".
const FlavorSymbol = slip.Symbol("flavor")

var allFlavors = map[string]*Flavor{vanilla.name: &vanilla}

func init() {
	slip.DefConstant(FlavorSymbol, FlavorSymbol,
		`A _flavor_ encapsulates a class of objects.`)
}

// Flavor of Objects.
type Flavor struct {
	name            string
	docs            string
	inherit         []*Flavor
	defaultVars     map[string]slip.Object
	methods         map[string]*method
	included        []string
	required        []string
	requiredMethods []string
	requiredVars    []string
	initable        []string
	defaultHandler  slip.Caller
	abstract        bool
	noVanilla       bool
}

func defFlavor(name slip.Symbol, inherit ...slip.Symbol) *Flavor {
	key := strings.ToLower(string(name))
	if _, has := allFlavors[key]; has {
		panic(fmt.Sprintf("Flavor %s already defined.", name))
	}
	f := Flavor{
		name:        key,
		defaultVars: map[string]slip.Object{},
		methods:     map[string]*method{},
	}
	for _, sym := range inherit {
		if cf := allFlavors[strings.ToLower(string(sym))]; cf != nil {
			f.inheritFlavor(cf)
		} else {
			panic(fmt.Sprintf("Flavor %s not defined.", sym))
		}
	}
	allFlavors[key] = &f
	FlavorsPkg.Set(string(name), &f)
	return &f
}

func (obj *Flavor) addVar(name slip.Symbol, val slip.Object) {
	obj.defaultVars[strings.ToLower(string(name))] = val
}

func (obj *Flavor) defMethod(name string, methodType string, caller slip.Caller) {
	name = strings.ToLower(name)
	m := obj.methods[name]
	add := false
	if m == nil {
		add = true
		m = &method{
			name: name,
			from: obj,
		}
	}
	switch strings.ToLower(methodType) {
	case ":primary", "":
		m.primary = caller
	case ":before":
		m.before = caller
	case ":after":
		m.after = caller
	case ":whopper", ":wrapper":
		m.wrap = caller
	default:
		panic(fmt.Sprintf("%s is not a valid method type.", methodType))
	}
	if add {
		obj.methods[name] = m
	}
}

// String representation of the Object.
func (obj *Flavor) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Flavor) Append(b []byte) []byte {
	b = append(b, "#<flavor "...)
	b = append(b, obj.name...)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj *Flavor) Simplify() any {
	flist := make([]any, 0, len(obj.inherit))
	for _, f := range obj.inherit {
		flist = append(flist, f.name)
	}
	vars := map[string]any{}
	for k, o := range obj.defaultVars {
		if o == nil {
			vars[k] = nil
		} else {
			vars[k] = o.Simplify()
		}
	}
	methods := make([]any, 0, len(obj.methods))
	names := make([]string, 0, len(obj.methods))

	for name := range obj.methods {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		methods = append(methods, obj.methods[name].Simplify())
	}
	return map[string]any{
		"name":            obj.name,
		"docs":            obj.docs,
		"inherit":         flist,
		"defaultVars":     vars,
		"methods":         methods,
		"included":        obj.simplifyStringArray(obj.included),
		"required":        obj.simplifyStringArray(obj.required),
		"requiredMethods": obj.simplifyStringArray(obj.requiredMethods),
		"requiredVars":    obj.simplifyStringArray(obj.requiredVars),
		"initable":        obj.simplifyStringArray(obj.initable),
		"defaultHandler":  (obj.defaultHandler != nil),
		"abstract":        obj.abstract,
	}
}

func (obj *Flavor) simplifyStringArray(sa []string) (simple []any) {
	for _, s := range sa {
		simple = append(simple, s)
	}
	return
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Flavor) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Flavor) Hierarchy() []slip.Symbol {
	return []slip.Symbol{FlavorSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj *Flavor) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

func (obj *Flavor) inheritsFlavor(name string) bool {
	for _, f2 := range obj.inherit {
		if f2.name == name {
			return true
		}
	}
	return false
}

func (obj *Flavor) inheritFlavor(cf *Flavor) {
	for _, f2 := range obj.inherit {
		if f2 == cf {
			return
		}
	}
	obj.inherit = append(obj.inherit, cf)
	for k := range cf.defaultVars {
		if v, has := obj.defaultVars[k]; !has {
			obj.defaultVars[k] = v
		}
	}
	for k, m := range cf.methods {
		xm := obj.methods[k]
		if xm != nil {
			xm.inherit = append(xm.inherit, m)
		} else {
			xm = &method{
				name:    k,
				from:    obj,
				inherit: []*method{m},
			}
			obj.methods[k] = xm
		}
	}
	for _, f2 := range cf.inherit {
		if &vanilla != f2 {
			obj.inheritFlavor(f2)
		}
	}
}

func (obj *Flavor) makeInstance() *Instance {
	inst := Instance{
		flavor: obj,
		scope:  slip.NewScope(),
	}
	for k, v := range obj.defaultVars {
		inst.scope.Let(slip.Symbol(k), v)
	}
	return &inst
}
