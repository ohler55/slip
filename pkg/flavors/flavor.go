// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

// FlavorSymbol is the symbol with a value of "flavor".
const FlavorSymbol = slip.Symbol("flavor")

var allFlavors = map[string]*Flavor{vanilla.name: &vanilla}

func init() {
	slip.DefConstant(FlavorSymbol, FlavorSymbol,
		`A _flavor_ encapsulates a class of objects.`)

	defFlavor(slip.Symbol("Foo")) // TBD remove once the defflavor function is implemented
}

// Flavor of Objects.
type Flavor struct {
	name            string
	docs            string
	inherit         []*Flavor
	defaultVars     map[string]slip.Object
	methods         map[string]*method
	required        []string
	requiredMethods []string
	requiredVars    []string
	defaultHandler  slip.Caller
	abstract        bool
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
	f.inheritFlavor(&vanilla)
	allFlavors[key] = &f
	FlavorsPkg.Set(string(name), &f)
	return &f
}

func (obj *Flavor) addVar(name slip.Symbol, val slip.Object) {
	obj.defaultVars[strings.ToLower(string(name))] = val
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
func (obj *Flavor) Simplify() interface{} {
	return obj.String()
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

func (obj *Flavor) inheritFlavor(cf *Flavor) {
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
