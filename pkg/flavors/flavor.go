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
	name             string
	docs             string
	inherit          []*Flavor
	defaultVars      map[string]slip.Object
	keywords         map[string]slip.Object
	methods          map[string][]*method
	included         []string
	required         []string
	requiredMethods  []string
	requiredVars     []string
	requiredKeywords []string
	initable         map[string]bool
	defaultHandler   slip.Caller
	abstract         bool
	noVanilla        bool
	allowOtherKeys   bool
}

func (obj *Flavor) defMethod(name string, methodType string, caller slip.Caller) {
	name = strings.ToLower(name)
	var m *method
	add := false
	ma := obj.methods[name]
	if 0 < len(ma) {
		m = ma[0]
	} else {
		add = true
		m = &method{name: name, from: obj}
		ma = []*method{m}
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
		obj.methods[name] = ma
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
		vars[k] = slip.Simplify(o)
	}
	keywords := map[string]any{}
	for k, o := range obj.keywords {
		keywords[k] = slip.Simplify(o)
	}
	methods := make([]any, 0, len(obj.methods))
	names := make([]string, 0, len(obj.methods))

	for name := range obj.methods {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		var daemons []any
		for _, m := range obj.methods[name] {
			if !m.empty() {
				daemons = append(daemons, m.Simplify())
			}
		}
		methods = append(methods, daemons)
	}
	initable := map[string]any{}
	for k := range obj.initable {
		initable[k] = true
	}
	_, isDefHand := obj.defaultHandler.(defHand)
	return map[string]any{
		"name":             obj.name,
		"docs":             obj.docs,
		"inherit":          flist,
		"defaultVars":      vars,
		"methods":          methods,
		"keywords":         keywords,
		"included":         obj.simplifyStringArray(obj.included),
		"required":         obj.simplifyStringArray(obj.required),
		"requiredMethods":  obj.simplifyStringArray(obj.requiredMethods),
		"requiredVars":     obj.simplifyStringArray(obj.requiredVars),
		"requiredKeywords": obj.simplifyStringArray(obj.requiredKeywords),
		"initable":         initable,
		"defaultHandler":   !isDefHand,
		"abstract":         obj.abstract,
		"allowOtherKeys":   obj.allowOtherKeys,
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
	if cf.allowOtherKeys {
		obj.allowOtherKeys = true
	}
	for k, v := range cf.defaultVars {
		if _, has := obj.defaultVars[k]; !has {
			obj.defaultVars[k] = v
		}
	}
	for k, v := range cf.keywords {
		if _, has := obj.keywords[k]; !has {
			obj.keywords[k] = v
		}
	}
	for k, ma := range cf.methods {
		xma := obj.methods[k]
		if 0 < len(xma) {
			obj.methods[k] = append(obj.methods[k], ma[0])
		} else {
			obj.methods[k] = []*method{{name: k, from: obj}, ma[0]}
		}
	}
	for _, f2 := range cf.inherit {
		if &vanilla != f2 {
			obj.inheritFlavor(f2)
		}
	}
}

func (obj *Flavor) makeInstance() *Instance {
	inst := Instance{flavor: obj}
	inst.Scope.Init()
	for k, v := range obj.defaultVars {
		inst.Vars[k] = v
	}
	inst.Vars["self"] = &inst

	return &inst
}

// Describe the instance in detail.
func (obj *Flavor) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = append(b, obj.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, obj.name...)
	}
	if obj.abstract {
		b = append(b, " is an abstract flavor:\n"...)
	} else {
		b = append(b, " is a flavor:\n"...)
	}
	i2 := indent + 2
	i3 := indent + 4
	if 0 < len(obj.docs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Description:\n"...)
		b = slip.AppendDoc(b, obj.docs, i3, right, ansi)
		b = append(b, '\n')
	}
	if 0 < len(obj.inherit) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Inherits:"...)
		for _, f := range obj.inherit {
			b = append(b, ' ')
			b = append(b, f.name...)
		}
		b = append(b, '\n')
	}
	if 0 < len(obj.defaultVars) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Variables:\n"...)
		var keys []string
		for k := range obj.defaultVars {
			if k == "self" {
				continue
			}
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, obj.defaultVars[k])
			if obj.initable[":"+k] {
				b = append(b, " (initable)"...)
			}
			b = append(b, '\n')
		}
	}
	if 0 < len(obj.keywords) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Keywords with default values:\n"...)
		var keys []string
		for k := range obj.keywords {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, obj.keywords[k])
			b = append(b, '\n')
		}
	}
	if obj.allowOtherKeys {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Allow Other Keywords: true\n"...)
	}
	if 0 < len(obj.methods) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Methods:\n"...)
		var keys []string
		for k := range obj.methods {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, '\n')
		}
	}
	b = obj.describeStrings(b, "Included Flavors", obj.included, indent, right)
	b = obj.describeStrings(b, "Required Flavors", obj.required, indent, right)
	b = obj.describeStrings(b, "Required Variable", obj.requiredVars, indent, right)
	b = obj.describeStrings(b, "Required Methods", obj.requiredMethods, indent, right)
	b = obj.describeStrings(b, "Required Keywords", obj.requiredKeywords, indent, right)

	return b
}

func (obj *Flavor) describeStrings(b []byte, label string, list []string, indent, right int) []byte {
	if 0 < len(list) {
		b = append(b, indentSpaces[:indent+2]...)
		b = append(b, label...)
		b = append(b, ":\n"...)
		for _, s := range list {
			b = append(b, indentSpaces[:indent+4]...)
			b = append(b, s...)
			b = append(b, '\n')
		}
	}
	return b
}
