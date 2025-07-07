// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"sort"
	"strconv"
	"unsafe"

	"github.com/ohler55/slip"
)

// StandardClassSymbol is the symbol with a value of "standard-class".
const StandardClassSymbol = slip.Symbol("standard-class")

// StandardClass is a CLOS standard-class.
type StandardClass struct {
	WithSlots
	name            string
	docs            string
	supers          []slip.Symbol
	inherit         []*StandardClass
	slotDefs        map[string]*SlotDef
	pkg             *slip.Package
	precedence      []slip.Symbol
	defaultInitArgs slip.List
}

// String representation of the Object.
func (c *StandardClass) String() string {
	return string(c.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (c *StandardClass) Append(b []byte) []byte {
	b = append(b, "#<standard-class "...)
	b = append(b, c.name...)
	return append(b, '>')
}

// Simplify by returning the string representation of the class.
func (c *StandardClass) Simplify() any {
	simple := c.WithSlots.Simplify()
	simple.(map[string]any)["id"] = strconv.FormatUint(uint64(uintptr(unsafe.Pointer(c))), 16)
	simple.(map[string]any)["name"] = c.name
	simple.(map[string]any)["package"] = c.pkg.Name
	simple.(map[string]any)["docs"] = c.docs
	simple.(map[string]any)["superclasses"] = simplifySymList(c.supers)

	slotDefs := map[string]any{}
	for k, sd := range c.slotDefs {
		slotDefs[k] = sd.Simplify()
	}
	simple.(map[string]any)["slotDefs"] = slotDefs

	return simple
}

// Equal returns true if this Object and the other are equal in value.
func (c *StandardClass) Equal(other slip.Object) (eq bool) {
	return c == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (c *StandardClass) Hierarchy() []slip.Symbol {
	return []slip.Symbol{StandardClassSymbol, ClassSymbol, StandardObjectSymbol, slip.TrueSymbol}
}

// Inherits returns true if this StandardClass inherits from a specified Class.
func (c *StandardClass) Inherits(sc slip.Class) bool {
	for _, c2 := range c.inherit {
		if c2 == sc {
			return true
		}
	}
	return false
}

// Eval returns self.
func (c *StandardClass) Eval(s *slip.Scope, depth int) slip.Object {
	return c
}

// Name of the class.
func (c *StandardClass) Name() string {
	return c.name
}

// Pkg returns the package the class was defined in.
func (c *StandardClass) Pkg() *slip.Package {
	return c.pkg
}

// Documentation of the class.
func (c *StandardClass) Documentation() string {
	return c.docs
}

// SetDocumentation of the class.
func (c *StandardClass) SetDocumentation(doc string) {
	c.docs = doc
}

// Describe the class in detail.
func (c *StandardClass) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = append(b, c.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, c.name...)
	}
	b = append(b, " is a class:\n"...)
	i2 := indent + 2
	i3 := indent + 4
	if 0 < len(c.docs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Documentation:\n"...)
		b = slip.AppendDoc(b, c.docs, i3, right, ansi)
		b = append(b, '\n')
	}

	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Direct superclasses:"...)
	for _, f := range c.inherit {
		b = append(b, ' ')
		b = append(b, f.name...)
	}
	b = append(b, '\n')

	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Class precedence list:"...)
	for _, sym := range c.precedence {
		b = append(b, ' ')
		b = append(b, sym...)
	}
	b = append(b, '\n')

	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Slots:"...)
	if 0 < len(c.slotDefs) {
		b = append(b, '\n')
		var keys []string
		for k := range c.slotDefs {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, c.slotDefs[k].initform)
			b = append(b, '\n')
		}
	} else {
		b = append(b, " None\n"...)
	}
	if 0 < len(c.Vars) {
		b = append(b, "Class Slots:\n"...)
		var keys []string
		for k := range c.Vars {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, c.Vars[k])
			b = append(b, '\n')
		}
	}
	return b
}

// MakeInstance creates a new instance but does not call the :init method.
func (c *StandardClass) MakeInstance() slip.Instance {
	obj := StandardObject{
		WithSlots: WithSlots{
			Vars:   map[string]slip.Object{},
			locker: slip.NoOpLocker{},
		},
		Type: c,
	}
	for k, sd := range c.slotDefs {
		if !sd.classStore {
			obj.Vars[k] = sd.initform
		}
	}
	return &obj
}

// DefList returns a list that can be evaluated to create the class or nil if
// the class is a built in class.
func (c *StandardClass) DefList() slip.List {
	supers := make(slip.List, len(c.supers))
	for i, super := range c.supers {
		supers[i] = super
	}
	var slots slip.List
	for _, sd := range c.slotDefs {
		if sd.class != c {
			continue
		}
		slots = append(slots, sd.DefList())
	}
	def := slip.List{
		slip.Symbol("defclass"),
		slip.Symbol(c.name),
		supers,
		slots,
	}
	if 0 < len(c.docs) {
		def = append(def, slip.List{slip.Symbol(":documentation"), slip.String(c.docs)})
	}
	if 0 < len(c.defaultInitArgs) {
		def = append(def, append(slip.List{slip.Symbol(":default-initargs")}, c.defaultInitArgs...))
	}
	return def
}

// Ready returns true when the class is ready for use or that all superclasses
// have been defined and merged.
func (c *StandardClass) Ready() bool {
	return 0 < len(c.precedence)
}

func (c *StandardClass) mergeSupers() bool {
	// Just look at the super inherited. No need to dig any deeper if the
	// super is ready.
	c.inherit = c.inherit[:0]
	for _, super := range c.supers {
		sc := c.pkg.FindClass(string(super))
		ssc, ok := sc.(*StandardClass)
		if !ok {
			slip.PanicType("superclass", sc, "standard-class")
		}
		if ssc == nil || len(ssc.precedence) == 0 {
			c.inherit = c.inherit[:0]
			return false
		}
		if c.Inherits(ssc) {
			continue
		}
		c.inherit = append(c.inherit, ssc)
		for _, ic := range ssc.inherit {
			if !c.Inherits(ic) {
				c.inherit = append(c.inherit, ic)
			}
		}
	}
	// TBD add readers, writers, and accessors for each slot before inheriting

	for _, ic := range c.inherit {
		for name, isd := range ic.slotDefs {
			sd := c.slotDefs[name]
			if sd == nil {
				c.slotDefs[name] = isd
			}
		}
	}
	c.precedence = make([]slip.Symbol, 0, len(c.inherit)+3)
	c.precedence = append(c.precedence, slip.Symbol(c.name))
	for _, ic := range c.inherit {
		c.precedence = append(c.precedence, slip.Symbol(ic.name))
	}
	c.precedence = append(c.precedence, StandardObjectSymbol, slip.TrueSymbol)

	return true
}

func makeClassesReady(p *slip.Package) {
	var not []*StandardClass
	for _, c := range p.AllClasses() {
		if sc, ok := c.(*StandardClass); ok && len(sc.precedence) == 0 {
			not = append(not, sc)
		}
	}
	if 0 < len(not) {
		for {
			var changed bool
			for _, sc := range not {
				if len(sc.precedence) == 0 {
					if sc.mergeSupers() {
						changed = true
					}
				}
			}
			if !changed {
				break
			}
		}
	}
}
