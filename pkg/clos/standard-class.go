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
	defname         string
	supers          []slip.Symbol
	inherit         []slip.Class
	inheritCheck    func(c slip.Class) *StandardClass
	slotDefs        map[string]*SlotDef
	pkg             *slip.Package
	precedence      []slip.Symbol
	defaultInitArgs map[string]slip.Object
	initArgs        map[string]*SlotDef // map with keys of initargs
	initForms       map[string]*SlotDef
	baseClass       slip.Symbol
	Final           bool
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

// Inherits returns true if this Class inherits from a specified Class.
func (c *StandardClass) Inherits(sc slip.Class) bool {
	for _, c2 := range c.inherit {
		if c2.Name() == sc.Name() {
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
		b = append(b, f.Name()...)
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
	slotDefs := c.allSlotsDefs()
	if 0 < len(slotDefs) {
		b = append(b, '\n')
		for _, sd := range slotDefs {
			b = append(b, indentSpaces[:i3]...)
			b = sd.Describe(b, c, i3, right, ansi)
		}
	} else {
		b = append(b, " None\n"...)
	}
	if 0 < len(c.defaultInitArgs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Default-initargs:\n"...)
		keys := make([]string, 0, len(c.defaultInitArgs))
		for k := range c.defaultInitArgs {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			v := c.defaultInitArgs[k]
			b = append(b, k...)
			b = append(b, ':', ' ')
			b = slip.Append(b, v)
			b = append(b, '\n')
		}
	}
	if 0 < len(c.vars) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Class Slots:\n"...)
		var keys []string
		for k := range c.vars {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, c.vars[k])
			b = append(b, '\n')
		}
	}
	return b
}

func (c *StandardClass) allSlotsDefs() []*SlotDef {
	defs := map[string]*SlotDef{}
	for i := len(c.inherit) - 1; 0 <= i; i-- {
		if sc, ok := c.inherit[i].(isStandardClass); ok {
			for k, sd := range sc.slotDefMap() {
				defs[k] = sd
			}
		}
	}
	for k, sd := range c.slotDefs {
		defs[k] = sd
	}
	keys := make([]string, 0, len(defs))
	for k := range defs {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	sda := make([]*SlotDef, len(keys))
	for i, k := range keys {
		sda[i] = defs[k]
	}
	return sda
}

// MakeInstance creates a new instance but does not call the :init method.
func (c *StandardClass) MakeInstance() slip.Instance {
	if len(c.precedence) == 0 {
		slip.NewPanic("The class %s has undefined superclasses.", c.name)
	}
	obj := StandardObject{
		WithSlots: WithSlots{
			vars:   map[string]slip.Object{},
			locker: slip.NoOpLocker{},
		},
		Type: c,
	}
	c.initObjSlots(&obj)

	return &obj
}

func (c *StandardClass) initObjSlots(obj *StandardObject) {
	for k, sd := range c.slotDefs {
		if !sd.classStore {
			obj.vars[k] = sd.initform
		}
	}
	for _, ic := range c.inherit {
		if sc, ok := ic.(isStandardClass); ok {
			for k, sd := range sc.slotDefMap() {
				if _, has := obj.vars[k]; !has && !sd.classStore {
					obj.vars[k] = sd.initform
				}
			}
		}
	}
}

// DefList returns a list that can be evaluated to create the class or nil if
// the class is a built in class.
func (c *StandardClass) DefList() slip.List {
	supers := make(slip.List, len(c.supers))
	for i, super := range c.supers {
		supers[i] = super
	}
	keys := make([]string, 0, len(c.slotDefs))
	for k := range c.slotDefs {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	slots := make(slip.List, len(keys))
	for i, k := range keys {
		slots[i] = c.slotDefs[k].DefList()
	}
	def := slip.List{
		slip.Symbol(c.defname),
		slip.Symbol(c.name),
		supers,
		slots,
	}
	if 0 < len(c.docs) {
		def = append(def, slip.List{slip.Symbol(":documentation"), slip.String(c.docs)})
	}
	if 0 < len(c.defaultInitArgs) {
		defs := make(slip.List, 0, len(c.defaultInitArgs)*2)
		keys := make([]string, 0, len(c.defaultInitArgs))
		for k := range c.defaultInitArgs {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			defs = append(defs, slip.Symbol(k), c.defaultInitArgs[k])
		}
		def = append(def, append(slip.List{slip.Symbol(":default-initargs")}, defs...))
	}
	return def
}

// Ready returns true when the class is ready for use or that all superclasses
// have been defined and merged.
func (c *StandardClass) Ready() bool {
	return 0 < len(c.precedence)
}

func (c *StandardClass) mergeSupers() bool {
	// Just look at the super precedence. No need to dig any deeper if the
	// super is ready.
	c.inherit = c.inherit[:0]
	for _, super := range c.supers {
		sc := c.pkg.FindClass(string(super))
		ssc := c.inheritCheck(sc)
		if ssc == nil || len(ssc.precedence) == 0 {
			c.inherit = c.inherit[:0]
			return false
		}
		if c.Inherits(ssc) {
			continue
		}
		// Place all direct classes on the list first.
		c.inherit = append(c.inherit, sc)
	}
	// Expand the inheritance list based on super-supers.
	ics := c.inherit
	for _, ic := range ics {
		sc := c.inheritCheck(ic)
		for _, super := range sc.inherit {
			if !c.Inherits(super) {
				c.inherit = append(c.inherit, super)
			}
		}
	}

	// TBD add readers, writers, and accessors for each slot if not already added
	//  maybe keep pointer to the generic method for each

	c.initArgs = map[string]*SlotDef{}
	c.initForms = map[string]*SlotDef{}
	for i := len(c.inherit) - 1; 0 <= i; i-- {
		if sc, ok := c.inherit[i].(isStandardClass); ok {
			for _, sd := range sc.slotDefMap() {
				for _, ia := range sd.initargs {
					c.initArgs[string(ia)] = sd
				}
				if sd.initform != nil {
					c.initForms[sd.name] = sd
				}
			}
		}
	}
	for _, sd := range c.slotDefs {
		for _, ia := range sd.initargs {
			c.initArgs[string(ia)] = sd
		}
		if sd.initform != nil {
			c.initForms[sd.name] = sd
		}
	}

	c.precedence = make([]slip.Symbol, 0, len(c.inherit)+3)
	c.precedence = append(c.precedence, slip.Symbol(c.name))
	for _, ic := range c.inherit {
		c.precedence = append(c.precedence, slip.Symbol(ic.Name()))
	}
	if 0 < len(c.baseClass) {
		c.precedence = append(c.precedence, c.baseClass)
	}
	c.precedence = append(c.precedence, slip.TrueSymbol)

	return true
}

// Metaclass returns the symbol standard-class.
func (c *StandardClass) Metaclass() slip.Symbol {
	return StandardClassSymbol
}

func (c *StandardClass) slotDefMap() map[string]*SlotDef {
	return c.slotDefs
}

func (c *StandardClass) initArgDef(name string) *SlotDef {
	return c.initArgs[name]
}

func (c *StandardClass) initFormMap() map[string]*SlotDef {
	return c.initForms
}

func (c *StandardClass) defaultsMap() map[string]slip.Object {
	return c.defaultInitArgs
}

func (c *StandardClass) precedenceList() []slip.Symbol {
	return c.precedence
}

func (c *StandardClass) inheritedClasses() []slip.Class {
	return c.inherit
}

func makeClassesReady(p *slip.Package) {
	var not []isStandardClass
	for _, c := range p.AllClasses() {
		if sc, ok := c.(isStandardClass); ok && !sc.Ready() {
			not = append(not, sc)
		}
	}
	if 0 < len(not) {
		for {
			var changed bool
			for _, sc := range not {
				if !sc.Ready() {
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

func classChanged(cc slip.Class, p *slip.Package) {
	for _, c := range p.AllClasses() {
		if c.Inherits(cc) {
			if sc, ok := c.(isStandardClass); ok {
				sc.mergeSupers()
			}
		}
	}
}
