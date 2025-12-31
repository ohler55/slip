// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"sort"

	"github.com/ohler55/slip"
)

const (
	// StructureClassSymbol is the metaclass symbol for structure classes.
	StructureClassSymbol = slip.Symbol("structure-class")
	// StructureSymbol is used in type hierarchies.
	StructureSymbol = slip.Symbol("structure")
)

// ConstructorSpec describes a constructor for a structure.
// For keyword constructors: name is the constructor name, boaList is nil.
// For BOA constructors: name is the constructor name, boaList is the lambda-list.
type ConstructorSpec struct {
	name    string
	boaList slip.List // nil for standard keyword constructor
}

// StructureClass represents a structure type defined by defstruct.
type StructureClass struct {
	name          string
	docs          string
	slots         []*StructureSlot // ordered list of slots
	concName      string           // prefix for accessor names (default: name-)
	constructors  []ConstructorSpec
	copierName    string          // copier function name (empty = none)
	predicateName string          // predicate function name (empty = none)
	include       *StructureClass // parent structure (from :include)
	initialOffset int             // number of slots to skip at start (:initial-offset)
	named         bool            // whether structure is named (:named)
	repType       slip.Symbol     // representation type: "vector", "list", or "" for standard
	printFunc     slip.Object     // :print-function
	printObject   slip.Object     // :print-object
	pkg           *slip.Package   // package where structure was defined
	precedence    []slip.Symbol   // type hierarchy for typep
}

// NewStructureClass creates a new structure class with default options.
func NewStructureClass(name string, pkg *slip.Package) *StructureClass {
	sc := &StructureClass{
		name:          name,
		concName:      name + "-",
		copierName:    "copy-" + name,
		predicateName: name + "-p",
		named:         true, // structures are named by default (when no :type)
		pkg:           pkg,
	}
	// Default constructor
	sc.constructors = []ConstructorSpec{{name: "make-" + name}}
	return sc
}

// String representation of the structure class.
func (sc *StructureClass) String() string {
	return string(sc.Append([]byte{}))
}

// Append a buffer with a representation of the structure class.
func (sc *StructureClass) Append(b []byte) []byte {
	b = append(b, "#<structure-class "...)
	b = append(b, sc.name...)
	return append(b, '>')
}

// Simplify returns a simplified representation for debugging.
func (sc *StructureClass) Simplify() any {
	slots := make([]string, len(sc.slots))
	for i, s := range sc.slots {
		slots[i] = s.name
	}
	return map[string]any{
		"name":      sc.name,
		"slots":     slots,
		"conc-name": sc.concName,
		"copier":    sc.copierName,
		"predicate": sc.predicateName,
	}
}

// Equal returns true if this is the same structure class.
func (sc *StructureClass) Equal(other slip.Object) bool {
	return sc == other
}

// Hierarchy returns the type hierarchy for the structure class itself.
func (sc *StructureClass) Hierarchy() []slip.Symbol {
	return []slip.Symbol{StructureClassSymbol, slip.Symbol("class"), slip.TrueSymbol}
}

// Eval returns self.
func (sc *StructureClass) Eval(s *slip.Scope, depth int) slip.Object {
	return sc
}

// Name of the structure.
func (sc *StructureClass) Name() string {
	return sc.name
}

// Pkg returns the package where the structure was defined.
func (sc *StructureClass) Pkg() *slip.Package {
	return sc.pkg
}

// Documentation of the structure.
func (sc *StructureClass) Documentation() string {
	return sc.docs
}

// SetDocumentation sets the structure's documentation.
func (sc *StructureClass) SetDocumentation(doc string) {
	sc.docs = doc
}

// Describe the structure class in detail.
func (sc *StructureClass) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, sc.name...)
	b = append(b, " is a structure:\n"...)
	i2 := indent + 2
	i3 := indent + 4

	if sc.docs != "" {
		b = append(b, "  Documentation:\n    "...)
		b = append(b, sc.docs...)
		b = append(b, '\n')
	}

	if sc.include != nil {
		b = append(b, "  Includes: "...)
		b = append(b, sc.include.name...)
		b = append(b, '\n')
	}

	b = append(b, "  Conc-name: "...)
	b = append(b, sc.concName...)
	b = append(b, '\n')

	b = append(b, "  Constructors:"...)
	for _, c := range sc.constructors {
		b = append(b, ' ')
		b = append(b, c.name...)
	}
	b = append(b, '\n')

	if sc.copierName != "" {
		b = append(b, "  Copier: "...)
		b = append(b, sc.copierName...)
		b = append(b, '\n')
	}

	if sc.predicateName != "" {
		b = append(b, "  Predicate: "...)
		b = append(b, sc.predicateName...)
		b = append(b, '\n')
	}

	if sc.repType != "" {
		b = append(b, "  Type: "...)
		b = append(b, sc.repType...)
		b = append(b, '\n')
	}

	b = append(b, "  Slots:\n"...)
	_ = i2
	_ = i3
	for _, slot := range sc.slots {
		b = append(b, "    "...)
		b = append(b, slot.name...)
		if slot.readOnly {
			b = append(b, " (read-only)"...)
		}
		if slot.initform != nil {
			b = append(b, " = "...)
			b = slip.Append(b, slot.initform)
		}
		b = append(b, '\n')
	}

	return b
}

// MakeInstance creates a new structure instance.
func (sc *StructureClass) MakeInstance() slip.Instance {
	return NewStructureObject(sc)
}

// Inherits returns true if this structure inherits from the specified class.
func (sc *StructureClass) Inherits(c slip.Class) bool {
	for parent := sc.include; parent != nil; parent = parent.include {
		if parent.name == c.Name() {
			return true
		}
	}
	return false
}

// InheritsList returns the list of inherited classes.
func (sc *StructureClass) InheritsList() []slip.Class {
	var classes []slip.Class
	for parent := sc.include; parent != nil; parent = parent.include {
		classes = append(classes, parent)
	}
	return classes
}

// LoadForm returns a list that can be evaluated to recreate the structure.
func (sc *StructureClass) LoadForm() slip.Object {
	// Build the defstruct form
	form := slip.List{slip.Symbol("defstruct")}

	// Name and options
	if sc.hasNonDefaultOptions() {
		opts := slip.List{slip.Symbol(sc.name)}
		if sc.concName != sc.name+"-" {
			if sc.concName == "" {
				opts = append(opts, slip.Symbol(":conc-name"), nil)
			} else {
				opts = append(opts, slip.Symbol(":conc-name"), slip.Symbol(sc.concName))
			}
		}
		if sc.copierName == "" {
			opts = append(opts, slip.Symbol(":copier"), nil)
		} else if sc.copierName != "copy-"+sc.name {
			opts = append(opts, slip.Symbol(":copier"), slip.Symbol(sc.copierName))
		}
		if sc.predicateName == "" {
			opts = append(opts, slip.Symbol(":predicate"), nil)
		} else if sc.predicateName != sc.name+"-p" {
			opts = append(opts, slip.Symbol(":predicate"), slip.Symbol(sc.predicateName))
		}
		if sc.include != nil {
			opts = append(opts, slip.List{slip.Symbol(":include"), slip.Symbol(sc.include.name)})
		}
		if sc.repType != "" {
			opts = append(opts, slip.Symbol(":type"), sc.repType)
		}
		form = append(form, opts)
	} else {
		form = append(form, slip.Symbol(sc.name))
	}

	// Documentation
	if sc.docs != "" {
		form = append(form, slip.String(sc.docs))
	}

	// Slots - skip inherited slots at the beginning
	inheritedCount := 0
	if sc.include != nil {
		inheritedCount = len(sc.include.slots)
	}
	for i, slot := range sc.slots {
		if i < sc.initialOffset+inheritedCount {
			continue // Skip inherited slots
		}
		slotForm := sc.slotLoadForm(slot)
		form = append(form, slotForm)
	}

	return form
}

func (sc *StructureClass) hasNonDefaultOptions() bool {
	return sc.concName != sc.name+"-" ||
		sc.copierName != "copy-"+sc.name ||
		sc.predicateName != sc.name+"-p" ||
		sc.include != nil ||
		sc.repType != "" ||
		len(sc.constructors) != 1 ||
		sc.constructors[0].name != "make-"+sc.name
}

func (sc *StructureClass) slotLoadForm(slot *StructureSlot) slip.Object {
	if slot.initform == nil && slot.slotType == nil && !slot.readOnly {
		return slip.Symbol(slot.name)
	}
	form := slip.List{slip.Symbol(slot.name)}
	if slot.initform != nil {
		form = append(form, slot.initform)
	}
	if slot.slotType != nil {
		form = append(form, slip.Symbol(":type"), slot.slotType)
	}
	if slot.readOnly {
		form = append(form, slip.Symbol(":read-only"), slip.TrueSymbol)
	}
	return form
}

// Metaclass returns the structure-class symbol.
func (sc *StructureClass) Metaclass() slip.Symbol {
	return StructureClassSymbol
}

// VarNames returns all slot names.
func (sc *StructureClass) VarNames() []string {
	names := make([]string, 0, len(sc.slots))
	for _, slot := range sc.slots {
		names = append(names, slot.name)
	}
	sort.Strings(names)
	return names
}

// GetSlot returns a slot by name, or nil if not found.
func (sc *StructureClass) GetSlot(name string) *StructureSlot {
	for _, slot := range sc.slots {
		if slot.name == name {
			return slot
		}
	}
	return nil
}

// SlotCount returns the total number of slots.
func (sc *StructureClass) SlotCount() int {
	return len(sc.slots)
}

// AllSlots returns all slots including inherited ones.
func (sc *StructureClass) AllSlots() []*StructureSlot {
	return sc.slots
}

// buildPrecedence builds the type precedence list for typep.
func (sc *StructureClass) buildPrecedence() {
	sc.precedence = []slip.Symbol{slip.Symbol(sc.name)}
	for parent := sc.include; parent != nil; parent = parent.include {
		sc.precedence = append(sc.precedence, slip.Symbol(parent.name))
	}
	sc.precedence = append(sc.precedence, StructureSymbol, slip.TrueSymbol)
}

// Precedence returns the type precedence list.
func (sc *StructureClass) Precedence() []slip.Symbol {
	return sc.precedence
}

// Constructors returns the list of constructor specifications.
func (sc *StructureClass) Constructors() []ConstructorSpec {
	return sc.constructors
}

// ConcName returns the accessor prefix.
func (sc *StructureClass) ConcName() string {
	return sc.concName
}

// CopierName returns the copier function name.
func (sc *StructureClass) CopierName() string {
	return sc.copierName
}

// PredicateName returns the predicate function name.
func (sc *StructureClass) PredicateName() string {
	return sc.predicateName
}

// IsNamed returns true if the structure is named.
func (sc *StructureClass) IsNamed() bool {
	return sc.named
}

// RepType returns the representation type.
func (sc *StructureClass) RepType() slip.Symbol {
	return sc.repType
}

// InitialOffset returns the initial slot offset.
func (sc *StructureClass) InitialOffset() int {
	return sc.initialOffset
}

// Include returns the parent structure class.
func (sc *StructureClass) Include() *StructureClass {
	return sc.include
}

// PrintFunction returns the custom print function.
func (sc *StructureClass) PrintFunction() slip.Object {
	return sc.printFunc
}

// PrintObject returns the custom print-object function.
func (sc *StructureClass) PrintObject() slip.Object {
	return sc.printObject
}

// FindStructureClass looks up a structure class by name.
func FindStructureClass(name string) *StructureClass {
	c := slip.CurrentPackage.FindClass(name)
	if sc, ok := c.(*StructureClass); ok {
		return sc
	}
	return nil
}

// MustFindStructureClass looks up a structure class and panics if not found.
func MustFindStructureClass(s *slip.Scope, depth int, name string) *StructureClass {
	sc := FindStructureClass(name)
	if sc == nil {
		slip.ErrorPanic(s, depth, "structure %s is not defined", name)
	}
	return sc
}

// accessorName returns the accessor name for a slot.
func (sc *StructureClass) accessorName(slotName string) string {
	return fmt.Sprintf("%s%s", sc.concName, slotName)
}
