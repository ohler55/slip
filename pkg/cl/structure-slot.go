// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

// StructureSlot encapsulates the definition of a slot in a structure.
type StructureSlot struct {
	name     string
	slotType slip.Object // :type option - type specifier for slot values
	readOnly bool        // :read-only option - if true, slot cannot be modified after creation
	initform slip.Object // default value form (evaluated at construction time)
	index    int         // position in the structure's slot array
}

// NewStructureSlot creates a new StructureSlot from a slot-description in defstruct.
// Slot descriptions can be:
//   - symbol: just a slot name with no options
//   - (slot-name default-value slot-option*)
//
// Slot options are :type and :read-only.
func NewStructureSlot(s *slip.Scope, def slip.Object, index, depth int) *StructureSlot {
	ss := &StructureSlot{
		initform: nil, // nil means uninitialized (behavior undefined if read before set)
		index:    index,
	}

	switch td := def.(type) {
	case slip.Symbol:
		ss.name = string(td)
	case slip.List:
		if len(td) == 0 {
			slip.ErrorPanic(s, depth, "slot-description cannot be an empty list")
		}
		// First element is the slot name
		if sym, ok := td[0].(slip.Symbol); ok {
			ss.name = string(sym)
		} else {
			slip.TypePanic(s, depth, "slot-name", td[0], "symbol")
		}

		// Parse remaining elements: optional default value followed by slot-options
		i := 1
		// Check if second element is a default value (not a keyword)
		if i < len(td) {
			if sym, ok := td[i].(slip.Symbol); !ok || (len(sym) > 0 && sym[0] != ':') {
				// Not a keyword, treat as initform
				ss.initform = td[i]
				i++
			}
		}

		// Parse slot-options (must come in pairs)
		for i < len(td) {
			if i+1 >= len(td) {
				slip.ErrorPanic(s, depth, "slot-option %v requires a value", td[i])
			}
			switch td[i] {
			case slip.Symbol(":type"):
				if ss.slotType != nil {
					slip.ErrorPanic(s, depth, "for slot %s, :type can only be specified once", ss.name)
				}
				ss.slotType = td[i+1]
			case slip.Symbol(":read-only"):
				ss.readOnly = td[i+1] != nil
			default:
				slip.TypePanic(s, depth, "slot-option", td[i], ":type", ":read-only")
			}
			i += 2
		}
	default:
		slip.TypePanic(s, depth, "slot-description", def, "symbol", "list")
	}
	return ss
}

// Name returns the slot name.
func (ss *StructureSlot) Name() string {
	return ss.name
}

// Index returns the slot's position in the structure.
func (ss *StructureSlot) Index() int {
	return ss.index
}

// IsReadOnly returns true if the slot cannot be modified after construction.
func (ss *StructureSlot) IsReadOnly() bool {
	return ss.readOnly
}

// Initform returns the slot's default value form, or nil if none specified.
func (ss *StructureSlot) Initform() slip.Object {
	return ss.initform
}

// SlotType returns the type specifier for the slot, or nil if none specified.
func (ss *StructureSlot) SlotType() slip.Object {
	return ss.slotType
}

// Copy creates a copy of the slot definition, optionally with a new index.
func (ss *StructureSlot) Copy(newIndex int) *StructureSlot {
	return &StructureSlot{
		name:     ss.name,
		slotType: ss.slotType,
		readOnly: ss.readOnly,
		initform: ss.initform,
		index:    newIndex,
	}
}
