// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

// StructureSlot encapsulates the definition of a slot in a structure.
type StructureSlot struct {
	slip.Function
	name     string
	slotType slip.Object // :type option - type specifier for slot values
	readOnly bool        // :read-only option - if true, slot cannot be modified after creation
	initform slip.Object // default value form (evaluated at construction time)
	index    int         // position in the structure's slot array
}

// newStructureSlot creates a new StructureSlot from a slot-description in defstruct.
// Slot descriptions can be:
//   - symbol: just a slot name with no options
//   - (slot-name default-value slot-option*)
//
// Slot options are :type and :read-only.
func newStructureSlot(s *slip.Scope, def slip.Object, index, depth int) *StructureSlot {
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

// Copy creates a copy of the slot definition with a new index.
func (ss *StructureSlot) Copy(newIndex int) *StructureSlot {
	return &StructureSlot{
		name:     ss.name,
		slotType: ss.slotType,
		readOnly: ss.readOnly,
		initform: ss.initform,
		index:    newIndex,
	}
}

// generateAccessor creates the accessor function for a slot.
func (ss *StructureSlot) generateAccessor(sc *StructureClass) {
	name := sc.accessorName(ss.name)

	slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			ss.Function = slip.Function{Name: name, Args: args}
			ss.Self = ss
			return ss
		},
		&slip.FuncDoc{
			Name: name,
			Args: []*slip.DocArg{
				{Name: "structure", Type: ss.name, Text: "Structure to access."},
			},
			Return: "t",
			Text:   fmt.Sprintf("Returns the %s slot of a %s structure.", ss.name, sc.name),
		},
	)
}

func (ss *StructureSlot) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, ss, args, 1, 1)
	obj, ok := args[0].(*StructureObject)
	if !ok {
		slip.TypePanic(s, depth, "structure", args[0], ss.name)
	}
	return obj.GetSlotByIndex(ss.index)
}

// Place implements setf support for the accessor.
func (ss *StructureSlot) Place(s *slip.Scope, args slip.List, value slip.Object) {
	if ss.readOnly {
		slip.ErrorPanic(s, 0, "cannot setf read-only slot")
	}
	obj, ok := args[0].(*StructureObject)
	if !ok {
		slip.TypePanic(s, 0, "structure", args[0], ss.name)
	}
	obj.SetSlotByIndex(ss.index, value)
}
