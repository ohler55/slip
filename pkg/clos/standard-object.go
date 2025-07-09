// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"sort"
	"strconv"
	"unsafe"

	"github.com/ohler55/slip"
)

// StandardObjectSymbol is the symbol with a value of "standard-object".
const StandardObjectSymbol = slip.Symbol("standard-object")

// StandardObject is an instance of a Flavor.
type StandardObject struct {
	WithSlots
	Type *StandardClass
}

// String representation of the Object.
func (obj *StandardObject) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *StandardObject) Append(b []byte) []byte {
	b = append(b, "#<"...)
	b = append(b, obj.Type.name...)
	b = append(b, ' ')
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(obj))), 16)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj *StandardObject) Simplify() interface{} {
	simple := obj.WithSlots.Simplify()
	simple.(map[string]any)["class"] = obj.Type.name
	simple.(map[string]any)["id"] = strconv.FormatUint(uint64(uintptr(unsafe.Pointer(obj))), 16)

	return simple
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *StandardObject) Hierarchy() []slip.Symbol {
	return obj.Type.precedence
}

// IsA return true if the instance is of a flavor that inherits from the
// provided flavor.
func (obj *StandardObject) IsA(class slip.Class) bool {
	if obj.Type == class {
		return true
	}
	for _, f := range obj.Type.inherit {
		if class == f {
			return true
		}
	}
	return false
}

// Init the instance slots from the provided args list. If the scope is not
// nil then send :init is called.
func (obj *StandardObject) Init(scope *slip.Scope, args slip.List, depth int) {
	nameMap := map[string]string{}
	argMap := map[string]slip.Object{}
	fillMapFromKeyArgs(args, argMap)
	for k, v := range argMap {
		sd := obj.Type.initArgs[k]
		if sd == nil {
			slip.NewPanic("%s is not a valid initarg for %s.", k, obj.Type.name)
		}
		if n, has := nameMap[sd.name]; has {
			slip.NewPanic("Duplicate initarg (%s) for slot %s. %s already specified.", sd.name, k, n)
		}
		obj.setSlot(sd, v)
		nameMap[sd.name] = k
	}
	for k, v := range obj.Type.defaultInitArgs {
		sd := obj.Type.initArgs[k]
		if _, has := nameMap[sd.name]; !has {
			if v == nil {
				obj.setSlot(sd, nil)
			} else {
				obj.setSlot(sd, v.Eval(scope, depth+1))
			}
			nameMap[sd.name] = k
		}
	}
	for k, sd := range obj.Type.initForms {
		if _, has := nameMap[k]; !has {
			// If in the initForms then initform will not be nil.
			obj.setSlot(sd, sd.initform.Eval(scope, depth+1))
		}
	}
}

func (obj *StandardObject) setSlot(sd *SlotDef, value slip.Object) {
	if sd.argType != nil && sd.argType != slip.TrueSymbol {
		var typeOk bool
		if sym, ok := sd.argType.(slip.Symbol); ok { // TBD handle more complex type specs
			for _, h := range value.Hierarchy() {
				if h == sym {
					typeOk = true
					break
				}
			}
		}
		if !typeOk {
			slip.PanicType(sd.name, value, slip.ObjectString(sd.argType))
		}
	}
	if sd.classStore {
		obj.Type.Vars[sd.name] = value
	} else {
		obj.Vars[sd.name] = value
	}
}

// Equal returns true if this Object and the other are equal in value.
func (obj *StandardObject) Equal(other slip.Object) bool {
	if obj == other {
		return true
	}
	if o, ok := other.(*StandardObject); ok && obj.Type == o.Type {
		for k, val := range obj.Vars {
			if !slip.ObjectEqual(val, o.Vars[k]) {
				return false
			}
		}
		return true
	}
	return false
}

// Eval returns self.
func (obj *StandardObject) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Describe the instance in detail.
func (obj *StandardObject) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = obj.Append(b)
		b = append(b, colorOff...)
	} else {
		b = obj.Append(b)
	}
	b = append(b, ", an instance of class "...)
	if ansi {
		b = append(b, bold...)
		b = append(b, obj.Type.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, obj.Type.name...)
	}
	b = append(b, ",\n"...)

	keys := obj.SlotNames()
	if 0 < len(keys) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "  has slot values:\n"...)
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:indent+4]...)
			b = append(b, k...)
			b = append(b, ": "...)
			v, _ := obj.SlotValue(slip.Symbol(k))
			b = slip.ObjectAppend(b, v)
			b = append(b, '\n')
		}
	}
	return b
}

// Class returns the flavor of the instance.
func (obj *StandardObject) Class() slip.Class {
	return obj.Type
}

// SlotNames returns a list of the slots names for the instance.
func (obj *StandardObject) SlotNames() []string {
	names := obj.WithSlots.SlotNames()
	for k := range obj.Type.Vars {
		names = append(names, k)
	}
	return names
}

// SlotValue return the value of an instance variable.
func (obj *StandardObject) SlotValue(sym slip.Symbol) (value slip.Object, has bool) {
	if value, has = obj.WithSlots.SlotValue(sym); !has {
		value, has = obj.Type.SlotValue(sym)
	}
	return
}

// SetSlotValue sets the value of an instance variable.
func (obj *StandardObject) SetSlotValue(sym slip.Symbol, value slip.Object) (has bool) {
	if has = obj.WithSlots.SetSlotValue(sym, value); !has {
		has = obj.Type.SetSlotValue(sym, value)
	}
	return
}
