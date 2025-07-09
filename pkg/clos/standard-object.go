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
		obj.Vars[sd.name] = v
		nameMap[sd.name] = k
	}
	for k, v := range obj.Type.defaultInitArgs {
		sd := obj.Type.initArgs[k]
		if _, has := nameMap[sd.name]; !has {
			// TBD eval if a func
			obj.Vars[sd.name] = v
			nameMap[sd.name] = k
		}
	}
	for k, sd := range obj.Type.initForms {
		if _, has := nameMap[k]; !has {
			// TBD eval
			obj.Vars[sd.name] = sd.initform
		}
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

	keys := make([]string, 0, len(obj.Vars))
	for k := range obj.Vars {
		if k != "self" {
			keys = append(keys, k)
		}
	}
	if 0 < len(keys) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "  has slot values:\n"...)
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:indent+4]...)
			b = append(b, k...)
			b = append(b, ": "...)
			b = slip.ObjectAppend(b, obj.Vars[k])
			b = append(b, '\n')
		}
	}
	return b
}

// Class returns the flavor of the instance.
func (obj *StandardObject) Class() slip.Class {
	return obj.Type
}
