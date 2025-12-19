// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
)

var structureIDCounter uint64

// StructureObject represents an instance of a structure.
type StructureObject struct {
	Type   *StructureClass
	slots  []slip.Object
	id     uint64
	locker sync.Locker
}

// NewStructureObject creates a new structure instance with all slots set to nil.
func NewStructureObject(sc *StructureClass) *StructureObject {
	obj := &StructureObject{
		Type:   sc,
		slots:  make([]slip.Object, len(sc.slots)),
		id:     atomic.AddUint64(&structureIDCounter, 1),
		locker: slip.NoOpLocker{},
	}
	// Initialize slots with their initforms (evaluated at creation time by constructor)
	// Here we just set to nil; initforms are evaluated in the constructor
	return obj
}

// String representation of the structure object.
func (obj *StructureObject) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the structure object.
func (obj *StructureObject) Append(b []byte) []byte {
	// Check for custom print function/object
	if obj.Type.printFunc != nil || obj.Type.printObject != nil {
		// Custom printing will be handled elsewhere when a scope is available
		// For now, use default format
	}

	b = append(b, "#S("...)
	b = append(b, obj.Type.name...)
	for _, slot := range obj.Type.slots {
		b = append(b, ' ')
		b = append(b, ":"...)
		b = append(b, slot.name...)
		b = append(b, ' ')
		b = slip.Append(b, obj.slots[slot.index])
	}
	return append(b, ')')
}

// Simplify returns a simplified representation for debugging.
func (obj *StructureObject) Simplify() any {
	slots := make(map[string]any)
	for _, slot := range obj.Type.slots {
		slots[slot.name] = slip.SimpleObject(obj.slots[slot.index])
	}
	return map[string]any{
		"type":  obj.Type.name,
		"slots": slots,
	}
}

// Equal returns true if the structures are structurally equal.
func (obj *StructureObject) Equal(other slip.Object) bool {
	o, ok := other.(*StructureObject)
	if !ok {
		return false
	}
	if obj.Type != o.Type {
		return false
	}
	for i, v := range obj.slots {
		if !equal(v, o.slots[i]) {
			return false
		}
	}
	return true
}

// Hierarchy returns the type hierarchy for typep.
func (obj *StructureObject) Hierarchy() []slip.Symbol {
	return obj.Type.precedence
}

// Eval returns self.
func (obj *StructureObject) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Class returns the structure class.
func (obj *StructureObject) Class() slip.Class {
	return obj.Type
}

// IsA returns true if the instance is of the specified type.
func (obj *StructureObject) IsA(class string) bool {
	for _, sym := range obj.Type.precedence {
		if string(sym) == class {
			return true
		}
	}
	return false
}

// Init initializes the structure from keyword arguments.
func (obj *StructureObject) Init(scope *slip.Scope, args slip.List, depth int) {
	// Structures don't have :init methods, initialization is done by constructor
	// This is called by make-instance compatibility
	for i := 0; i < len(args)-1; i += 2 {
		if sym, ok := args[i].(slip.Symbol); ok {
			name := string(sym)
			if len(name) > 0 && name[0] == ':' {
				name = name[1:]
			}
			if slot := obj.Type.GetSlot(name); slot != nil {
				obj.slots[slot.index] = args[i+1]
			}
		}
	}
}

// SetSynchronized sets thread-safe mode.
func (obj *StructureObject) SetSynchronized(on bool) {
	if on {
		obj.locker = &sync.Mutex{}
	} else {
		obj.locker = slip.NoOpLocker{}
	}
}

// Synchronized returns true if the instance is thread-safe.
func (obj *StructureObject) Synchronized() bool {
	_, ok := obj.locker.(*sync.Mutex)
	return ok
}

// SlotNames returns all slot names.
func (obj *StructureObject) SlotNames() []string {
	names := make([]string, len(obj.Type.slots))
	for i, slot := range obj.Type.slots {
		names[i] = slot.name
	}
	return names
}

// SlotValue returns the value of a slot.
func (obj *StructureObject) SlotValue(name slip.Symbol) (slip.Object, bool) {
	slot := obj.Type.GetSlot(string(name))
	if slot == nil {
		return nil, false
	}
	obj.locker.Lock()
	defer obj.locker.Unlock()
	return obj.slots[slot.index], true
}

// SetSlotValue sets the value of a slot.
func (obj *StructureObject) SetSlotValue(name slip.Symbol, value slip.Object) bool {
	slot := obj.Type.GetSlot(string(name))
	if slot == nil {
		return false
	}
	if slot.readOnly {
		return false
	}
	obj.locker.Lock()
	defer obj.locker.Unlock()
	obj.slots[slot.index] = value
	return true
}

// GetSlotByIndex returns the value at a specific slot index.
func (obj *StructureObject) GetSlotByIndex(index int) slip.Object {
	if index < 0 || index >= len(obj.slots) {
		return nil
	}
	obj.locker.Lock()
	defer obj.locker.Unlock()
	return obj.slots[index]
}

// SetSlotByIndex sets the value at a specific slot index.
func (obj *StructureObject) SetSlotByIndex(index int, value slip.Object) {
	if index >= 0 && index < len(obj.slots) {
		obj.locker.Lock()
		defer obj.locker.Unlock()
		obj.slots[index] = value
	}
}

// GetMethod returns nil - structures don't have methods.
func (obj *StructureObject) GetMethod(name string) *slip.Method {
	return nil
}

// MethodNames returns an empty list - structures don't have methods.
func (obj *StructureObject) MethodNames() slip.List {
	return nil
}

// ID returns the unique ID for this instance.
func (obj *StructureObject) ID() uint64 {
	return obj.id
}

// Dup creates a shallow copy of the structure.
func (obj *StructureObject) Dup() slip.Instance {
	obj.locker.Lock()
	defer obj.locker.Unlock()

	newObj := &StructureObject{
		Type:   obj.Type,
		slots:  make([]slip.Object, len(obj.slots)),
		id:     atomic.AddUint64(&structureIDCounter, 1),
		locker: slip.NoOpLocker{},
	}
	copy(newObj.slots, obj.slots)
	return newObj
}

// Receive handles method calls - structures don't support methods.
func (obj *StructureObject) Receive(s *slip.Scope, message string, args slip.List, depth int) slip.Object {
	slip.ErrorPanic(s, depth, "structures do not support methods: %s", message)
	return nil
}
