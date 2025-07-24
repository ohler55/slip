// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"strconv"
	"strings"
	"sync"
	"unsafe"

	"github.com/ohler55/slip"
)

// HasSlots is an instance of a Flavor.
type HasSlots struct {
	vars   map[string]slip.Object
	locker slip.Locker
}

// Init initializes the object.
func (obj *HasSlots) Init(synchronize bool) {
	obj.vars = map[string]slip.Object{}
	if synchronize {
		obj.locker = &sync.Mutex{}
	} else {
		obj.locker = slip.NoOpLocker{}
	}
}

// AddSlot adds a new slot with the initial value provided.
func (obj *HasSlots) AddSlot(sym slip.Symbol, value slip.Object) {
	name := strings.ToLower(string(sym))
	obj.Lock()
	obj.vars[name] = value
	obj.Unlock()
}

// RemoveSlot remove a slot.
func (obj *HasSlots) RemoveSlot(sym slip.Symbol) {
	name := strings.ToLower(string(sym))
	obj.Lock()
	delete(obj.vars, name)
	obj.Unlock()
}

// Simplify by returning the string representation of the flavor.
func (obj *HasSlots) Simplify() any {
	vars := map[string]any{}
	for name, val := range obj.vars {
		vars[name] = slip.Simplify(val)
	}
	simple := map[string]any{
		"id":   strconv.FormatUint(uint64(uintptr(unsafe.Pointer(obj))), 16),
		"vars": vars,
	}
	return simple
}

// SlotNames returns a list of the slots names for the instance.
func (obj *HasSlots) SlotNames() []string {
	names := make([]string, 0, len(obj.vars))
	for k := range obj.vars {
		names = append(names, k)
	}
	return names
}

// SlotValue return the value of an instance variable.
func (obj *HasSlots) SlotValue(sym slip.Symbol) (value slip.Object, has bool) {
	name := strings.ToLower(string(sym))
	obj.locker.Lock()
	if obj.vars != nil {
		value, has = obj.vars[name]
	}
	obj.locker.Unlock()
	return
}

// SetSlotValue sets the value of an instance variable.
func (obj *HasSlots) SetSlotValue(sym slip.Symbol, value slip.Object) (has bool) {
	name := strings.ToLower(string(sym))
	obj.Lock()
	if _, has = obj.vars[name]; has {
		obj.vars[name] = value
	}
	obj.Unlock()
	return
}

// SetSynchronized set the synchronized mode of the scope.
func (obj *HasSlots) SetSynchronized(on bool) {
	if on {
		obj.locker = &sync.Mutex{}
	} else {
		obj.locker = slip.NoOpLocker{}
	}
}

// Synchronized returns true if the scope is in synchronized mode.
func (obj *HasSlots) Synchronized() bool {
	_, ok := obj.locker.(*sync.Mutex)
	return ok
}

// Lock the scope to synchronize changes.
func (obj *HasSlots) Lock() {
	obj.locker.Lock()
}

// Unlock the scope to synchronize changes.
func (obj *HasSlots) Unlock() {
	obj.locker.Unlock()
}

// Vars returns the vars map.
func (obj *HasSlots) Vars() map[string]slip.Object {
	return obj.vars
}
