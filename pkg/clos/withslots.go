// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"strconv"
	"strings"
	"sync"
	"unsafe"

	"github.com/ohler55/slip"
)

// WithSlots is an instance of a Flavor.
type WithSlots struct {
	Vars   map[string]slip.Object
	locker slip.Locker
}

// Simplify by returning the string representation of the flavor.
func (obj *WithSlots) Simplify() any {
	vars := map[string]any{}
	for name, val := range obj.Vars {
		vars[name] = slip.Simplify(val)
	}
	simple := map[string]any{
		"id":   strconv.FormatUint(uint64(uintptr(unsafe.Pointer(obj))), 16),
		"vars": vars,
	}
	return simple
}

// SlotNames returns a list of the slots names for the instance.
func (obj *WithSlots) SlotNames() []string {
	names := make([]string, 0, len(obj.Vars))
	for k := range obj.Vars {
		names = append(names, k)
	}
	return names
}

// SlotValue return the value of an instance variable.
func (obj *WithSlots) SlotValue(sym slip.Symbol) (value slip.Object, has bool) {
	name := strings.ToLower(string(sym))
	obj.locker.Lock()
	if obj.Vars != nil {
		value, has = obj.Vars[name]
	}
	obj.locker.Unlock()
	return
}

// SetSlotValue sets the value of an instance variable.
func (obj *WithSlots) SetSlotValue(sym slip.Symbol, value slip.Object) (has bool) {
	name := strings.ToLower(string(sym))
	obj.Lock()
	if _, has = obj.Vars[name]; has {
		obj.Vars[name] = value
	}
	obj.Unlock()
	return
}

// SetSynchronized set the synchronized mode of the scope.
func (obj *WithSlots) SetSynchronized(on bool) {
	if on {
		obj.locker = &sync.Mutex{}
	} else {
		obj.locker = slip.NoOpLocker{}
	}
}

// Synchronized returns true if the scope is in synchronized mode.
func (obj *WithSlots) Synchronized() bool {
	_, ok := obj.locker.(*sync.Mutex)
	return ok
}

// Lock the scope to synchronize changes.
func (obj *WithSlots) Lock() {
	obj.locker.Lock()
}

// Unlock the scope to synchronize changes.
func (obj *WithSlots) Unlock() {
	obj.locker.Unlock()
}
