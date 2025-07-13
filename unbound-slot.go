// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// UnboundSlotSymbol is the symbol with a value of "unbound-slot".
const UnboundSlotSymbol = Symbol("unbound-slot")

// NewUnboundSlot raises a UnboundSlotPanic (unbound-slot)
// describing a unbound-slot error.
func NewUnboundSlot(instance Object, name Object, format string, args ...any) Object {
	c := FindClass("unbound-slot")
	obj := c.MakeInstance()
	initArgs := List{
		Symbol(":name"), name,
		Symbol(":instance"), instance,
	}
	if 0 < len(format) {
		initArgs = append(initArgs, Symbol(":message"), String(fmt.Sprintf(format, args...)))
	} else {
		initArgs = append(initArgs, Symbol(":message"), String(fmt.Sprintf("The slot %s is unbound in the object %s.",
			name, instance)))
	}

	obj.Init(NewScope(), initArgs, 0)

	return obj
}

// PanicUnboundSlot raises a UnboundSlotPanic (unbound-slot)
// describing a unbound-slot error.
func PanicUnboundSlot(instance Object, name Object, format string, args ...any) {
	panic(NewUnboundSlot(instance, name, format, args...))
}
