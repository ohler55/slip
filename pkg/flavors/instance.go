// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"strconv"
	"unsafe"

	"github.com/ohler55/slip"
)

// InstanceSymbol is the symbol with a value of "instance".
const InstanceSymbol = slip.Symbol("instance")

func init() {
	slip.DefConstant(InstanceSymbol, InstanceSymbol,
		`An _instance_ of a _flavor_ .`)
}

// Instance is an instance of a Flavor.
type Instance struct {
	slip.Scope
	flavor *Flavor
}

// String representation of the Object.
func (obj *Instance) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Instance) Append(b []byte) []byte {
	b = append(b, "#<"...)
	b = append(b, obj.flavor.name...)
	b = append(b, ' ')
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(obj))), 16)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj *Instance) Simplify() interface{} {
	// TBD make a map of vars and flavor
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Instance) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Instance) Hierarchy() []slip.Symbol {
	return []slip.Symbol{InstanceSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj *Instance) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

func (obj *Instance) send(message string, args slip.List, depth int) slip.Object {

	// TBD

	return nil
}
