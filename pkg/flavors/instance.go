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
	vars := map[string]any{}
	for name, val := range obj.Vars {
		vars[name] = slip.Simplify(val)
	}
	return map[string]any{
		"flavor": obj.flavor.name,
		"vars":   vars,
	}
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
	ma := obj.flavor.methods[message]
	if len(ma) == 0 {
		xargs := append(args, slip.Symbol(":"+message), obj)
		obj.flavor.defaultHandler.Call(&obj.Scope, xargs, depth)
	}
	for _, m := range ma {
		if m.wrap != nil {
			return obj.sendWrap(ma, args, depth)
		}
	}
	return obj.sendInner(ma, args, depth)
}

func (obj *Instance) sendWrap(ma []*method, args slip.List, depth int) slip.Object {
	// TBD
	// walk wrappers
	//  need to register a reference to the method progress
	//    maybe just and index to the next starting at 0
	//    when greater than len of inherited move to others
	return nil
}

func (obj *Instance) sendInner(ma []*method, args slip.List, depth int) slip.Object {
	for _, m := range ma {
		if m.before != nil {
			m.before.Call(&obj.Scope, args, depth)
		}
	}
	var result slip.Object
	for _, m := range ma {
		if m.primary != nil {
			result = m.primary.Call(&obj.Scope, args, depth)
			break
		}
	}
	for _, m := range ma {
		if m.after != nil {
			m.after.Call(&obj.Scope, args, depth)
		}
	}
	return result
}
