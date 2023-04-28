// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"sort"
	"strconv"
	"unsafe"

	"github.com/ohler55/slip"
)

// InstanceSymbol is the symbol with a value of "instance".
const InstanceSymbol = slip.Symbol("instance")

func init() {
	slip.DefConstant(InstanceSymbol, InstanceSymbol,
		`An _instance_ of a _flavor_.`)
}

// Instance is an instance of a Flavor.
type Instance struct {
	slip.Scope
	Flavor *Flavor
	// Any is available to go methods.
	Any any
}

// String representation of the Object.
func (obj *Instance) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Instance) Append(b []byte) []byte {
	b = append(b, "#<"...)
	b = append(b, obj.Flavor.name...)
	b = append(b, ' ')
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(obj))), 16)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj *Instance) Simplify() interface{} {
	vars := map[string]any{}
	for name, val := range obj.Vars {
		if name != "self" {
			vars[name] = slip.Simplify(val)
		}
	}
	simple := map[string]any{
		"flavor": obj.Flavor.name,
		"vars":   vars,
	}
	return simple
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Instance) Equal(other slip.Object) bool {
	if obj == other {
		return true
	}
	if o, ok := other.(*Instance); ok && obj.Flavor == o.Flavor {
		for k, val := range obj.Vars {
			if k == "self" {
				continue
			}
			if !slip.ObjectEqual(val, o.Vars[k]) {
				return false
			}
		}
		return true
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Instance) Hierarchy() []slip.Symbol {
	return []slip.Symbol{slip.Symbol(obj.Flavor.name), InstanceSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj *Instance) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Receive a method invocation from the send function. Not intended to be
// call by any code other than the send function but is public to allow it
// to be over-ridden.
func (obj *Instance) Receive(message string, args slip.List, depth int) slip.Object {
	ma := obj.Flavor.methods[message]
	if len(ma) == 0 {
		xargs := args
		xargs = append(xargs, slip.Symbol(message))
		return obj.Flavor.defaultHandler.Call(&obj.Scope, xargs, depth)
	}
	for i, m := range ma {
		if m.wrap != nil {
			loc := &whopLoc{methods: ma, current: i}
			ws := obj.Scope.NewScope()
			ws.Let("~whopper-location~", loc)
			(m.wrap.(*slip.Lambda)).Closure = ws

			return m.wrap.Call(ws, args, depth)
		}
	}
	return obj.innerReceive(ma, args, depth)
}

func (obj *Instance) innerReceive(ma []*method, args slip.List, depth int) slip.Object {
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

const (
	bold         = "\x1b[1m"
	colorOff     = "\x1b[m"
	indentSpaces = "                                                                                "
)

// Describe the instance in detail.
func (obj *Instance) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = obj.Append(b)
		b = append(b, colorOff...)
	} else {
		b = obj.Append(b)
	}
	b = append(b, ", an instance of flavor "...)
	if ansi {
		b = append(b, bold...)
		b = append(b, obj.Flavor.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, obj.Flavor.name...)
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
		b = append(b, "  has instance variable values:\n"...)
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

// Length returns the length of the object.
func (obj Instance) Length() (size int) {
	if 0 < len(obj.Flavor.methods[":length"]) {
		v := obj.Receive(":length", slip.List{}, 0)
		if num, ok := v.(slip.Fixnum); ok {
			size = int(num)
		}
	} else {
		switch tv := obj.Any.(type) {
		case string:
			size = len(tv)
		case []any:
			size = len(tv)
		case map[string]any:
			size = len(tv)
		}
	}
	return
}
