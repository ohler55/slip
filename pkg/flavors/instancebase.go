// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"strconv"
	"unsafe"

	"github.com/ohler55/slip"
)

// Instance is the base for flavor and class instances.
type InstanceBase struct {
	slip.Scope
	Methods map[string][]*Method
	Type    slip.Class
}

// String representation of the Object.
func (obj *Instance) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Instance) Append(b []byte) []byte {
	b = append(b, "#<"...)
	b = append(b, obj.Type.Name()...)
	b = append(b, ' ')
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(obj))), 16)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj *Instance) Simplify() interface{} {
	vars := map[string]any{}
	for name, val := range obj.Vars {
		if name != "self" {
			if iv, ok := val.(*Instance); ok {
				vars[name] = iv.String()
			} else {
				vars[name] = slip.Simplify(val)
			}
		}
	}
	simple := map[string]any{
		"flavor": obj.Type.Name(),
		"id":     strconv.FormatUint(uint64(uintptr(unsafe.Pointer(obj))), 16),
		"vars":   vars,
	}
	return simple
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Instance) Hierarchy() []slip.Symbol {
	return []slip.Symbol{slip.Symbol(obj.Type.Name()), InstanceSymbol, slip.TrueSymbol}
}

// HasMethod returns true if the instance handles the named method.
func (obj *Instance) HasMethod(method string) bool {
	_, has := obj.Methods[method]

	return has
}

// Receive a method invocation from the send function. Not intended to be
// called by any code other than the send function but is public to allow it
// to be over-ridden.
func (obj *Instance) Receive(s *slip.Scope, message string, args slip.List, depth int) slip.Object {
	ma := obj.Methods[message]
	if len(ma) == 0 {
		xargs := make(slip.List, 0, len(args)+1)
		xargs = append(xargs, slip.Symbol(message))
		xargs = append(xargs, args...)
		if flavor, ok := obj.Type.(*Flavor); ok {
			return flavor.defaultHandler.Call(&obj.Scope, xargs, depth)
		}
		slip.NewPanic("Method %s not defined for %s.", message, obj.Type.Name())
	}
	for i, m := range ma {
		if m.wrap != nil {
			loc := &whopLoc{methods: ma, current: i}
			ws := obj.Scope.NewScope()
			if s != nil {
				ws.AddParent(s)
			}
			ws.Let("~whopper-location~", loc)
			(m.wrap.(*slip.Lambda)).Closure = ws

			return m.wrap.Call(ws, args, depth)
		}
	}
	return obj.innerReceive(s, ma, args, depth)
}

func (obj *Instance) innerReceive(s *slip.Scope, ma []*Method, args slip.List, depth int) slip.Object {
	scope := obj.NewScope()
	if s != nil {
		scope.AddParent(s)
	}
	for _, m := range ma {
		if m.before != nil {
			m.before.Call(scope, args, depth)
		}
	}
	var result slip.Object
	for _, m := range ma {
		if m.primary != nil {
			result = m.primary.Call(scope, args, depth)
			break
		}
	}
	for _, m := range ma {
		if m.after != nil {
			m.after.Call(scope, args, depth)
		}
	}
	return result
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Instance) Equal(other slip.Object) bool {
	if obj == other {
		return true
	}
	if o, ok := other.(*Instance); ok && obj.Type == o.Type {
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

// Eval returns self.
func (obj *Instance) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// BoundReceive receives a method invocation with all arguments already bound to a scope.
func (obj *Instance) BoundReceive(ps *slip.Scope, message string, bindings *slip.Scope, depth int) slip.Object {
	s := obj.NewScope()
	if ps != nil {
		s.AddParent(ps)
	}
	if bindings != nil {
		for k, v := range bindings.Vars {
			s.Vars[k] = v
		}
	}
	ma := obj.Methods[message]
	if len(ma) == 0 {
		if flavor, ok := obj.Type.(*Flavor); ok {
			if bc, _ := flavor.defaultHandler.(slip.BoundCaller); bc != nil {
				s.Let(slip.Symbol("method"), slip.Symbol(message))
				var args slip.List
				for k, v := range bindings.Vars {
					args = append(args, slip.List{slip.Symbol(k), slip.Tail{Value: v}})
				}
				s.Let(slip.Symbol("args"), args)
				return bc.BoundCall(s, depth)
			}
		}
		slip.PanicUnboundSlot(obj, slip.Symbol(message), "%s is not a method of flavor %s.", message, obj.Type.Name())
	}
	for i, m := range ma {
		if m.wrap != nil {
			loc := &whopLoc{methods: ma, current: i}
			ws := s.NewScope()
			ws.Let("~whopper-location~", loc)
			(m.wrap.(*slip.Lambda)).Closure = s
			if bc, _ := m.wrap.(slip.BoundCaller); bc != nil {
				return bc.BoundCall(ws, depth)
			}
		}
	}
	return obj.innerBoundReceive(ma, s, depth)
}

func (obj *Instance) innerBoundReceive(ma []*Method, s *slip.Scope, depth int) slip.Object {
	for _, m := range ma {
		if bc, _ := m.before.(slip.BoundCaller); bc != nil {
			bc.BoundCall(s, depth)
		}
	}
	var result slip.Object
	for _, m := range ma {
		if bc, _ := m.primary.(slip.BoundCaller); bc != nil {
			result = bc.BoundCall(s, depth)
			break
		}
	}
	for _, m := range ma {
		if bc, _ := m.after.(slip.BoundCaller); bc != nil {
			bc.BoundCall(s, depth)
		}
	}
	return result
}
