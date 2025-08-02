// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"sort"
	"strconv"
	"strings"
	"unsafe"

	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/slip"
)

// InstanceSymbol is the symbol with a value of "instance".
const InstanceSymbol = slip.Symbol("instance")

// Instance is an instance of a Flavor.
type Instance struct {
	slip.Scope
	Type *Flavor
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
	b = append(b, obj.Type.name...)
	b = append(b, ' ')
	b = strconv.AppendUint(b, obj.ID(), 16)
	return append(b, '>')
}

// ID returns unique ID for the instance.
func (obj *Instance) ID() uint64 {
	return uint64(uintptr(unsafe.Pointer(obj)))
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
		"flavor": obj.Type.name,
		"id":     strconv.FormatUint(obj.ID(), 16),
		"vars":   vars,
	}
	return simple
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Instance) Hierarchy() []slip.Symbol {
	return obj.Type.Precedence
}

// IsA return true if the instance is of a flavor that inherits from the
// provided flavor.
func (obj *Instance) IsA(class string) bool {
	for _, sym := range obj.Type.Precedence {
		if class == string(sym) {
			return true
		}
	}
	return false
}

// SlotNames returns a list of the slots names for the instance.
func (obj *Instance) SlotNames() []string {
	names := make([]string, 0, len(obj.Vars))
	for k := range obj.Vars {
		names = append(names, k)
	}
	return names
}

// SlotValue return the value of an instance variable.
func (obj *Instance) SlotValue(sym slip.Symbol) (slip.Object, bool) {
	return obj.LocalGet(sym)
}

// SetSlotValue sets the value of an instance variable.
func (obj *Instance) SetSlotValue(sym slip.Symbol, value slip.Object) (has bool) {
	name := strings.ToLower(string(sym))
	if name != "self" {
		obj.Lock()
		if _, has = obj.Vars[name]; has {
			obj.Vars[name] = value

		}
		obj.Unlock()
	}
	return
}

// Init the instance slots from the provided args list. If the scope is not
// nil then send :init is called.
func (obj *Instance) Init(scope *slip.Scope, args slip.List, depth int) {
	obj.Keep = true
	var plist slip.List
	keys := map[string]bool{}
	cf := obj.Type
	for i := 0; i < len(args); i++ {
		sym, ok := args[i].(slip.Symbol)
		if !ok || len(sym) < 2 || sym[0] != ':' {
			slip.PanicType("initialization keyword", args[i], "keyword")
		}
		key := strings.ToLower(string(sym))
		if key == ":self" {
			slip.NewPanic("initialization keyword 'self' is not initable.")
		}
		i++
		val := args[i]
		if len(cf.initable) == 0 || cf.initable[key] {
			vkey := key[1:]
			if _, has := cf.defaultVars[vkey]; has {
				obj.Let(slip.Symbol(vkey), val)
				continue
			}
		}
		keys[key] = true
		if cf.allowOtherKeys {
			plist = append(plist, sym, val)
		} else if _, has := cf.keywords[key]; has {
			plist = append(plist, sym, val)
		} else {
			slip.NewPanic("%s is not an init keyword for flavor %s.", sym, cf.name)
		}
	}
	for _, k := range cf.requiredKeywords {
		if !keys[k] {
			slip.NewPanic("Keyword %s missing from initialization list for flavor %s.", k, cf.name)
		}
	}
	if scope != nil {
		_ = obj.Receive(scope, ":init", slip.List{plist}, depth+1)
	}
}

// HasMethod returns true if the instance handles the named method.
func (obj *Instance) HasMethod(method string) (has bool) {
	if obj.Type.GetMethod(method) != nil {
		has = true
	}
	return
}

// GetMethod returns the method if it exists.
func (obj *Instance) GetMethod(name string) *slip.Method {
	return obj.Type.methods[name]
}

// MethodNames returns a sorted list of the methods of the instance.
func (obj *Instance) MethodNames() slip.List {
	return obj.Type.MethodNames()
}

// Receive a method invocation from the send function. Not intended to be
// called by any code other than the send function but is public to allow it
// to be over-ridden.
func (obj *Instance) Receive(s *slip.Scope, message string, args slip.List, depth int) slip.Object {
	m := obj.Type.GetMethod(message)
	if m == nil {
		xargs := make(slip.List, 0, len(args)+1)
		xargs = append(xargs, slip.Symbol(message))
		xargs = append(xargs, args...)

		return obj.Type.defaultHandler.Call(&obj.Scope, xargs, depth)
	}
	scope := obj.NewScope()
	if s != nil {
		scope.AddParent(s)
	}
	return m.Call(scope, args, depth)
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
		if obj.Any != nil {
			if alt.Compare(obj.Any, o.Any) != nil {
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
	m := obj.Type.GetMethod(message)
	if m == nil {
		if bc, _ := obj.Type.defaultHandler.(slip.BoundCaller); bc != nil {
			s.Let(slip.Symbol("method"), slip.Symbol(message))
			var args slip.List
			for k, v := range bindings.Vars {
				args = append(args, slip.List{slip.Symbol(k), slip.Tail{Value: v}})
			}
			s.Let(slip.Symbol("args"), args)
			return bc.BoundCall(s, depth)
		}
		slip.PanicInvalidMethod(obj, nil, slip.Symbol(message), "%s is not a method of %s.", message, obj.Type.name)
	}
	return m.BoundCall(s, depth)
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
		b = append(b, obj.Type.Name()...)
		b = append(b, colorOff...)
	} else {
		b = append(b, obj.Type.Name()...)
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
func (obj *Instance) Length() (size int) {
	if obj.Type.GetMethod(":length") != nil {
		v := obj.Receive(nil, ":length", slip.List{}, 0)
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

// Class returns the flavor of the instance.
func (obj *Instance) Class() slip.Class {
	return obj.Type
}

// Dup returns a duplicate of the instance.
func (obj *Instance) Dup() slip.Instance {
	dup := Instance{
		Type: obj.Type,
		Any:  obj.Any,
	}
	dup.Vars = map[string]slip.Object{}
	for k, v := range obj.Vars {
		dup.Vars[k] = v
	}
	dup.Vars["self"] = &dup

	return &dup
}

// ChangeFlavor returns a copy of the instance with a new flavor as the
// type. Instance variables that are in each are kept while others are
// removed. New variables are added as unbound.
func (obj *Instance) ChangeFlavor(flavor *Flavor) {
	obj.Type = flavor
	vars := obj.Vars
	obj.Vars = map[string]slip.Object{}
	for k, v := range flavor.defaultVars {
		if ov, has := vars[k]; has {
			v = ov
		}
		obj.Vars[k] = v
	}
}
