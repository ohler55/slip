// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
	"unsafe"
)

// Method represents a method for flavors and CLOS generics.
type Method struct {
	Name         string
	Doc          *FuncDoc
	Combinations []*Combination
}

// Simplify by returning a representation of the method.
func (m *Method) Simplify() any {
	var combos []any
	for _, c := range m.Combinations {
		if !c.Empty() {
			combos = append(combos, c.Simplify())
		}
	}
	simple := map[string]any{
		"name":         m.Name,
		"combinations": combos,
	}
	return simple
}

// Call the method.
func (m *Method) Call(s *Scope, args List, depth int) Object {
	for i, c := range m.Combinations {
		if c.Wrap != nil {
			loc := &WhopLoc{Method: m, Current: i}
			ws := s.NewScope()
			ws.Let("~whopper-location~", loc)
			(c.Wrap.(*Lambda)).Closure = ws

			return c.Wrap.Call(ws, args, depth+1)
		}
	}
	return m.InnerCall(s, args, depth)
}

// InnerCall calls the before, after, and primary method daemons.
func (m *Method) InnerCall(s *Scope, args List, depth int) (result Object) {
	for _, c := range m.Combinations {
		if c.Before != nil {
			c.Before.Call(s, args, depth)
		}
	}
	for _, c := range m.Combinations {
		if c.Primary != nil {
			result = c.Primary.Call(s, args, depth)
			break
		}
	}
	for i := len(m.Combinations) - 1; 0 <= i; i-- {
		c := m.Combinations[i]
		if c.After != nil {
			c.After.Call(s, args, depth)
		}
	}
	return
}

func (m *Method) BoundCall(s *Scope, depth int) Object {
	for i, c := range m.Combinations {
		if c.Wrap != nil {
			loc := &WhopLoc{Method: m, Current: i}
			ws := s.NewScope()
			ws.Let("~whopper-location~", loc)
			(c.Wrap.(*Lambda)).Closure = ws
			if bc, _ := c.Wrap.(BoundCaller); bc != nil {
				return bc.BoundCall(ws, depth)
			}
		}
	}
	return m.BoundInnerCall(s, depth)
}

func (m *Method) BoundInnerCall(s *Scope, depth int) (result Object) {
	for _, c := range m.Combinations {
		if bc, _ := c.Before.(BoundCaller); bc != nil {
			bc.BoundCall(s, depth)
		}
	}
	for _, c := range m.Combinations {
		if bc, _ := c.Primary.(BoundCaller); bc != nil {
			result = bc.BoundCall(s, depth)
			break
		}
	}
	for _, c := range m.Combinations {
		if bc, _ := c.After.(BoundCaller); bc != nil {
			bc.BoundCall(s, depth)
		}
	}
	return
}

// String representation of the Object.
func (m *Method) String() string {
	return string(m.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (m *Method) Append(b []byte) []byte {
	b = append(b, "#<method "...)
	b = append(b, m.Name...)
	for _, c := range m.Combinations {
		if c.Before != nil {
			b = append(b, " :before"...)
			break
		}
	}
	for _, c := range m.Combinations {
		if c.After != nil {
			b = append(b, " :after"...)
			break
		}
	}
	for _, c := range m.Combinations {
		if c.Wrap != nil {
			b = append(b, " :around"...)
			break
		}
	}
	if m.Doc != nil {
		b = append(b, ' ')
		b = printer.Append(b, m.Doc.LoadForm(), 0)
	}
	b = append(b, ' ', '{')
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(m))), 16)

	return append(b, '}', '>')
}

// Equal returns true if this Object and the other are equal in value.
func (m *Method) Equal(other Object) (eq bool) {
	return m == other
}

// Hierarchy returns the class hierarchy as symbols for the whopLoc.
func (m *Method) Hierarchy() []Symbol {
	return []Symbol{MethodSymbol, TrueSymbol}
}

// Eval returns self.
func (m *Method) Eval(s *Scope, depth int) Object {
	return m
}

// HasMethodFromClass return true if a combinations from a class is present.
func (m *Method) HasMethodFromClass(from string) bool {
	for _, c := range m.Combinations {
		if c.From != nil && c.From.Name() == from {
			return true
		}
	}
	return false
}

// CompareArgs compares argument types and panics on a mismatch.
func (m *Method) CompareArgs(fd *FuncDoc) {
	if len(m.Doc.Args) != len(fd.Args) {
		NewPanic("Lambda list mismatch. %d versus %d aruments for %s.", len(m.Doc.Args), len(fd.Args), m.Name)
	}
	if m.Doc.Return != fd.Return {
		NewPanic("Return type mismatch. %s versus %s for %s.", m.Doc.Return, fd.Return, m.Name)
	}
	for i, da := range fd.Args {
		if da.Type != m.Doc.Args[i].Type {
			NewPanic("Lambda list mismatch. %d argument type %s versus %s for %s.",
				i, m.Doc.Args[i].Type, da.Type, m.Name)
		}
	}
}

// CheckMethodArgCount raises a panic describing the wrong number of arguments
// to a method if the argument count is outside the expected bounds.
func CheckMethodArgCount(inst Instance, method string, cnt, mn, mx int) {
	if cnt < mn {
		NewPanic("Too few arguments to the %s %s method. At least %d expected but got %d.",
			inst.Class().Name(), method, mn, cnt)
	}
	if mx != -1 && mx < cnt {
		panic(ErrorNew(NewScope(), 0, "Too many arguments to the %s %s method. At most %d expected but got %d.",
			inst.Class().Name(), method, mx, cnt))
	}
}

// MethodArgCountCheck raises a panic describing the wrong number of arguments
// to a method if the argument count is outside the expected bounds.
func MethodArgCountCheck(s *Scope, depth int, inst Instance, method string, cnt, mn, mx int) {
	if cnt < mn {
		NewPanic("Too few arguments to the %s %s method. At least %d expected but got %d.",
			inst.Class().Name(), method, mn, cnt)
	}
	if mx != -1 && mx < cnt {
		panic(ErrorNew(s, depth, "Too many arguments to the %s %s method. At most %d expected but got %d.",
			inst.Class().Name(), method, mx, cnt))
	}
}

// PanicMethodArgCount raises a panic describing the wrong number of arguments
// to a method.
func PanicMethodArgCount(inst Instance, method string, cnt, mn, mx int) {
	if cnt < mn {
		NewPanic("Too few arguments to the %s %s method. At least %d expected but got %d.",
			inst.Class().Name(), method, mn, cnt)
	}
	panic(ErrorNew(NewScope(), 0, "Too many arguments to the %s %s method. At most %d expected but got %d.",
		inst.Class().Name(), method, mx, cnt))
}

// MethodArgCountPanic raises a panic describing the wrong number of arguments
// to a method.
func MethodArgCountPanic(s *Scope, depth int, inst Instance, method string, cnt, mn, mx int) {
	if cnt < mn {
		ErrorPanic(s, depth, "Too few arguments to the %s %s method. At least %d expected but got %d.",
			inst.Class().Name(), method, mn, cnt)
	}
	panic(ErrorNew(s, depth, "Too many arguments to the %s %s method. At most %d expected but got %d.",
		inst.Class().Name(), method, mx, cnt))
}

// PanicMethodArgChoice raises a panic describing the wrong number of
// arguments to a method when the expected argument are a choice of a set of
// values.
func PanicMethodArgChoice(inst Instance, method string, cnt int, choices string) {
	NewPanic("Wrong number of arguments to the %s %s method. Either %s expected but got %d.",
		inst.Class().Name(), method, choices, cnt)
}
