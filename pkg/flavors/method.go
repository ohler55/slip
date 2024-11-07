// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

// Method is the combined methods for a Flavor that includes the inherited
// methods with the same name.
type Method struct {
	Name    string
	From    slip.Class
	primary slip.Caller
	before  slip.Caller
	after   slip.Caller
	wrap    slip.Caller
}

func (m *Method) empty() bool {
	return m.primary == nil && m.wrap == nil && m.before == nil && m.after == nil
}

// Simplify by returning a representation of the method.
func (m *Method) Simplify() any {
	simple := map[string]any{
		"name": m.Name,
		"from": m.From.Name(),
	}
	if m.wrap != nil {
		simple["whopper"] = true
	}
	if m.before != nil {
		simple["before"] = true
	}
	if m.primary != nil {
		simple["primary"] = true
	}
	if m.after != nil {
		simple["after"] = true
	}
	return simple
}

// CheckMethodArgCount raises a panic describing the wrong number of arguments
// to a method if the argument count is outside the expected bounds.
func CheckMethodArgCount(inst *Instance, method string, cnt, min, max int) {
	if cnt < min {
		slip.NewPanic("Too few arguments to the %s %s method. At least %d expected but got %d.",
			inst.Type.Name(), method, min, cnt)
	}
	if max != -1 && max < cnt {
		panic(slip.NewError("Too many arguments to the %s %s method. At most %d expected but got %d.",
			inst.Type.Name(), method, max, cnt))
	}
}

// PanicMethodArgCount raises a panic describing the wrong number of arguments
// to a method.
func PanicMethodArgCount(inst *Instance, method string, cnt, min, max int) {
	if cnt < min {
		slip.NewPanic("Too few arguments to the %s %s method. At least %d expected but got %d.",
			inst.Type.Name(), method, min, cnt)
	}
	panic(slip.NewError("Too many arguments to the %s %s method. At most %d expected but got %d.",
		inst.Type.Name(), method, max, cnt))
}

// PanicMethodArgChoice raises a panic describing the wrong number of
// arguments to a method when the expected argument are a choice of a set of
// values.
func PanicMethodArgChoice(inst *Instance, method string, cnt int, choices string) {
	slip.NewPanic("Wrong number of arguments to the %s %s method. Either %s expected but got %d.",
		inst.Type.Name(), method, choices, cnt)
}
