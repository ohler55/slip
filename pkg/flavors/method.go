// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"

	"github.com/ohler55/slip"
)

// method is the combined methods for a Flavor that includes the inherited
// methods with the same name.
type method struct {
	name    string
	from    *Flavor
	primary slip.Caller
	before  slip.Caller
	after   slip.Caller
	wrap    slip.Caller
}

func (m *method) empty() bool {
	return m.primary == nil && m.wrap == nil && m.before == nil && m.after == nil
}

// Simplify by returning a representation of the method.
func (m *method) Simplify() any {
	simple := map[string]any{
		"name": m.name,
		"from": m.from.name,
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

// PanicMethodArgCount raises a panic describing the wrong number of arguments
// to a method.
func PanicMethodArgCount(inst *Instance, method string, cnt, min, max int) {
	if cnt < min {
		panic(&slip.Panic{
			Message: fmt.Sprintf("Too few arguments to the %s %s method. At least %d expected but got %d.",
				inst.Flavor.name, method, min, cnt),
		})
	}
	panic(&slip.Panic{
		Message: fmt.Sprintf("Too many arguments to the %s %s method. At most %d expected but got %d.",
			inst.Flavor.name, method, min, cnt),
	})
}

// PanicMethodArgChoice raises a panic describing the wrong number of
// arguments to a method when the expected argument are a choice of a set of
// values.
func PanicMethodArgChoice(inst *Instance, method string, cnt int, choices string) {
	panic(&slip.Panic{
		Message: fmt.Sprintf("Wrong number of arguments to the %s %s method. Either %s expected but got %d.",
			inst.Flavor.name, method, choices, cnt),
	})
}
