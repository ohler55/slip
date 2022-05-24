// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

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
