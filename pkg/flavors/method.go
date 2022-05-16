// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

// method is the combined methods for a Flavor that includes the inherited
// methods with the same name.
type method struct {
	name    string
	from    *Flavor
	inherit []*method // in order
	primary slip.Caller
	before  slip.Caller
	after   slip.Caller
	wrap    slip.Caller
}

// Simplify by returning a representation of the method.
func (m *method) Simplify() any {
	var daemons []any
	full := make([]*method, len(m.inherit)+1)
	full[0] = m
	copy(full[1:], m.inherit)
	for _, m2 := range full {
		if m2.wrap != nil {
			daemons = append(daemons, map[string]any{"flavor": m2.from.name, "type": "whopper"})
		}
	}
	for _, m2 := range full {
		if m2.before != nil {
			daemons = append(daemons, map[string]any{"flavor": m2.from.name, "type": "before"})
		}
	}
	for _, m2 := range full {
		if m2.primary != nil {
			daemons = append(daemons, map[string]any{"flavor": m2.from.name, "type": "primary"})
			break
		}
	}
	for i := len(full) - 1; 0 <= i; i-- {
		m2 := full[i]
		if m2.before != nil {
			daemons = append(daemons, map[string]any{"flavor": m2.from.name, "type": "after"})
		}
	}
	return map[string]any{
		"name":    m.name,
		"daemons": daemons,
	}
}
