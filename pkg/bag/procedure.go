// Copyright (c) 2025, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
)

// Procedure defines the interface for functions for script fragments between
// [( and )] delimiters.
type Procedure struct {
	scope  *slip.Scope
	caller slip.Caller
}

// SetCompileScript sets the World instance for the jp.CompileScript function.
func SetCompileScript(s *slip.Scope) {
	jp.CompileScript = func(code []byte) jp.Procedure {
		p := CompileScript(code).(*Procedure)
		p.scope = s
		return p
	}
}

// CompileScript creates a new Procedure instance.
func CompileScript(code []byte) jp.Procedure {
	return &Procedure{
		// TBD caller
	}
}

// Get returns the result of evaluating a list of all matching elements.
func (p *Procedure) Get(data any) (result []any) {
	v := p.eval(data)
	switch tv := v.(type) {
	case nil:
		// leave as nil
	case []any:
		result = tv
	default:
		result = []any{v}
	}
	return
}

// First should return a single matching in the data element or nil if
// there are no matches.
func (p *Procedure) First(data any) any {
	return p.eval(data)
}

func (p *Procedure) eval(data any) (result any) {
	obj := slip.SimpleObject(data)

	return slip.Simplify(p.caller.Call(p.scope, slip.List{obj}, 0))
}
