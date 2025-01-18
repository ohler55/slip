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
	rest   bool
}

// SetCompileScript sets the World instance for the jp.CompileScript function.
func SetCompileScript(s *slip.Scope) {
	if s == nil {
		s = slip.NewScope()
	}
	jp.CompileScript = func(code []byte) jp.Procedure {
		obj := slip.Compile(code).(any)
		var (
			doc    *slip.FuncDoc
			caller slip.Caller
			rest   bool
		)
	top:
		switch to := obj.(type) {
		case *slip.Dynamic:
			obj = to.Self
			goto top
		case *slip.Lambda:
			caller = to
			doc = to.Doc
		case slip.Funky:
			caller = to.Caller()
			doc = slip.CurrentPackage.GetFunc(to.GetName()).Doc
		}
		for _, fa := range doc.Args {
			if fa.Name == "&rest" {
				rest = true
				break
			}
		}
		return &Procedure{
			scope:  s,
			caller: caller,
			rest:   rest,
		}
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
	var (
		args slip.List
		ok   bool
	)
	if args, ok = obj.(slip.List); !ok || !p.rest {
		args = slip.List{obj}
	}
	return slip.Simplify(p.caller.Call(p.scope, args, 0))
}
