// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// WhopLocSymbol is the symbol with a value of "whopLoc".
const WhopLocSymbol = Symbol("whopper-location")

type WhopLoc struct {
	Method  *Method
	Current int
}

// String representation of the Object.
func (wl *WhopLoc) String() string {
	return string(wl.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (wl *WhopLoc) Append(b []byte) []byte {
	return fmt.Appendf(b, "#<whopper-location %d>", wl.Current)
}

// Simplify by returning the string representation of the flavor.
func (wl *WhopLoc) Simplify() interface{} {
	return wl.String()
}

// Equal returns true if this Object and the other are equal in value.
func (wl *WhopLoc) Equal(other Object) (eq bool) {
	return wl == other
}

// Hierarchy returns the class hierarchy as symbols for the whopLoc.
func (wl *WhopLoc) Hierarchy() []Symbol {
	return []Symbol{WhopLocSymbol, TrueSymbol}
}

// Eval returns self.
func (wl *WhopLoc) Eval(s *Scope, depth int) Object {
	return wl
}

func (wl *WhopLoc) Continue(s *Scope, args List, depth int) Object {
	for wl.Current++; wl.Current < len(wl.Method.Combinations); wl.Current++ {
		wrap := wl.Method.Combinations[wl.Current].Wrap
		if wrap == nil {
			continue
		}
		ws := s.NewScope()
		ws.Let("~whopper-location~", &WhopLoc{Method: wl.Method, Current: wl.Current + 1})
		if lam, ok := wrap.(*Lambda); ok {
			lam.Closure = ws
		}
		return wrap.Call(ws, args, depth+1)
	}
	return wl.Method.InnerCall(s, args, depth)
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj *WhopLoc) LoadForm() Object {
	return obj
}
