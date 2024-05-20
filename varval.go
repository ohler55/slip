// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slip

import "strings"

// VarVal hold the information about a variable value.
type VarVal struct {
	name   string
	Val    Object
	Get    func() Object
	Set    func(Object)
	Doc    string
	Pkg    *Package // package interned in
	Export bool
}

// Simplify the Object into an int64.
func (vv *VarVal) Simplify() interface{} {
	simple := map[string]interface{}{"doc": vv.Doc}

	var val interface{}
	if v := vv.Value(); v != nil {
		if pkg, ok := v.(*Package); ok {
			val = pkg.Name
		} else {
			val = v.Simplify()
		}
	}
	simple["val"] = val

	if vv.Pkg != nil {
		simple["pkg"] = vv.Pkg.Name
	}
	return simple
}

// Value returns the value of the instance.
func (vv *VarVal) Value() (val Object) {
	switch {
	case vv.Val != nil:
		val = vv.Val
	case vv.Get != nil:
		val = vv.Get()
	}
	return
}

// String representation of the instance.
func (vv *VarVal) String() string {
	return vv.name
}

// Append the object to a byte slice.
func (vv *VarVal) Append(b []byte) []byte {
	return append(b, vv.name...)
}

// Equal returns true if this Object and the other are equal in value.
func (vv *VarVal) Equal(other Object) bool {
	switch to := other.(type) {
	case *VarVal:
		return strings.EqualFold(to.name, vv.name)
	case Symbol:
		return strings.EqualFold(string(to), vv.name)
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (vv *VarVal) Hierarchy() []Symbol {
	return []Symbol{SymbolSymbol, TrueSymbol}
}

// Eval the object.
func (vv *VarVal) Eval(s *Scope, depth int) Object {
	return vv.Value()
}

func newUnboundVar(name string) *VarVal {
	return &VarVal{
		name: name,
		Val:  Unbound,
	}
}
