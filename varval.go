// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// VarVal hold the information about a variable value.
type VarVal struct {
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
