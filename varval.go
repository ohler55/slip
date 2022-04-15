// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// VarVal hold the information about a variable value.
type VarVal struct {
	Val Object
	Get func() Object
	Set func(Object)
	Doc string
	Pkg *Package // package interned in
}

// Simplify the Object into an int64.
func (vv *VarVal) Simplify() interface{} {
	simple := map[string]interface{}{
		// TBD get and or set
		"doc": vv.Doc,
	}
	if vv.Val != nil {
		simple["val"] = vv.Val.Simplify()
	}
	if vv.Pkg != nil {
		simple["pkg"] = vv.Pkg.Name
	}
	return simple
}
