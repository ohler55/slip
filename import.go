// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Import is used to identify what package variables are imported.
type Import struct {
	Pkg  *Package
	Name string
}

// Simplify the Object into an int64.
func (imp *Import) Simplify() any {
	return map[string]any{
		"pkg":  imp.Pkg.String(),
		"name": imp.Name,
	}
}
