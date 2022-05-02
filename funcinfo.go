// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// FuncInfo stores information about a function.
type FuncInfo struct {
	Name   string
	Create func(args List) Object
	Doc    *FuncDoc
	Pkg    *Package // package interned in
}
