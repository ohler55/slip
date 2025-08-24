// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PackageErrorSymbol is the symbol with a value of "package-error".
const PackageErrorSymbol = Symbol("package-error")

// PackageErrorNew returns a new PackageError (package-error) describing a
// package error.
func PackageErrorNew(s *Scope, depth int, pkg *Package, format string, args ...any) Object {
	c := FindClass("package-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":package"), pkg,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// PackagePanic raises a PackageError (package-error) describing a package
// error.
func PackagePanic(s *Scope, depth int, pkg *Package, format string, args ...any) {
	panic(PackageErrorNew(s, depth, pkg, format, args...))
}
