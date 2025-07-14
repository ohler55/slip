// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PackageErrorSymbol is the symbol with a value of "package-error".
const PackageErrorSymbol = Symbol("package-error")

// NewPackageError returns a new PackagePanic (package-error) describing a
// package error.
func NewPackageError(pkg *Package, format string, args ...any) Object {
	c := FindClass("package-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":package"), pkg,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicPackage raises a PackagePanic (package-error) describing a package
// error.
func PanicPackage(pkg *Package, format string, args ...any) {
	panic(NewPackageError(pkg, format, args...))
}
