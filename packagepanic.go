// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PackagePanic represents a package-error.
type PackagePanic struct {
	Panic
	pkg *Package
}

// IsPackageError need not do anything other than exist.
func (pp *PackagePanic) IsPackageError() {
}

// Package is the package the error was in.
func (pp *PackagePanic) Package() *Package {
	return pp.pkg
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (pp *PackagePanic) Hierarchy() []Symbol {
	return []Symbol{PackageErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (pp *PackagePanic) Eval(s *Scope, depth int) Object {
	return pp
}

// PanicPackage raises a PackagePanic (package-error) describing a package
// error.
func PanicPackage(pkg *Package, format string, args ...any) {
	panic(&PackagePanic{
		pkg:   pkg,
		Panic: Panic{Message: fmt.Sprintf(format, args...)},
	})
}
