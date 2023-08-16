// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PackageErrorSymbol is the symbol with a value of "package-error".
const PackageErrorSymbol = Symbol("package-error")

func init() {
	RegisterCondition("package-error", makePackageError)
}

// PackageError is the interface for all package-errors.
type PackageError interface {
	Error

	// IsPackageError need not do anything other than exist.
	IsPackageError()

	// Package is the package the error was in.
	Package() *Package
}

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

func makePackageError(args List) Condition {
	c := &PackagePanic{}
	for k, v := range parseInitList(args) {
		if k == ":package" {
			c.pkg, _ = v.(*Package)
		}
	}
	return c
}
