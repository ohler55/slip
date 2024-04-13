// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PackageErrorSymbol is the symbol with a value of "package-error".
const PackageErrorSymbol = Symbol("package-error")

var packageErrorHierarchy = []Symbol{
	PackageErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

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

// Equal returns true if this Object and the other are equal in value.
func (pp *PackagePanic) Equal(other Object) bool {
	return pp == other
}

// Eval the object.
func (pp *PackagePanic) Eval(s *Scope, depth int) Object {
	return pp
}

// NewPackageError returns a new PackagePanic (package-error) describing a
// package error.
func NewPackageError(pkg *Package, format string, args ...any) *PackagePanic {
	var pp PackagePanic
	pp.hierarchy = packageErrorHierarchy
	pp.pkg = pkg
	pp.Message = fmt.Sprintf(format, args...)
	return &pp
}

// PanicPackage raises a PackagePanic (package-error) describing a package
// error.
func PanicPackage(pkg *Package, format string, args ...any) {
	panic(NewPackageError(pkg, format, args...))
}

func makePackageError(args List) Condition {
	var (
		pkg *Package
		msg String
	)
	for k, v := range ParseInitList(args) {
		switch k {
		case ":package":
			pkg, _ = v.(*Package)
		case ":message":
			msg, _ = v.(String)
		}
	}
	return NewPackageError(pkg, "%s", string(msg))
}
