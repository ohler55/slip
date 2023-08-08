// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// PackageErrorSymbol is the symbol with a value of "package-error".
const PackageErrorSymbol = Symbol("package-error")

// PackageError is the interface for all package-errors.
type PackageError interface {
	Error

	// IsPackageError need not do anything other than exist.
	IsPackageError()

	// Package is the package the error was in.
	Package() *Package
}
