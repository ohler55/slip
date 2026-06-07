// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Funky is an interface shared by all functions.
type Funky interface {
	Object

	// GetArgs returns the function arguments.
	GetArgs() List

	// GetName returns the function name.
	GetName() string

	// Apply evaluates without the need to evaluate the args..
	Apply(s *Scope, args List, depth int) Object

	// CompileArgs for the function.
	CompileArgs(listProvs ProvSet)

	// Caller returns the function's Caller (Self).
	Caller() Caller

	setPkg(p *Package)

	// GetPkg returns the package the function was defined in.
	GetPkg() *Package

	// SetProvenance sets the provenance for the function.
	SetProvenance(p *Prov)

	// Provenance return the provenance for the function.
	Provenance() *Prov
}
