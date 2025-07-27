// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import "github.com/ohler55/slip"

type Meth struct {
	slip.Method
	target slip.Symbol
	subs   map[string]*Meth
}

// Aux encapsulates the auxiliary data for a generic function.
type Aux struct {
	docs    *slip.FuncDoc // var-symbol, &optional, &rest, &key &allow-other-keys
	cache   map[string]*Meth
	methods map[string]*Meth
	// TBD mutex
}

// NewAux creates a new generic aux.
func NewAux(fd *slip.FuncDoc) *Aux {
	return &Aux{
		cache:   map[string]*Meth{},
		methods: map[string]*Meth{},
		docs:    fd,
	}
}

// Call the the function with the arguments provided.
func (g *Aux) Call(gf slip.Object, s *slip.Scope, args slip.List, depth int) slip.Object {

	// TBD validate against docs

	// TBD check arg types for cached method then methods if not found

	// No matches on methods so call no-applicable-method (g, args...) which default to a panic
	//  find slip.FindFunc then Kind is generic
	//   maybe add option arg to Package.Define, an any for storing generic?
	//

	// look for no-applicable-method. If not found then panic with no-applicable-method-error

	// The generic function no-applicable-method not found in the current
	// package so raise a no-applicable-method-error. This is a way to handle
	// a new package that does not yet have any generics defined.
	panic(slip.NewNoApplicableMethodError(gf, args, ""))
}

// TBD **** defgeneric should be like any other function

// func (obj *Package) Define(creator func(args List) Object, doc *FuncDoc, aux ... any) {

// TBD for the :method option, use defmethod but in the lisp defgeneric allow :method
// (defgeneric to-list (object)
//   (:method ((object t))
//     (list object))
//   (:method ((object list))
//     object))
