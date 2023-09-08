// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import "github.com/ohler55/slip"

var (
	builtInClass = Class{
		name:  "built-in-class",
		final: true,
	}
	numberClass = Class{
		name:    "number",
		final:   true,
		docs:    "built-in number class",
		inherit: []*Class{&builtInClass},
	}
	realClass = Class{
		name:    "real",
		final:   true,
		docs:    "built-in real number class",
		inherit: []*Class{&builtInClass, &numberClass},
	}
	rationalClass = Class{
		name:    "rational",
		final:   true,
		docs:    "built-in rational number class",
		inherit: []*Class{&builtInClass, &realClass},
	}
	integerClass = Class{
		name:    "integer",
		final:   true,
		docs:    "built-in integer class",
		inherit: []*Class{&builtInClass, &rationalClass},
	}
	fixnumClass = Class{
		name:      "fixnum",
		final:     true,
		docs:      "built-in fixed number class",
		inherit:   []*Class{&builtInClass, &integerClass},
		prototype: slip.Fixnum(42),
	}
	// TBD
	// arithmetic-error
	// array
	// bignum
	// cell-error
	// character
	// class
	// class-not-found
	// complex
	// control-error
	// double-float
	// error
	// file-error
	// file-stream
	// float
	// hash-table
	// input-stream
	// instance - subclass or something for flavor vs clos (is that nevcessary?)
	//   clos::instance
	// long-float
	// method-error
	// output-stream
	// package
	// package-error
	// program-error
	// ratio
	// reader-error
	// sequence
	// serious-condition
	// short-float
	// single-float
	// stream
	// string
	// symbol
	// time
	// unbound
	// unbound-slot
	// undefined-function
	// values
	// vector
	// warning
	// TBD types in cl, flavors, bag
	// --- gi
	// logger
	// channel
	// --- bag
	// bag-path
	// --- flavors
	// flavor
	// instance (flavors::instance)
	// --- cl
	// simple-condition
	// simple-error
	// simple-warning
	// simple-type-error
	//
	//  put all in clos to eliminate need for import tweaking

)

func init() {
	for _, c := range []*Class{
		&builtInClass,
		&numberClass,
		&realClass,
		&rationalClass,
		&integerClass,
		&fixnumClass,
	} {
		allClasses[c.name] = c
	}
}
