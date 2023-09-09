// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"math/big"
	"time"

	"github.com/ohler55/slip"
)

var (
	builtInClass = Class{
		name:  "built-in-class",
		final: true,
	}
	symbolClass = Class{
		name:    "symbol",
		final:   true,
		docs:    "built-in symbol class",
		inherit: []*Class{&builtInClass},
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
	bignumClass = Class{
		name:      "bignum",
		final:     true,
		docs:      "built-in fixed number class",
		inherit:   []*Class{&builtInClass, &integerClass},
		prototype: (*slip.Bignum)(big.NewInt(42)),
	}
	floatClass = Class{
		name:    "float",
		final:   true,
		docs:    "built-in float number class",
		inherit: []*Class{&builtInClass, &realClass},
	}
	doubleFloatClass = Class{
		name:      "double-float",
		final:     true,
		docs:      "built-in double-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: slip.DoubleFloat(42.1),
	}
	singleFloatClass = Class{
		name:      "single-float",
		final:     true,
		docs:      "built-in single-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: slip.SingleFloat(42.1),
	}
	shortFloatClass = Class{
		name:      "short-float",
		final:     true,
		docs:      "built-in short-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: slip.ShortFloat(42.1),
	}
	longFloatClass = Class{
		name:      "long-float",
		final:     true,
		docs:      "built-in long-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: (*slip.LongFloat)(big.NewFloat(42.1)),
	}
	ratioClass = Class{
		name:      "ratio",
		final:     true,
		docs:      "built-in ratio class",
		inherit:   []*Class{&builtInClass, &rationalClass},
		prototype: slip.NewRatio(3, 4),
	}
	sequenceClass = Class{
		name:    "sequence",
		final:   true,
		docs:    "built-in sequence class",
		inherit: []*Class{&builtInClass},
	}
	arrayClass = Class{
		name:    "array",
		final:   true,
		docs:    "built-in array class",
		inherit: []*Class{&builtInClass, &sequenceClass},
	}
	vectorClass = Class{
		name:    "vector",
		final:   true,
		docs:    "built-in vector class",
		inherit: []*Class{&builtInClass, &arrayClass},
	}
	stringClass = Class{
		name:    "string",
		final:   true,
		docs:    "built-in symbol class",
		inherit: []*Class{&builtInClass, &vectorClass},
	}
	complexClass = Class{
		name:      "complex",
		final:     true,
		docs:      "built-in complex number class",
		inherit:   []*Class{&builtInClass, &numberClass},
		prototype: slip.Complex(1 + 2i),
	}
	characterClass = Class{
		name:      "character",
		final:     true,
		docs:      "built-in character class",
		inherit:   []*Class{&builtInClass},
		prototype: slip.Character('A'),
	}
	timeClass = Class{
		name:      "time",
		final:     true,
		docs:      "built-in time class",
		inherit:   []*Class{&builtInClass},
		prototype: slip.Time(time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)),
	}

	// TBD
	// arithmetic-error
	// cell-error
	// class
	// class-not-found
	// control-error
	// error
	// file-error
	// file-stream
	// hash-table
	// input-stream
	// instance - subclass or something for flavor vs clos (is that nevcessary?)
	//   clos::instance
	// method-error
	// output-stream
	// package
	// package-error
	// program-error
	// reader-error
	// serious-condition
	// stream
	// unbound-slot
	// undefined-function
	// values
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
		&arrayClass,
		&bignumClass,
		&builtInClass,
		&characterClass,
		&complexClass,
		&doubleFloatClass,
		&fixnumClass,
		&floatClass,
		&integerClass,
		&longFloatClass,
		&numberClass,
		&ratioClass,
		&rationalClass,
		&realClass,
		&sequenceClass,
		&shortFloatClass,
		&singleFloatClass,
		&stringClass,
		&symbolClass,
		&timeClass,
		&vectorClass,
	} {
		allClasses[c.name] = c
	}
}
