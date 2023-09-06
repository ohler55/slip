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
		inherit: []*Class{&builtInClass, &realClass, &numberClass},
	}
	integerClass = Class{
		name:    "integer",
		final:   true,
		docs:    "built-in integer class",
		inherit: []*Class{&builtInClass, &rationalClass, &realClass, &numberClass},
	}
	fixnumClass = Class{
		name:      "fixnum",
		final:     true,
		docs:      "built-in fixed number class",
		inherit:   []*Class{&builtInClass, &integerClass, &rationalClass, &realClass, &numberClass},
		prototype: slip.Fixnum(42),
	}
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
