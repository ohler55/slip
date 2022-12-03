// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math"
)

// ShortFloatSymbol is the symbol with a value of "shortFloat".
const ShortFloatSymbol = Symbol("short-float")

func init() {
	DefConstant(ShortFloatSymbol, ShortFloatSymbol,
		`A _short-float_ represents a decimal _number_ or _float_. It is implemented
as a float32 as defined by IEEE 754 as a short precision decimal with 7 significant
digits and a maximum exponent of 38.`)
	DefConstant(Symbol("most-positive-short-float"), ShortFloat(math.MaxFloat32),
		"The most positive value a _short-float_ can have.")
	DefConstant(Symbol("most-negative-short-float"), ShortFloat(-math.MaxFloat32),
		"The most negative value a _short-float_ can have.")
	DefConstant(Symbol("least-positive-short-float"), ShortFloat(math.SmallestNonzeroFloat32),
		"The smallest non-zero positive value a _short-float_ can have.")
	DefConstant(Symbol("least-negative-short-float"), ShortFloat(-math.SmallestNonzeroFloat32),
		"The smallest non-zero negative value a _short-float_ can have.")
	DefConstant(Symbol("short-float-epsilon"), ShortFloat(5.960465e-8),
		`The smallest positive _short-float_ such the addition of the epsilon value to
1.0s0 returns a value greater than 1.0s0.`)
	DefConstant(Symbol("short-float-negative-epsilon"), ShortFloat(5.960465e-8),
		`The smallest positive _short-float_ such the subtraction of the epsilon value from
1.0s0 returns a value less than 1.0s0.`)
}

// ShortFloat is a float32 Object.
type ShortFloat = SingleFloat
