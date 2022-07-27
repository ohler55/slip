// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.DefConstant(slip.Symbol("pi"), slip.DoubleFloat(math.Pi), "The value of PI.")
	slip.DefConstant(slip.Symbol("e"), slip.DoubleFloat(math.E), "The value of e.")
}
