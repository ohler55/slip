// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.DefConstant(slip.Symbol("pi"), slip.DoubleFloat(math.Pi), "The value of PI.")
	slip.DefConstant(slip.Symbol("e"), slip.DoubleFloat(math.E), "The value of e.")
	slip.DefConstant(slip.Symbol("boole-1"), slip.Symbol("boole-1"), "boole-1")
	slip.DefConstant(slip.Symbol("boole-2"), slip.Symbol("boole-2"), "boole-2")
	slip.DefConstant(slip.Symbol("boole-and"), slip.Symbol("boole-and"), "boole-and")
	slip.DefConstant(slip.Symbol("boole-andc1"), slip.Symbol("boole-andc1"), "boole-andc1")
	slip.DefConstant(slip.Symbol("boole-andc2"), slip.Symbol("boole-andc2"), "boole-andc2")
	slip.DefConstant(slip.Symbol("boole-c1"), slip.Symbol("boole-c1"), "boole-c1")
	slip.DefConstant(slip.Symbol("boole-c2"), slip.Symbol("boole-c2"), "boole-c2")
	slip.DefConstant(slip.Symbol("boole-clr"), slip.Symbol("boole-clr"), "boole-clr")
	slip.DefConstant(slip.Symbol("boole-eqv"), slip.Symbol("boole-eqv"), "boole-eqv")
	slip.DefConstant(slip.Symbol("boole-ior"), slip.Symbol("boole-ior"), "boole-ior")
	slip.DefConstant(slip.Symbol("boole-nand"), slip.Symbol("boole-nand"), "boole-nand")
	slip.DefConstant(slip.Symbol("boole-nor"), slip.Symbol("boole-nor"), "boole-nor")
	slip.DefConstant(slip.Symbol("boole-orc1"), slip.Symbol("boole-orc1"), "boole-orc1")
	slip.DefConstant(slip.Symbol("boole-orc2"), slip.Symbol("boole-orc2"), "boole-orc2")
	slip.DefConstant(slip.Symbol("boole-set"), slip.Symbol("boole-set"), "boole-set")
	slip.DefConstant(slip.Symbol("boole-xor"), slip.Symbol("boole-xor"), "boole-xor")
}
