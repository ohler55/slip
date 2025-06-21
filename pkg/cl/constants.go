// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.DefConstant(&slip.CLPkg, "pi", slip.DoubleFloat(math.Pi), "The value of PI.")
	slip.DefConstant(&slip.CLPkg, "e", slip.DoubleFloat(math.E), "The value of e.")
	slip.DefConstant(&slip.CLPkg, "boole-1", slip.Symbol("boole-1"), "boole-1")
	slip.DefConstant(&slip.CLPkg, "boole-2", slip.Symbol("boole-2"), "boole-2")
	slip.DefConstant(&slip.CLPkg, "boole-and", slip.Symbol("boole-and"), "boole-and")
	slip.DefConstant(&slip.CLPkg, "boole-andc1", slip.Symbol("boole-andc1"), "boole-andc1")
	slip.DefConstant(&slip.CLPkg, "boole-andc2", slip.Symbol("boole-andc2"), "boole-andc2")
	slip.DefConstant(&slip.CLPkg, "boole-c1", slip.Symbol("boole-c1"), "boole-c1")
	slip.DefConstant(&slip.CLPkg, "boole-c2", slip.Symbol("boole-c2"), "boole-c2")
	slip.DefConstant(&slip.CLPkg, "boole-clr", slip.Symbol("boole-clr"), "boole-clr")
	slip.DefConstant(&slip.CLPkg, "boole-eqv", slip.Symbol("boole-eqv"), "boole-eqv")
	slip.DefConstant(&slip.CLPkg, "boole-ior", slip.Symbol("boole-ior"), "boole-ior")
	slip.DefConstant(&slip.CLPkg, "boole-nand", slip.Symbol("boole-nand"), "boole-nand")
	slip.DefConstant(&slip.CLPkg, "boole-nor", slip.Symbol("boole-nor"), "boole-nor")
	slip.DefConstant(&slip.CLPkg, "boole-orc1", slip.Symbol("boole-orc1"), "boole-orc1")
	slip.DefConstant(&slip.CLPkg, "boole-orc2", slip.Symbol("boole-orc2"), "boole-orc2")
	slip.DefConstant(&slip.CLPkg, "boole-set", slip.Symbol("boole-set"), "boole-set")
	slip.DefConstant(&slip.CLPkg, "boole-xor", slip.Symbol("boole-xor"), "boole-xor")
}
