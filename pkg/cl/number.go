// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func syncFloatPrec(v0, v1 *slip.LongFloat) {
	p0 := (*big.Float)(v0).Prec()
	p1 := (*big.Float)(v1).Prec()
	// The big.Float adds random digits when increasing precision so
	// increase the hard way by converting to a string and re-parsing.
	if p0 < p1 {
		s := (*big.Float)(v0).Text('e', -1)
		(*big.Float)(v0).SetPrec(p1)
		_, _, _ = (*big.Float)(v0).Parse(s, 10)
	} else if p1 < p0 {
		s := (*big.Float)(v1).Text('e', -1)
		(*big.Float)(v1).SetPrec(p0)
		_, _, _ = (*big.Float)(v1).Parse(s, 10)
	}
}
