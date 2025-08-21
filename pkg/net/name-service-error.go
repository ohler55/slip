// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
)

// NameServiceErrorSymbol is the symbol with a value of "NameServiceError".
const NameServiceErrorSymbol = slip.Symbol("name-service-error")

func defNameServiceError() {
	s := slip.NewScope()
	clos.DefConditionClass(s, "name-service-error", slip.List{slip.ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A __name-service-error__ represents an error with the network naming service.`),
			},
		},
		0).Final = true
}
