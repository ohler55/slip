// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	hostentFlavor *flavors.Flavor
)

func defHostent() {
	hostentFlavor = flavors.DefFlavor("host-ent",
		map[string]slip.Object{
			"name":      nil,
			"addresses": nil,
		},
		nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`An address lookup result.`),
			},
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
			slip.Symbol(":settable-instance-variables"),
		},
		&Pkg,
	)
}
