// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	hostentFlavor *flavors.Flavor
)

func defHostent() *flavors.Flavor {
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
	return hostentFlavor
}

func makeHostEnt(name slip.String, addrs slip.List) (inst *flavors.Instance) {
	inst = hostentFlavor.MakeInstance().(*flavors.Instance)
	inst.UnsafeLet(slip.Symbol("name"), name)
	inst.UnsafeLet(slip.Symbol("addresses"), addrs)

	return
}
