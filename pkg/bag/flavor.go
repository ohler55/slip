// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var flavor *flavors.Flavor

func init() {
	flavor = flavors.DefFlavor("bag-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.String(`A bag is flexible a container for data composed of t, nil, integer,
float, string, time, list, as well as the special bag::map and
bag::false. It can be parsed from JSON or SEN (ojg package) and be encoded
in the same way. It can also be converted to and from native LISP with
bag::map becoming an assoc list. The bag::false value is the only
non-native value that is retained since LSIP does not differenciate between
nil and boolean false.`),
				slip.Symbol(":documentation")},
		},
	)

	// TBD add methods
}

// Flavor returns the bag-flavor.
func Flavor() *flavors.Flavor {
	return flavor
}
