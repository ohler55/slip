// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"fmt"

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
:false. It can be parsed from JSON or SEN (ojg package) and be encoded
in the same way. It can also be converted to and from native LISP with
bag::map becoming an assoc list. The bag::false value is the only
non-native value that is retained since LSIP does not differentiate between
nil and boolean false.`),
				slip.Symbol(":documentation")},
		},
	)
	flavor.DefMethod(":set", "", setCaller(true))

	// TBD add methods
	// get
	// has
	// parse
	// walk
	// to-lisp
	// format
}

// Flavor returns the bag-flavor.
func Flavor() *flavors.Flavor {
	return flavor
}

type setCaller bool

func (caller setCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		panic(fmt.Sprintf("Method bag-flavor set method expects two arguments but received %d.", len(args)))
	}
	set(obj, args)

	return obj
}
