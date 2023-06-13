// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var suiteFlavor *flavors.Flavor

func init() {
	_ = SuiteFlavor()
}

// SuiteFlavor returns the suite-flavor.
func SuiteFlavor() *flavors.Flavor {
	_ = TestableFlavor()
	if suiteFlavor == nil {
		suiteFlavor = flavors.DefFlavor("suite-flavor",
			map[string]slip.Object{ // variables
				"children": nil,
			},
			[]string{"testable-flavor"}, // inherited
			slip.List{ // options
				slip.List{
					slip.Symbol(":documentation"),
					slip.String(`A suite is a container for child suites and tests.`),
				},
				slip.Symbol(":gettable-instance-variables"),
			},
		)
		// flavor.DefMethod(":run", "", runCaller(true)) // &key filter verbose trace
		// flavor.DefMethod(":report", "", reportCaller(true))
		// flavor.DefMethod(":results", "", resultsCaller(true))
		// flavor.DefMethod(":find", "", findCaller(true)) // path
	}
	return suiteFlavor
}
