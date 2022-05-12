// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

// method is the combined methods for a Flavor that includes the inherited
// methods with the same name.
type method struct {
	name    string
	from    *Flavor
	inherit []*method // in order
	daemon  slip.Caller
	before  slip.Caller
	after   slip.Caller
	wrap    slip.Caller
}
