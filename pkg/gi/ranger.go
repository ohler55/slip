// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import "github.com/ohler55/slip"

// Ranger is an interface for ranging over a collection of values or a channel.
type Ranger interface {
	// Range over the values in a collection or channel.
	Range(s *slip.Scope, caller slip.Caller, depth int)
}
