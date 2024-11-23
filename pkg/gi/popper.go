// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import "github.com/ohler55/slip"

// Popper is an interface for popping values from a channel.
type Popper interface {
	// Pop a value from a channel.
	Pop() slip.Object
}
