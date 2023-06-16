// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

// Receiver handles methods invoked with the send function. Typically these
// are Instances or a type that embeds an Instance.
type Receiver interface {
	// Receive a method invocation from the send function. Not intended to be
	// called by any code other than the send function but is public to allow
	// it to be over-ridden.
	Receive(s *slip.Scope, message string, args slip.List, depth int) slip.Object
}
