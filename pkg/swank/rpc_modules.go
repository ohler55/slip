// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:swank-require", handleSwankRequire)
}

// handleSwankRequire loads optional Swank modules.
// Currently we just acknowledge the request and return an empty list.
func handleSwankRequire(c *Connection, args slip.List) slip.Object {
	// Return empty list of loaded modules
	return slip.List{}
}
