// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

// Closer is an interface for closing a channel.
type Closer interface {
	// Close the channel.
	Close()
}
