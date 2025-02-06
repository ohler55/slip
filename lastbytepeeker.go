// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// LastBytePeeker is an interface for checking the last byte written.
type LastBytePeeker interface {

	// LastByte returns the last byte written or zero if nothing has been written.
	LastByte() byte
}
