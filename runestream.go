// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"io"
)

// RuneStream defines functions to support reading streams of runes.
type RuneStream interface {
	io.Reader
	io.Closer
	io.RuneScanner
	io.ByteReader

	// IsOpen return true if the stream is open or false if not. Note that EOF
	// also indicates closed.
	IsOpen() bool
}
