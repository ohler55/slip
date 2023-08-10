// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// StreamErrorSymbol is the symbol with a value of "stream-error".
const StreamErrorSymbol = Symbol("stream-error")

// StreamError is the interface for all stream-errors.
type StreamError interface {
	Error

	// IsStreamError need not do anything other than exist.
	IsStreamError()

	// Stream returns the stream associated with the error.
	Stream() Stream
}
