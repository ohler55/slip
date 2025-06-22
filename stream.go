// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// StreamSymbol is the symbol with a value of "stream".
const StreamSymbol = Symbol("stream")

// Stream exists to allow assertions to determine if an Object is an stream.
type Stream interface {
	Object

	// StreamType returns the stream type of the instance which can be one of: file-stream
	StreamType() Symbol

	// IsOpen return true if the stream is open or false if not or it can not
	// be determined.
	IsOpen() bool
}
