// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// StreamSymbol is the symbol with a value of "stream".
const StreamSymbol = Symbol("stream")

func init() {
	DefConstant(StreamSymbol, StreamSymbol,
		`A _stream_ is an object that can be used with input or output function.`)
}

// Stream exists to allow assertions to determine if an Object is an stream.
type Stream interface {
	Object

	// StreamType returns the stream type of the instance which can be one of: file-stream
	StreamType() Symbol
}
