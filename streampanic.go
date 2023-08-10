// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// StreamPanic represents a stream-error.
type StreamPanic struct {
	Panic
	stream Stream
}

// IsStreamError need not do anything other than exist.
func (sp *StreamPanic) IsStreamError() {
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (sp *StreamPanic) Hierarchy() []Symbol {
	return []Symbol{StreamErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (sp *StreamPanic) Eval(s *Scope, depth int) Object {
	return sp
}

// Stream returns the stream associated with the error.
func (sp *StreamPanic) Stream() Stream {
	return sp.stream
}

// PanicStream raises a StreamPanic (stream-error) describing a stream
// error.
func PanicStream(stream Stream, format string, args ...any) {
	panic(&StreamPanic{
		Panic:  Panic{Message: fmt.Sprintf(format, args...)},
		stream: stream,
	})
}
