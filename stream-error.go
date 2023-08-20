// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// StreamErrorSymbol is the symbol with a value of "stream-error".
const StreamErrorSymbol = Symbol("stream-error")

var streamErrorHierarch = []Symbol{StreamErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}

func init() {
	RegisterCondition("stream-error", makeStreamError)
}

// StreamError is the interface for all stream-errors.
type StreamError interface {
	Error

	// IsStreamError need not do anything other than exist.
	IsStreamError()

	// Stream returns the stream associated with the error.
	Stream() Stream
}

// StreamPanic represents a stream-error.
type StreamPanic struct {
	Panic
	stream Stream
}

// IsStreamError need not do anything other than exist.
func (sp *StreamPanic) IsStreamError() {
}

// Stream returns the stream associated with the error.
func (sp *StreamPanic) Stream() Stream {
	return sp.stream
}

// Equal returns true if this Object and the other are equal in value.
func (sp *StreamPanic) Equal(other Object) bool {
	return sp == other
}

// Eval the object.
func (sp *StreamPanic) Eval(s *Scope, depth int) Object {
	return sp
}

// NewStreamError returns a new StreamPanic (stream-error) describing a stream
// error.
func NewStreamError(stream Stream, format string, args ...any) *StreamPanic {
	var sp StreamPanic
	sp.hierarchy = streamErrorHierarch
	sp.Message = fmt.Sprintf(format, args...)
	sp.stream = stream
	return &sp
}

// PanicStream raises a StreamPanic (stream-error) describing a stream
// error.
func PanicStream(stream Stream, format string, args ...any) {
	panic(NewStreamError(stream, format, args...))
}

func makeStreamError(args List) Condition {
	var (
		stream Stream
		msg    String
	)

	for k, v := range parseInitList(args) {
		switch k {
		case ":stream":
			stream, _ = v.(Stream)
		case ":message":
			msg, _ = v.(String)
		}
	}
	return NewStreamError(stream, "%s", string(msg))
}
