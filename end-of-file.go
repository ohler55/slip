// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// EndOfFileSymbol is the symbol with a value of "end-of-file".
const EndOfFileSymbol = Symbol("end-of-file")

var endOfFileHierarch = []Symbol{
	EndOfFileSymbol,
	StreamErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("end-of-file", makeEndOfFile)
}

// EndOfFilePanic represents a end-of-file.
type EndOfFilePanic struct {
	Panic
	stream Stream
}

// IsStreamError need not do anything other than exist.
func (sp *EndOfFilePanic) IsStreamError() {
}

// Stream returns the stream associated with the error.
func (sp *EndOfFilePanic) Stream() Stream {
	return sp.stream
}

// Equal returns true if this Object and the other are equal in value.
func (sp *EndOfFilePanic) Equal(other Object) bool {
	return sp == other
}

// Eval the object.
func (sp *EndOfFilePanic) Eval(s *Scope, depth int) Object {
	return sp
}

// NewEndOfFile returns a new EndOfFilePanic (end-of-file) describing a stream
// error.
func NewEndOfFile(stream Stream, format string, args ...any) *EndOfFilePanic {
	var eofp EndOfFilePanic
	eofp.hierarchy = endOfFileHierarch
	eofp.Message = fmt.Sprintf(format, args...)
	eofp.stream = stream
	return &eofp
}

// PanicStream raises a EndOfFilePanic (end-of-file) describing a stream
// error.
func PanicEndOfFile(stream Stream, format string, args ...any) {
	panic(NewEndOfFile(stream, format, args...))
}

func makeEndOfFile(args List) Condition {
	var (
		stream Stream
		msg    String
	)
	for k, v := range ParseInitList(args) {
		switch k {
		case ":stream":
			stream, _ = v.(Stream)
		case ":message":
			msg, _ = v.(String)
		}
	}
	return NewEndOfFile(stream, "%s", string(msg))
}
