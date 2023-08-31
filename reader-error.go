// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ReaderErrorSymbol is the symbol with a value of "reader-error".
const ReaderErrorSymbol = Symbol("reader-error")

var readerErrorHierarchy = []Symbol{
	ReaderErrorSymbol,
	ParseErrorSymbol,
	StreamErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("reader-error", makeReaderError)
}

// ReaderError is the interface for all reader-errors.
type ReaderError interface {
	ParseError
	StreamError

	// IsReaderError need not do anything other than exist.
	IsReaderError()
}

// ReaderPanic represents a reader-error.
type ReaderPanic struct {
	ParsePanic
	stream Stream
}

// IsReaderError need not do anything other than exist.
func (rp *ReaderPanic) IsReaderError() {
}

// IsStreamError need not do anything other than exist.
func (rp *ReaderPanic) IsStreamError() {
}

// Stream returns the stream associated with the error.
func (rp *ReaderPanic) Stream() Stream {
	return rp.stream
}

// Equal returns true if this Object and the other are equal in value.
func (rp *ReaderPanic) Equal(other Object) bool {
	return rp == other
}

// Eval the object.
func (rp *ReaderPanic) Eval(s *Scope, depth int) Object {
	return rp
}

// NewReaderError creates a ReaderPanic (reader-error) describing a parse error.
func NewReaderError(stream Stream, format string, args ...any) *ReaderPanic {
	var cond ReaderPanic
	cond.hierarchy = readerErrorHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	cond.stream = stream
	return &cond
}

// PanicParse raises a ReaderPanic (reader-error) describing a parse
// error.
func PanicReader(stream Stream, format string, args ...any) {
	panic(NewReaderError(stream, format, args...))
}

func makeReaderError(args List) Condition {
	var (
		msg    String
		stream Stream
	)
	for k, v := range ParseInitList(args) {
		switch k {
		case ":message":
			msg, _ = v.(String)
		case ":stream":
			stream, _ = v.(Stream)
		}
	}
	return NewReaderError(stream, "%s", string(msg))
}
