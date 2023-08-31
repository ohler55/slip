// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// FileErrorSymbol is the symbol with a value of "file-error".
const FileErrorSymbol = Symbol("file-error")

var fileErrorHierarchy = []Symbol{
	FileErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("file-error", makeFileError)
}

// FileError is the interface for all file-errors.
type FileError interface {
	Error

	// IsFileError need not do anything other than exist.
	IsFileError()

	// Pathname of the error.
	Pathname() Object
}

// FilePanic represents a file-error.
type FilePanic struct {
	Panic
	pathname Object
}

// IsFileError need not do anything other than exist.
func (fp *FilePanic) IsFileError() {
}

// Pathname of the error.
func (fp *FilePanic) Pathname() Object {
	return fp.pathname
}

// Equal returns true if this Object and the other are equal in value.
func (fp *FilePanic) Equal(other Object) bool {
	return fp == other
}

// Eval the object.
func (fp *FilePanic) Eval(s *Scope, depth int) Object {
	return fp
}

// NewFileError creates a FilePanic (file-error) describing a file error.
func NewFileError(pathname Object, format string, args ...any) *FilePanic {
	var cond FilePanic
	cond.hierarchy = fileErrorHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	cond.pathname = pathname
	return &cond
}

// PanicFile raises a FilePanic (file-error) describing a file
// error.
func PanicFile(pathname Object, format string, args ...any) {
	panic(NewFileError(pathname, format, args...))
}

func makeFileError(args List) Condition {
	var (
		msg      String
		pathname Object
	)
	for k, v := range ParseInitList(args) {
		switch k {
		case ":pathname":
			pathname = v
		case ":message":
			msg, _ = v.(String)
		}
	}
	return NewFileError(pathname, "%s", string(msg))
}
