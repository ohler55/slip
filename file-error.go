// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// FileErrorSymbol is the symbol with a value of "file-error".
const FileErrorSymbol = Symbol("file-error")

// NewFileError creates a FilePanic (file-error) describing a file error.
func NewFileError(pathname Object, format string, args ...any) Object {
	c := FindClass("file-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":pathname"), pathname,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicFile raises a FilePanic (file-error) describing a file
// error.
func PanicFile(pathname Object, format string, args ...any) {
	panic(NewFileError(pathname, format, args...))
}
