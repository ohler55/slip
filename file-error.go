// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// FileErrorSymbol is the symbol with a value of "file-error".
const FileErrorSymbol = Symbol("file-error")

// FileErrorNew creates a FilePanic (file-error) describing a file error.
func FileErrorNew(s *Scope, depth int, pathname Object, format string, args ...any) Object {
	c := FindClass("file-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":pathname"), pathname,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// FilePanic raises a FilePanic (file-error) describing a file
// error.
func FilePanic(s *Scope, depth int, pathname Object, format string, args ...any) {
	panic(FileErrorNew(s, depth, pathname, format, args...))
}
