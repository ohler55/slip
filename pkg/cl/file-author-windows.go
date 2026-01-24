//go:build windows

package cl

import "github.com/ohler55/slip"

func getFileOwner(s *slip.Scope, path string) string {
	// On Windows, file-author is not supported and should raise a file-error
	// according to the Common Lisp HyperSpec.
	panic(slip.FileErrorNew(s, 0, slip.String(path), "file-author is not supported on Windows"))
}
