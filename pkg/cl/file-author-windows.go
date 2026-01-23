//go:build windows

package cl

import "errors"

func getFileOwner(path string) (string, error) {
	// On Windows, file-author is not supported and should raise a file-error
	// according to the Common Lisp HyperSpec.
	return "", errors.New("file-author is not supported on Windows")
}
