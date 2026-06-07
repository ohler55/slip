// Copyright (c) 2026, Peter Ohler, All rights reserved.

package slip

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
)

var (
	coverage      = false
	coverageFuncs []Funky
)

// StartCoverage starts collecting coverage data. Functions read or compiled
// before this call will not be part of the coverage.
func StartCoverage() {
	Provenance = true
	coverage = true
	// Just in case the function is called again the function info is reset.
	coverageFuncs = coverageFuncs[:0]
}

// StopCoverage stops collecting coverage data. Existing data is not changed.
func StopCoverage() {
	coverage = false
}

// WriteCoverage writes a coverage file. The file is a Lisp file containing
// one list. The list starts with a property list of filenames and file
// checksums. Each file and shecksum pair is identified by number in the
// provenance lists that follow. Each provenance element in the list contains
// file-index, first-line, first-column, last-line, last-column, and the
// number of times the function was called.
// (("myfile.lisp" "12345") (0 1 1 2 3 4))
func WriteCoverage(filepath string) {
	var b []byte

	b = append(b, '(', '(')
	// TBD add files and checksum
	b = append(b, ')')
	// TBD add provs
	b = append(b, ')', '\n')

	if err := os.WriteFile(filepath, b, 0666); err != nil {
		panic(fmt.Sprintf("Failed to write %s. %s", filepath, err))
	}
}

// FileChecksum calculates a checksum of a file and returns the checksum as a
// string.
func FileChecksum(filepath string) string {
	f, err := os.Open(filepath)
	if err != nil {
		panic(err)
	}
	defer func() { _ = f.Close() }()

	hasher := sha256.New()
	if _, err = io.Copy(hasher, f); err != nil {
		panic(err)
	}
	return fmt.Sprintf("%x", hasher.Sum(nil))
}
