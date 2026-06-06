// Copyright (c) 2026, Peter Ohler, All rights reserved.

package slip

var (
	Coverage      = false
	coverageFuncs []Funky
)

// StartCoverage starts collecting coverage data. Functions read or compiled
// before this call will not be part of the coverage.
func StartCoverage() {
	Provenance = true
	Coverage = true
	// Just in case the function is called again the function info is reset.
	coverageFuncs = coverageFuncs[:0]
}

// StopCoverage stops collecting coverage data. Existing data is not changed.
func StopCoverage() {
	Coverage = false
}

// WriteCoverage writes a coverage file.
func WriteCoverage(filepath string) {
	// TBD
	// start with a list of filepaths with checksum
	// assign numbers to each filepath and use in rest of table
	// make it a lisp file
	//  ((file0 checksum0 file1 checksum1)
	//   (<file-index> <first-line> <first-column> <last-line> <last-column> <count>)

}
