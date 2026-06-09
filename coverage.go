// Copyright (c) 2026, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"sort"
	"sync/atomic"
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
}

// StopCoverage stops collecting coverage data. Existing data is not changed.
func StopCoverage() {
	coverage = false
}

// Coverage returns the current state of coverage.
func Coverage() bool {
	return coverage
}

// ResetCoverage resets all the function use counts to zero. If a hard reset
// then the coverage function list is also zeroed out.
func ResetCoverage(hard bool) {
	if hard {
		coverageFuncs = coverageFuncs[:0]
	} else {
		for _, f := range coverageFuncs {
			if p := f.Provenance(); p != nil {
				p.count = 0
			}
		}
	}
}

// WriteCoverage writes a coverage file with a coverage report generated from
// CoverageReport.
func WriteCoverage(filepath string) {
	if err := os.WriteFile(filepath, CoverageReport(nil), 0666); err != nil {
		panic(fmt.Sprintf("Failed to write %s. %s", filepath, err))
	}
}

// CoverageReport builds a coverage report. The report is a Lisp file
// containing one list. The list starts with a property list of filenames and
// file checksums. Each provenance element in the list contains filepath,
// first-line, first-column, last-line, last-column, and the number of times
// the function was called.  (("myfile.lisp" "12345") ("my-file" 1 1 2 3 4))
func CoverageReport(b []byte) []byte {
	provs := make([]*Prov, 0, len(coverageFuncs))
	// If a function is on the list it should have a Prov but just to be sure
	// use append.
	for _, funky := range coverageFuncs {
		if p := funky.Provenance(); p != nil {
			provs = append(provs, p)
		}
	}
	sort.Slice(provs, func(i, j int) bool {
		pi := provs[i]
		pj := provs[j]
		if pi.Filepath < pj.Filepath {
			return true
		}
		if pi.Filepath == pj.Filepath {
			if pi.FirstLine < pj.FirstLine {
				return true
			}
			if pi.FirstLine == pj.FirstLine {
				return pi.FirstColumn < pj.FirstColumn
			}
		}
		return false
	})

	b = append(b, '(', '(')
	var fp string
	for _, p := range provs {
		if fp != p.Filepath {
			fp = p.Filepath
			b = fmt.Appendf(b, "%q %q\n  ", fp, FileChecksum(fp))
		}
	}
	if bytes.HasSuffix(b, []byte("\n  ")) {
		b = b[:len(b)-3]
	}
	b = append(b, ')')
	for _, funky := range coverageFuncs {
		if p := funky.Provenance(); p != nil {
			b = fmt.Appendf(b, "\n (%q %d %d %d %d %d)",
				p.Filepath, p.FirstLine, p.FirstColumn, p.LastLine, p.LastColumn, atomic.LoadUint32(&p.count))
		}
	}
	return append(b, ')', '\n')
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
