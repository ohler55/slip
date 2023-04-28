// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

type dirty struct {
	top   int
	cnt   int      // number of lines below form to delete on next key stroke unless a tab
	lines [][]byte // set if scrolling
	box   bool
}
