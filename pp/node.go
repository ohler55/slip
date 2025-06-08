// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

// Node is the interface all pretty print elements must implement.
type Node interface {
	layout(left int) (w int)
	reorg(edge int) (w int)
	adjoin(b []byte) []byte
	width() int
	left() int
	setLeft(left int)
	right() int
	newline() bool
	setNewline(nl bool)
}
