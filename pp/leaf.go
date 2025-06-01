// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

// Leaf represent the lowest level object type such as number, strings, etc.
type Leaf struct {
	text []byte
	x    int
	nl   bool
}

func (leaf *Leaf) layout(left int) (w int) {
	leaf.x = left

	return len([]rune(string(leaf.text)))
}

func (leaf *Leaf) reorg(edge int) (w int) {
	return len([]rune(string(leaf.text)))
}

func (leaf *Leaf) adjoin(b []byte) []byte {
	return append(b, leaf.text...)
}

func (leaf *Leaf) left() int {
	return leaf.x
}

func (leaf *Leaf) setLeft(left int) {
	leaf.x = left
}

func (leaf *Leaf) width() int {
	return len([]rune(string(leaf.text)))
}

func (leaf *Leaf) right() int {
	return leaf.x + len([]rune(string(leaf.text)))
}

func (leaf *Leaf) newline() bool {
	return leaf.nl
}

func (leaf *Leaf) setNewline(nl bool) {
	leaf.nl = nl
}
