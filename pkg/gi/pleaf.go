// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

type pLeaf struct {
	text []byte
	x    int
	nl   bool
}

func (leaf *pLeaf) layout(left int) (w int) {
	leaf.x = left

	return len([]rune(string(leaf.text)))
}

func (leaf *pLeaf) reorg(edge int) (w int) {
	return len([]rune(string(leaf.text)))
}

func (leaf *pLeaf) adjoin(b []byte) []byte {
	return append(b, leaf.text...)
}

func (leaf *pLeaf) left() int {
	return leaf.x
}

func (leaf *pLeaf) setLeft(left int) {
	leaf.x = left
}

func (leaf *pLeaf) width() int {
	return len([]rune(string(leaf.text)))
}

func (leaf *pLeaf) right() int {
	return leaf.x + len([]rune(string(leaf.text)))
}

func (leaf *pLeaf) newline() bool {
	return leaf.nl
}

func (leaf *pLeaf) setNewline(nl bool) {
	leaf.nl = nl
}
