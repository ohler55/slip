// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

type pLeaf struct {
	text []byte
	x    int
	y    int
}

func (leaf *pLeaf) layout(left, line int) (w int) {
	leaf.x = left
	leaf.y = line

	return len([]rune(string(leaf.text)))
}

func (leaf *pLeaf) adjoin(b []byte) []byte {
	return append(b, leaf.text...)
}

func (leaf *pLeaf) left() int {
	return leaf.x
}

func (leaf *pLeaf) line() int {
	return leaf.y
}

func (leaf *pLeaf) width() int {
	return len([]rune(string(leaf.text)))
}

func (leaf *pLeaf) right() int {
	return leaf.x + len([]rune(string(leaf.text)))
}
