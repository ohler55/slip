// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

type pLeaf []byte

func (leaf pLeaf) layout(maxWidth, tightness int) (width int) {
	return len([]rune(string(leaf)))
}

func (leaf pLeaf) adjoin(b []byte, left, right int) []byte {
	return append(b, leaf...)
}

func (leaf pLeaf) depth() int {
	return 0
}

func (leaf pLeaf) width() int {
	return len([]rune(string(leaf)))
}

func (leaf pLeaf) mode() pMode {
	return pUsual
}
