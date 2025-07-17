// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Doc holds a documentation string.
type Doc struct {
	text string
	x    int
	wide int
	nl   bool
}

func (doc *Doc) layout(left int) int {
	doc.x = left
	doc.wide = len([]rune(doc.text)) + 2

	return doc.wide
}

func (doc *Doc) reorg(edge int) int {
	if edge < doc.right() {
		doc.wide = edge - doc.x
	}
	return doc.wide
}

func (doc *Doc) adjoin(b []byte) []byte {
	b = append(b, '"')
	b = slip.AppendDoc(b, doc.text, doc.x+1, doc.x+doc.wide, false, 0)

	return append(b, '"')
}

func (doc *Doc) left() int {
	return doc.x
}

func (doc *Doc) setLeft(left int) {
	doc.x = left
}

func (doc *Doc) width() int {
	return doc.wide
}

func (doc *Doc) right() int {
	return doc.x + doc.wide
}

func (doc *Doc) newline() bool {
	return doc.nl
}

func (doc *Doc) setNewline(nl bool) {
	doc.nl = nl
}
