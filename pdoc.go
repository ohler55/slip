// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

type pDoc struct {
	text string
	x    int
	wide int
}

func (doc *pDoc) layout(left int) int {
	doc.x = left
	doc.wide = len([]rune(doc.text))

	return doc.wide
}

func (doc *pDoc) reorg(edge int) int {
	if edge < doc.right() {
		doc.wide = edge - 3 // 2 quotes and at least one place for a character.
	}
	return doc.wide
}

func (doc *pDoc) adjoin(b []byte) []byte {
	b = append(b, '"')
	b = AppendDoc(b, doc.text, doc.x+1, doc.x+doc.wide, false, 0)

	return append(b, '"')
}

func (doc *pDoc) left() int {
	return doc.x
}

func (doc *pDoc) setLeft(left int) {
	doc.x = left
}

func (doc *pDoc) width() int {
	return doc.wide
}

func (doc *pDoc) right() int {
	return doc.x + doc.wide
}

func (doc *pDoc) newline() bool {
	return true
}

func (doc *pDoc) setNewline(nl bool) {
	// do nothing
}
