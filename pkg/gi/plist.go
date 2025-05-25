// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

type pList struct {
	children []pNode
	wide     int
}

func (list *pList) layout(maxWidth, tightness int) (width int) {
	mw := maxWidth - 1
	// A list always starts with an open paren.
	width = 1
	switch {
	case tightness == 0:
		for i, n := range list.children {
			if 0 < i {
				width++
			}
			width += n.layout(maxWidth-width, 0)
		}
		width++
		if width <= maxWidth {
			list.wide = width
		}
	case 0 < tightness: // pTight
		ct := tightness - 1
		w := 1
		for i, n := range list.children {
			cw := n.layout(mw, ct)
			if i == 0 { // no choice on the first one, always (quux
				w = cw + 1
			} else {
				if mw < w+cw+1 {
					if width < w {
						width = w
					}
					w = cw + 1
				} else {
					w += cw + 1
				}
			}
		}
		width++ // for closing paren
		if width <= maxWidth {
			list.wide = width
		}
	default: // squeeze
		ct := tightness + 1
		for i, n := range list.children {
			cw := n.layout(mw, ct) + 1
			if i == len(list.children)-1 {
				cw++
			}
			if width < cw {
				width = cw
			}
		}
		if width <= maxWidth {
			list.wide = width
		}
	}
	return
}

func (list *pList) adjoin(b []byte, left, right, tightness int) []byte {
	b = append(b, '(')
	mw := right - left - 1
	switch {
	case tightness == 0:
		for i, n := range list.children {
			if 0 < i {
				b = append(b, ' ')
			}
			b = n.adjoin(b, 0, right, 0)
		}
	case 0 < tightness: // pTight
		ct := tightness - 1
		w := 1
		for i, n := range list.children {
			cw := n.width()
			if i == 0 { // no choice on the first one, always (quux
				b = n.adjoin(b, w, right, ct)
				w = cw + 1
			} else if mw < w+cw+1 {
				b = append(b, indent[:left+2]...)
				b = n.adjoin(b, left+1, right, ct)
				w = cw + 1
			} else {
				if 0 < i {
					b = append(b, ' ')
				}
				b = n.adjoin(b, w, right, ct)
				w += cw + 1
			}
		}
	default:
		ct := tightness + 1
		for i, n := range list.children {
			if 0 < i {
				b = append(b, indent[:left+2]...)
			}
			b = n.adjoin(b, left+1, right, ct)
		}
	}
	return append(b, ')')
}

func (list *pList) depth() int {
	var mx int
	for _, n := range list.children {
		d := n.depth()
		if mx < d {
			mx = d
		}
	}
	return mx + 1
}

func (list *pList) width() int {
	return list.wide
}
