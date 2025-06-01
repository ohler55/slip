// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

type pDefun struct {
	pList
	name  string
	fname string
	args  pNode
}

func pDefunFromList(name string, args List, p *Printer) pNode {
	defun := pDefun{name: name}
	if sym, ok := args[0].(Symbol); ok {
		defun.fname = string(sym)
	}
	if list, ok := args[1].(List); ok {
		defun.args = newPlist(list, p, true)
	}
	args = args[2:]
	defun.children = make([]pNode, len(args))
	for i, v := range args {
		if i == 0 {
			if doc, ok := v.(String); ok {
				defun.children[i] = &pDoc{text: string(doc)}
				continue
			}
		}
		defun.children[i] = buildPnode(v, p)
	}
	return &defun
}

func (defun *pDefun) layout(left int) (w int) {
	defun.x = left
	w = len(defun.name) + len(defun.fname) + 3
	w += defun.args.layout(w)
	last := len(defun.children) - 1
	for i, n := range defun.children {
		n.setNewline(true)
		cw := n.layout(left + 2)
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	defun.wide = w

	return
}

func (defun *pDefun) reorg(edge int) int {
	if edge < defun.right() {
		w := defun.args.reorg(edge)
		if w2 := defun.reorgLines(edge, 2); w < w2 {
			w = w2
		}
		defun.wide = w
	}
	return defun.wide
}

func (defun *pDefun) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, defun.name...)
	b = append(b, ' ')
	b = append(b, defun.fname...)
	b = append(b, ' ')
	b = defun.args.adjoin(b)
	for _, n := range defun.children {
		b = append(b, indent[:n.left()+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}
