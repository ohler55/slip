// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

type pFun struct {
	pList
	name string
}

func newPfun(name string, args slip.List, p *slip.Printer) pNode {
	fun := pFun{
		pList: pList{children: make([]pNode, len(args))},
		name:  name,
	}
	for i, v := range args {
		fun.children[i] = buildPnode(v, p)
	}
	return &fun
}

func (fun *pFun) layout(left, line int) (w int) {
	fun.x = left
	fun.y = line
	x := left + len(fun.name)
	if len(fun.children) == 0 {
		x++
	}
	for _, n := range fun.children {
		x++
		x += n.layout(x, line)
	}
	fun.wide = x - left + 1

	return fun.wide
}

func (fun *pFun) adjoin(b []byte) []byte {
	b = append(b, '(')
	b = append(b, fun.name...)

	// TBD

	return append(b, ')')
}
