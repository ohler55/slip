// Copyright (c) 2025, Peter Ohler, All rights reserved.

package pp

import (
	"github.com/ohler55/slip"
)

// Lambda represents a lambda block.
type Lambda struct {
	List
	args Node
}

func lambdaFromList(args slip.List, p *slip.Printer) Node {
	lambda := Lambda{
		args: argsFromList(args[0], p),
	}
	args = args[1:]
	lambda.children = make([]Node, len(args))
	for i, v := range args {
		if i == 0 {
			if doc, ok := v.(slip.String); ok {
				lambda.children[i] = &Doc{text: string(doc)}
				continue
			}
		}
		lambda.children[i] = buildNode(v, p)
	}
	return &lambda
}

func (lambda *Lambda) layout(left int) (w int) {
	lambda.x = left
	w = 8 // (lambda and a space
	w += lambda.args.layout(w)
	last := len(lambda.children) - 1
	for i, n := range lambda.children {
		n.setNewline(true)
		cw := n.layout(left + 2)
		if last == i {
			cw++
		}
		if w < cw {
			w = cw
		}
	}
	lambda.wide = w

	return
}

func (lambda *Lambda) reorg(edge int) int {
	if edge < lambda.right() {
		w := lambda.args.reorg(edge) + 8
		if w2 := lambda.reorgLines(edge, 2); w < w2 {
			w = w2
		}
		lambda.wide = w
	}
	return lambda.wide
}

func (lambda *Lambda) adjoin(b []byte) []byte {
	b = append(b, "(lambda "...)
	b = lambda.args.adjoin(b)
	for _, n := range lambda.children {
		b = append(b, indent[:n.left()+1]...)
		b = n.adjoin(b)
	}
	return append(b, ')')
}

func argsFromList(arg slip.Object, p *slip.Printer) Node {
	if args, ok := arg.(slip.List); ok {
		nargs := &List{children: make([]Node, len(args))}
		for i, a := range args {
			switch ta := a.(type) {
			case slip.Symbol:
				nargs.children[i] = &Leaf{text: []byte(ta)}
			case slip.List:
				switch len(ta) {
				case 0:
					// bad definition so ignore
				case 1:
					if sym, ok2 := ta[0].(slip.Symbol); ok2 {
						nargs.children[i] = &Leaf{text: []byte(sym)}
					}
				default:
					if sym, ok2 := ta[0].(slip.Symbol); ok2 {
						nargs.children[i] = &List{
							children: []Node{
								&Leaf{text: []byte(sym)},
								buildNode(ta[1], p),
							},
						}
					}
				}
			}
		}
		return nargs
	}
	return buildNode(nil, p)
}
