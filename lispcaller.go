// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "fmt"

const (
	reqMode  = 0
	optMode  = 1
	restMode = 2
	keyMode  = 3
)

// LispCaller is a Caller for forms/objects. It provides a level of
// indirection so that functions can be defined without regard to the order
// defined.
type LispCaller struct {
	Name    string
	Doc     *FuncDoc
	Forms   List // reverse order
	Closure *Scope
}

// Call the the function with the arguments provided.
func (lc *LispCaller) Call(s *Scope, args List, depth int) (result Object) {
	// Copy the before and after from the calling scope but replace the parent
	// to the closure if there is one.
	ss := s.NewScope(Symbol(lc.Name))
	ss.parent = lc.Closure

	mode := reqMode
	ai := len(args) - 1
	var rest List
	var restSym Symbol
	for i, ad := range lc.Doc.Args {
		if ai < 0 {
			break
		}
	Mode:
		switch mode {
		case reqMode:
			switch ad.Name {
			case AmpOptional:
				mode = optMode
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			default:
				ss.Let(Symbol(ad.Name), args[ai])
				ai--
			}
		case optMode:
			switch ad.Name {
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			default:
				ss.Let(Symbol(ad.Name), args[ai])
				ai--
			}
		case restMode:
			for 0 <= ai {
				a := args[ai]
				if sym, ok := a.(Symbol); ok && 0 < len(sym) && sym[0] == ':' {
					sym = sym[1:]
					for j := i + 1; j < len(lc.Doc.Args); j++ {
						if string(sym) == lc.Doc.Args[j].Name {
							mode = keyMode
							break Mode
						}
					}
				}
				ai--
				if len(restSym) == 0 {
					restSym = Symbol(ad.Name)
				}
				rest = append(rest, a)
			}
		case keyMode:
			for 0 <= ai {
				a := args[ai]
				ai--
				if sym, ok := a.(Symbol); ok && 0 < len(sym) && sym[0] == ':' {
					sym = sym[1:]
					if ai < 0 {
						panic(fmt.Sprintf("Missing value for key :%s.", sym))
					}
					ss.Let(sym, args[ai])
					ai--
					continue
				}
				PanicType("keyword to function", a, "symbol")
			}
		}
	}
	if 0 < len(rest) {
		// Reverse rest since it was build with append in the wrong order for
		// a List.
		for i := len(rest)/2 - 1; 0 <= i; i-- {
			rest[i], rest[len(rest)-i-1] = rest[len(rest)-i-1], rest[i]
		}
		ss.Let(restSym, rest)
	}
	mode = reqMode
	for _, ad := range lc.Doc.Args {
		switch mode {
		case reqMode:
			switch ad.Name {
			case AmpOptional:
				mode = optMode
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			}
		case optMode:
			switch ad.Name {
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			default:
				if !ss.Has(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), nil)
				}
			}
		case restMode:
			switch ad.Name {
			case AmpKey:
				mode = keyMode
			default:
				if !ss.Has(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), nil)
				}
			}
		case keyMode:
			if !ss.Has(Symbol(ad.Name)) {
				ss.Let(Symbol(ad.Name), nil)
			}
		}
	}
	d2 := depth + 1
	for i := len(lc.Forms) - 1; 0 <= i; i-- {
		result = ss.Eval(lc.Forms[i], d2)
		if ss.returnFrom != nil {
			switch {
			case ss.returnFrom.tag == nil:
				if ss.name == nil {
					return ss.returnFrom.result
				}
			case ss.returnFrom.tag.Equal(ss.name):
				return ss.returnFrom.result
			default:
				s.returnFrom = ss.returnFrom
			}
			break
		}
	}
	return
}
