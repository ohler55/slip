// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "fmt"

const (
	reqMode  = 0
	optMode  = 1
	restMode = 2
	keyMode  = 3
)

// Dynamic represents the a function defined by a call to defun or lambda.
type Dynamic struct {
	Function
	Doc   FuncDoc
	Forms []Object // normal order
}

// Call the the function with the arguments provided.
func (f *Dynamic) Call(s *Scope, args List, depth int) (result Object) {
	ss := s.NewScope(Symbol(f.Name))
	mode := reqMode
	ai := 0
	var rest List
	for i, ad := range f.Doc.Args {
		if len(args) <= ai {
			break
		}
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
				ai++
			}
		case optMode:
			switch ad.Name {
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			default:
				ss.Let(Symbol(ad.Name), args[ai])
				ai++
			}
		case restMode:
			a := args[ai]
			ai++
			if sym, ok := a.(Symbol); ok {
				var key string
				for j := i + 1; j < len(f.Doc.Args); j++ {
					if string(sym) == f.Doc.Args[j].Name {
						key = string(sym)
						break
					}
				}
				if 0 < len(key) {
					if key[0] == ':' {
						key = key[1:]
					}
				}
				mode = keyMode
				if len(args) <= ai {
					panic(fmt.Sprintf("Missing value for key %s.", sym))
				}
				ss.Let(Symbol(key), args[ai])
				ai++
				break
			}
			ai++
			rest = append(rest, a)
		case keyMode:
			a := args[ai]
			ai++

			if sym, ok := a.(Symbol); ok {
				key := string(sym)
				if 0 < len(key) {
					if key[0] == ':' {
						key = key[1:]
					}
				}
				if len(args) <= ai {
					panic(fmt.Sprintf("Missing value for key %s.", sym))
				}
				ss.Let(Symbol(key), args[ai])
				ai++
				break
			}
			PanicType("keyword to function", a, "symbol")
		}
	}
	if 0 < len(rest) {
		ss.Let(Symbol("rest"), rest)
	}
	d2 := depth + 1
	for _, form := range f.Forms {
		result = ss.Eval(form, d2)
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

// String representation of the Object.
func (f *Dynamic) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Dynamic) Append(b []byte) []byte {
	b = append(b, '(')

	if 0 < len(f.Name) {
		b = append(b, f.Name...)
	} else {
		// TBD form list and print that
		// (lambda (x) (+ 1 2)) 7)
	}
	for i := len(f.Args) - 1; 0 <= i; i-- {
		b = append(b, ' ')
		b = Append(b, f.Args[i])
	}
	return append(b, ')')
}
