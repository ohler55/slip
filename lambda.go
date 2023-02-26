// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

const (
	reqMode  = 0
	optMode  = 1
	restMode = 2
	keyMode  = 3
)

// Lambda is a Caller for forms/objects. It provides a level of
// indirection so that functions can be defined without regard to the order
// defined.
type Lambda struct {
	Doc     *FuncDoc
	Forms   List // reverse order
	Closure *Scope
}

// Call the the function with the arguments provided.
func (lam *Lambda) Call(s *Scope, args List, depth int) (result Object) {
	ss := NewScope()
	if lam.Closure != nil {
		ss.parent = lam.Closure
	}
	mode := reqMode
	ai := len(args) - 1
	var rest List
	var restSym Symbol
	for i, ad := range lam.Doc.Args {
		if ai < 0 {
			break
		}
	Mode:
		switch mode {
		case reqMode:
			switch strings.ToLower(ad.Name) {
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
			switch strings.ToLower(ad.Name) {
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
					for j := i + 1; j < len(lam.Doc.Args); j++ {
						if string(sym) == lam.Doc.Args[j].Name {
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
	if 0 <= ai {
		panic(&Panic{Message: fmt.Sprintf("Too many arguments to %s. There are %d extra.", lam, ai+1)})
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
	for _, ad := range lam.Doc.Args {
		switch mode {
		case reqMode:
			switch strings.ToLower(ad.Name) {
			case AmpOptional:
				mode = optMode
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			}
		case optMode:
			switch strings.ToLower(ad.Name) {
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			default:
				if !ss.Bound(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), ad.Default)
				}
			}
		case restMode:
			switch strings.ToLower(ad.Name) {
			case AmpKey:
				mode = keyMode
			default:
				if !ss.Bound(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), ad.Default)
				}
			}
		case keyMode:
			if !ss.Bound(Symbol(ad.Name)) {
				ss.Let(Symbol(ad.Name), ad.Default)
			}
		}
	}
	d2 := depth + 1
	for i := len(lam.Forms) - 1; 0 <= i; i-- {
		result = ss.Eval(lam.Forms[i], d2)
		if ss.returnFrom != nil {
			switch {
			case ss.returnFrom.tag == nil:
				if ss.Name == nil {
					return ss.returnFrom.result
				}
			case ss.returnFrom.tag.Equal(ss.Name):
				return ss.returnFrom.result
			default:
				s.returnFrom = ss.returnFrom
			}
			break
		}
	}
	return
}

// DefLambda parses arguments into a Lambda. Arguments should be a
// lambda-list followed by an optional documentation strings and then the
// forms to evaluate when the Lambda is called.
func DefLambda(defName string, s *Scope, args List) (lam *Lambda) {
	pos := len(args) - 1
	var ll List
	switch tl := args[pos].(type) {
	case List:
		ll = tl
	case nil:
		// leave as empty list
	default:
		PanicType(fmt.Sprintf("lambda list of %s", defName), args[pos], "list")
	}
	pos--
	var (
		docStr String
		ok     bool
	)
	switch {
	case pos < 0:
		pos = 0
	case 0 < pos:
		docStr, ok = args[pos].(String)
		if !ok {
			pos++
		}
	default:
		pos++
	}
	lam = &Lambda{
		Doc: &FuncDoc{
			Return: "object",
			Text:   string(docStr),
			Kind:   LambdaSymbol,
		},
		Forms: args[:pos],
	}
	for i := len(ll) - 1; 0 <= i; i-- {
		switch ta := ll[i].(type) {
		case Symbol: // variable name
			lam.Doc.Args = append(lam.Doc.Args, &DocArg{Name: string(ta), Type: "object"})
		case List: // variable name and default value
			if len(ta) != 2 {
				PanicType("lambda list element with default value", ta, "list of two elements")
			}
			var name Symbol
			if name, ok = ta[1].(Symbol); !ok {
				PanicType("lambda list element with default value", ta, "list with a symbol as the first element")
			}
			if ta[0] == nil {
				lam.Doc.Args = append(lam.Doc.Args, &DocArg{Name: string(name), Type: "object"})
			} else {
				lam.Doc.Args = append(lam.Doc.Args,
					&DocArg{Name: string(name), Type: string(ta[0].Hierarchy()[0]), Default: ta[0]})
			}
		default:
			PanicType("lambda list element", ta, "symbol", "list")
		}
	}
	return
}

// String representation of the Object.
func (lam *Lambda) String() string {
	return string(lam.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (lam *Lambda) Append(b []byte) []byte {
	return printer.Append(b, lam, 0)
}

// Equal returns true if this Object and the other are equal in value.
func (lam *Lambda) Equal(other Object) bool {
	return lam == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (lam *Lambda) Hierarchy() []Symbol {
	return []Symbol{LambdaSymbol, TrueSymbol}
}

// Simplify the function.
func (lam *Lambda) Simplify() interface{} {
	simple := make([]any, 0, len(lam.Forms)+2)
	simple = append(simple, "lambda")
	args := make([]any, 0, len(lam.Doc.Args))
	for _, ad := range lam.Doc.Args {
		args = append(args, ad.Name)
	}
	simple = append(simple, args)
	for i := len(lam.Forms) - 1; 0 <= i; i-- {
		simple = append(simple, Simplify(lam.Forms[i]))
	}
	return simple
}

// Eval the object.
func (lam *Lambda) Eval(s *Scope, depth int) (result Object) {
	return lam
}
