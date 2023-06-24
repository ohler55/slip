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
	Forms   List
	Closure *Scope
	Macro   bool
}

// Call the the function with the arguments provided.
func (lam *Lambda) Call(s *Scope, args List, depth int) (result Object) {
	ss := NewScope()
	if lam.Closure != nil {
		ss.parents = append(ss.parents, lam.Closure)
		ss.Macro = lam.Closure.Macro
	} else if s.Keep { // flavors instance uses this
		ss.parents = append(ss.parents, s)
	}
	ss.Macro = ss.Macro || lam.Macro
	mode := reqMode
	ai := 0
	var rest List
	var restSym Symbol
	for i, ad := range lam.Doc.Args {
		if len(args) <= ai {
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
				ai++
			}
		case optMode:
			switch strings.ToLower(ad.Name) {
			case AmpRest:
				mode = restMode
			case AmpKey:
				mode = keyMode
			default:
				ss.Let(Symbol(ad.Name), args[ai])
				ai++
			}
		case restMode:
			for ai < len(args) {
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
				ai++
				if len(restSym) == 0 {
					restSym = Symbol(ad.Name)
				}
				rest = append(rest, a)
			}
		case keyMode:
			for ai < len(args) {
				a := args[ai]
				ai++
				if sym, ok := a.(Symbol); ok && 0 < len(sym) && sym[0] == ':' {
					sym = sym[1:]
					if len(args) <= ai {
						panic(fmt.Sprintf("Missing value for key :%s.", sym))
					}
					ss.Let(sym, args[ai])
					ai++
					continue
				}
				PanicType("keyword to function", a, "symbol")
			}
		}
	}
	if ai < len(args) {
		panic(&Panic{Message: fmt.Sprintf("Too many arguments to %s. There are %d extra.", lam, ai+1)})
	}
	if 0 < len(rest) {
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
	return lam.BoundCall(ss, depth)
}

// BoundCall the the function with the bindings provided.
func (lam *Lambda) BoundCall(s *Scope, depth int) (result Object) {
	d2 := depth + 1
	for _, form := range lam.Forms {
		result = s.Eval(form, d2)
	}
	return
}

// DefLambda parses arguments into a Lambda. Arguments should be a
// lambda-list followed by an optional documentation strings and then the
// forms to evaluate when the Lambda is called.
func DefLambda(defName string, s *Scope, args List) (lam *Lambda) {
	var ll List
	switch tl := args[0].(type) {
	case List:
		ll = tl
	case nil:
		// leave as empty list
	default:
		PanicType(fmt.Sprintf("lambda list of %s", defName), tl, "list")
	}
	var (
		docStr String
		ok     bool
	)
	args = args[1:]
	if 1 < len(args) {
		if docStr, ok = args[0].(String); ok {
			args = args[1:]
		}
	}
	lam = &Lambda{
		Doc: &FuncDoc{
			Return: "object",
			Text:   string(docStr),
			Kind:   LambdaSymbol,
		},
		Forms: args,
	}
	for _, a := range ll {
		switch ta := a.(type) {
		case Symbol: // variable name
			lam.Doc.Args = append(lam.Doc.Args, &DocArg{Name: string(ta), Type: "object"})
		case List: // variable name and default value
			if len(ta) != 2 {
				PanicType("lambda list element with default value", ta, "list of two elements")
			}
			var name Symbol
			if name, ok = ta[0].(Symbol); !ok {
				PanicType("lambda list element with default value", ta, "list with a symbol as the first element")
			}
			if ta[1] == nil {
				lam.Doc.Args = append(lam.Doc.Args, &DocArg{Name: string(name), Type: "object"})
			} else {
				lam.Doc.Args = append(lam.Doc.Args,
					&DocArg{Name: string(name), Type: string(ta[1].Hierarchy()[0]), Default: ta[1]})
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
