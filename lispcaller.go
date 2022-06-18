// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"io"
	"strconv"
	"strings"
	"unsafe"
)

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
	Name    string // TBD is this needed?
	Doc     *FuncDoc
	Forms   List // reverse order
	Closure *Scope
}

// Call the the function with the arguments provided.
func (lc *LispCaller) Call(s *Scope, args List, depth int) (result Object) {
	ss := NewScope()
	if 0 < len(lc.Name) {
		ss.name = Symbol(lc.Name)
	}
	if lc.Closure != nil {
		ss.parent = lc.Closure
	}
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
	if 0 <= ai {
		// TBD change lc.Name to lc.String()
		panic(&Panic{Message: fmt.Sprintf("Too many arguments to %s. There are %d extra.", lc.Name, ai+1)})
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
				if !ss.Has(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), ad.Default)
				}
			}
		case restMode:
			switch strings.ToLower(ad.Name) {
			case AmpKey:
				mode = keyMode
			default:
				if !ss.Has(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), ad.Default)
				}
			}
		case keyMode:
			if !ss.Has(Symbol(ad.Name)) {
				ss.Let(Symbol(ad.Name), ad.Default)
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

// DefLispCaller parses arguments into a LispCaller. Arguments should be a
// lambda-list followed by an optional documentation strings and then the
// forms to evaluate when the LispCaller is called.
func DefLispCaller(defName, funcName string, s *Scope, args List) (lc *LispCaller) {
	funcName = strings.ToLower(funcName)
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
	if pos < 0 {
		pos = 0
	} else {
		docStr, ok = args[pos].(String)
		if !ok {
			pos++
		}
	}
	lc = &LispCaller{
		Name: funcName,
		Doc: &FuncDoc{
			Name:   funcName,
			Return: "object",
			Text:   string(docStr),
		},
		Forms: args[:pos],
	}
	for i := len(ll) - 1; 0 <= i; i-- {
		switch ta := ll[i].(type) {
		case Symbol: // variable name
			lc.Doc.Args = append(lc.Doc.Args, &DocArg{Name: string(ta), Type: "object"})
		case List: // variable name and default value
			if len(ta) != 2 {
				PanicType("lambda list element with default value", ta, "list of two elements")
			}
			var name Symbol
			if name, ok = ta[1].(Symbol); !ok {
				PanicType("lambda list element with default value", ta, "list with a symbol as the first element")
			}
			if ta[0] == nil {
				lc.Doc.Args = append(lc.Doc.Args, &DocArg{Name: string(name), Type: "object"})
			} else {
				lc.Doc.Args = append(lc.Doc.Args,
					&DocArg{Name: string(name), Type: string(ta[0].Hierarchy()[0]), Default: ta[0]})
			}
		default:
			PanicType("lambda list element", ta, "symbol", "list")
		}
	}
	if fi := CurrentPackage.Funcs[funcName]; fi != nil {
		if fi.Pkg.Locked {
			panic(fmt.Sprintf("Redefining %s::%s in %s. Package %s is locked.",
				CurrentPackage.Name, funcName, defName, CurrentPackage.Name))
		}
		var w io.Writer
		if w, ok = ErrorOutput.(io.Writer); ok {
			_, _ = fmt.Fprintf(w, "WARNING: redefining %s::%s in %s\n", CurrentPackage.Name, funcName, defName)
		}
	}
	return
}

// String representation of the Object.
func (lc *LispCaller) String() string {
	return string(lc.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (lc *LispCaller) Append(b []byte) []byte {
	b = append(b, "#<"...)
	b = printer.Append(b, Symbol("function"), 0)
	b = append(b, " ("...)
	b = printer.Append(b, Symbol("lambda"), 0)
	b = append(b, " ("...)
	for i, ad := range lc.Doc.Args {
		if 0 < i {
			b = append(b, ' ')
		}
		b = append(b, ad.Name...)
	}
	b = append(b, ')')

	// TBD if *print-full* or *print-forms* then print forms else number

	b = append(b, ") {"...)
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(lc))), 16)
	b = append(b, '}')

	return append(b, '>')
}

// Equal returns true if this Object and the other are equal in value.
func (lc *LispCaller) Equal(other Object) bool {
	return lc == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (lc *LispCaller) Hierarchy() []Symbol {
	return []Symbol{FunctionSymbol, TrueSymbol}
}

// Simplify the function.
func (lc *LispCaller) Simplify() interface{} {
	simple := make([]any, 0, len(lc.Forms)+2)
	simple = append(simple, "lambda")
	args := make([]any, 0, len(lc.Doc.Args))
	for _, ad := range lc.Doc.Args {
		args = append(args, ad.Name)
	}
	simple = append(simple, args)
	for i := len(lc.Forms) - 1; 0 <= i; i-- {
		simple = append(simple, Simplify(lc.Forms[i]))
	}
	return simple
}

// Eval the object.
func (lc *LispCaller) Eval(s *Scope, depth int) (result Object) {
	return lc
}
