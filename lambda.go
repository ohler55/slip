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
	auxMode  = 4
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
	ss := s.NewScope()
	if lam.Closure != nil {
		ss.parents = append(ss.parents, lam.Closure)
		ss.Macro = lam.Closure.Macro
	} else if s.Keep { // flavors instance uses this
		ss.parents = append(ss.parents, s)
	}
	ss.Macro = ss.Macro || lam.Macro
	ss.Block = true
	if 0 < len(lam.Doc.Name) {
		ss.Name = Symbol(lam.Doc.Name)
	}
	mode := reqMode
	ai := 0
	var (
		rest    List
		restSym Symbol
	)
Aux:
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
			case AmpRest, AmpBody:
				mode = restMode
			case AmpKey:
				mode = keyMode
			case AmpAux: // should not be possible to get here without an error later
				break Aux
			case AmpAllowOtherKeys:
				// ignore
			default:
				ss.Let(Symbol(ad.Name), args[ai])
				ai++
			}
		case optMode:
			switch strings.ToLower(ad.Name) {
			case AmpRest, AmpBody:
				mode = restMode
			case AmpKey:
				mode = keyMode
			case AmpAux:
				break Aux
			case AmpAllowOtherKeys:
				// ignore
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
		NewPanic("Too many arguments to %s. There are %d extra.", lam, ai+1)
	}
	if 0 < len(rest) {
		ss.Let(restSym, rest)
	}
	// Next bind any unbound &key vars
	mode = reqMode
	for _, ad := range lam.Doc.Args {
		switch mode {
		case reqMode:
			switch strings.ToLower(ad.Name) {
			case AmpOptional:
				mode = optMode
			case AmpRest, AmpBody:
				mode = restMode
			case AmpKey:
				mode = keyMode
			case AmpAux:
				mode = auxMode
			case AmpAllowOtherKeys:
				// ignore
			}
		case optMode:
			switch strings.ToLower(ad.Name) {
			case AmpRest, AmpBody:
				mode = restMode
			case AmpKey:
				mode = keyMode
			case AmpAux:
				mode = auxMode
			case AmpAllowOtherKeys:
				// ignore
			default:
				if !ss.Bound(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), ad.Default)
				}
			}
		case restMode:
			switch strings.ToLower(ad.Name) {
			case AmpKey:
				mode = keyMode
			case AmpAux:
				mode = auxMode
			case AmpAllowOtherKeys:
				// ignore
			default:
				if !ss.Bound(Symbol(ad.Name)) {
					ss.Let(Symbol(ad.Name), ad.Default)
				}
			}
		case keyMode:
			asym := Symbol(ad.Name)
			if AmpAux == asym {
				mode = auxMode
			} else if !ss.Bound(asym) {
				ss.Let(asym, ad.Default)
			}
		case auxMode:
			val := ad.Default
			if list, ok := val.(List); ok && 1 < len(list) {
				d2 := depth + 1
				val = ss.Eval(ListToFunc(ss, list, d2), d2)
			}
			ss.Let(Symbol(ad.Name), val)
		}
	}
	return lam.BoundCall(ss, depth)
}

// BoundCall the the function with the bindings provided.
func (lam *Lambda) BoundCall(s *Scope, depth int) (result Object) {
	d2 := depth + 1
	for _, form := range lam.Forms {
		result = s.Eval(form, d2)
		if rr, ok := result.(*ReturnResult); ok {
			if rr.Tag == s.Name && s.Name != Symbol("lambda") {
				result = rr.Result
			}
			break
		}
	}
	return
}

// DefLambda parses arguments into a Lambda. Arguments should be a lambda-list
// followed by an optional documentation strings and then the forms to
// evaluate when the Lambda is called. The extraVars is a list of variables
// that will be defined when the lambda is called. This is used for methods
// where the scope will be an instance of a flavor/class.
func DefLambda(defName string, s *Scope, args List, extraVars ...string) (lam *Lambda) {
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
			Name:   defName,
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
	// Compile forms while in the current package instead of waiting until
	// invoked.
	for i, f := range lam.Forms {
	expand:
		switch tf := f.(type) {
		case Symbol:
			if s.has(string(tf)) || lam.Doc.getArg(string(tf)) != nil {
				break
			}
			for _, vn := range extraVars {
				if vn == string(tf) {
					break expand
				}
			}
			vv := CurrentPackage.GetVarVal(string(tf))
			if vv == nil {
				CurrentPackage.mu.Lock()
				if vv = CurrentPackage.vars[string(tf)]; vv == nil {
					vv = newUnboundVar(string(tf))
					CurrentPackage.vars[string(tf)] = vv
				}
				CurrentPackage.mu.Unlock()
			}
			lam.Forms[i] = vv
		case List:
			lam.Forms[i] = CompileList(tf)
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
func (lam *Lambda) Simplify() any {
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

// Docs returns the documentation of the instance.
func (lam *Lambda) Docs() (docs string) {
	if lam.Doc != nil {
		docs = lam.Doc.Text
	}
	return
}

// DefList returns a definition list such as (lambda (x) (1+ x)).
func (lam *Lambda) DefList() List {
	var dl List
	dl = append(dl, Symbol("lambda"))
	dl = append(dl, lam.Doc.DefList())
	if 0 < len(lam.Doc.Text) {
		dl = append(dl, String(lam.Doc.Text))
	}
	dl = append(dl, lam.Forms...)

	return dl
}
