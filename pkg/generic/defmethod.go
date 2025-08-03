// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"strings"

	"github.com/ohler55/slip"
)

func defDefmethod() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defmethod{Function: slip.Function{Name: "defmethod", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defmethod",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "list",
					Text: `Can be of two forms. If a list then the method being defined is for Flavors and
must be a list of the flavor name, optional method type, and operation (method name) If not a list then a
method name followed by method qualifiers will result in a CLOS method definition.`,
				},
				{
					Name: "args",
					Type: "lambda-list",
					Text: `The arguments to the method. A specialized lambda-list that allows providing
default values for each variable.`,
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: `Documentation for the method.`,
				},
				{Name: slip.AmpRest},
				{
					Name: "forms",
					Type: "object",
					Text: `The forms that process the method.`,
				},
			},
			Return: "nil",
			Text:   `__defmethod__ defines a method for a flavor or class.`,
			Examples: []string{
				"(defflavor strawberry (size) ()) => #<flavor strawberry>",
				`(defmethod (strawberry :before :size) () (format t "getting size"))  => nil`,
			},
		}, &Pkg)
}

// Defmethod represents the defmethod function.
type Defmethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defmethod) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	switch ta := args[0].(type) {
	case slip.Symbol:
		var aux *Aux
		if fi := slip.FindFunc(string(ta)); fi != nil {
			if aux, _ = fi.Aux.(*Aux); aux == nil {
				slip.PanicProgram("%s already names an ordinary function or macro.", ta)
			}
		}
		result = defGenericMethod(s, ta, args[1:], aux)
	case slip.List:
		if len(ta) == 2 && slip.Symbol("setf") == ta[0] {
			fname := ta.String()
			var aux *Aux
			if fi := slip.FindFunc(fname); fi != nil {
				if aux, _ = fi.Aux.(*Aux); aux == nil {
					slip.PanicProgram("%s already names an ordinary function or macro.", fname)
				}
			}
			result = defGenericMethod(s, slip.Symbol(fname), args[1:], aux)
		} else {
			result = defDirectMethod(s, ta, args[1:])
		}
	default:
		slip.PanicType("method designator for defmethod", ta, "symbol", "list")
	}
	return
}

func defDirectMethod(s *slip.Scope, ml, args slip.List) slip.Object {
	var (
		class  slip.Class
		daemon string
		method string
	)
	switch len(ml) {
	case 0, 1:
		slip.NewPanic("Too few elements in the method for defmethod. Expected 2 or 3 but got %d.", len(ml))
	case 2:
		if sym, ok2 := ml[0].(slip.Symbol); ok2 {
			class = slip.FindClass(string(sym))
		}
	case 3:
		if sym, ok2 := ml[0].(slip.Symbol); ok2 {
			class = slip.FindClass(string(sym))
		}
		if sym, ok2 := ml[1].(slip.Symbol); ok2 {
			daemon = string(sym)
		}
	default:
		slip.NewPanic("Too many elements in the method for defmethod. Expected 2 or 3 but got %d.", len(ml))
	}
	if sym, ok2 := ml[len(ml)-1].(slip.Symbol); ok2 && 1 < len(sym) && sym[0] == ':' {
		method = string(sym)
	} else {
		slip.PanicType("method for defmethod", ml[len(ml)-1], "keyword")
	}
	if class == nil {
		slip.PanicType("class for defmethod", ml[0], "name of class or flavor")
	}
	return DefClassMethod(class, method, daemon, slip.DefLambda(method, s, args, class.VarNames()...))
}

// DefClassMethod defines a direct method on a class.
func DefClassMethod(obj slip.Class, name, daemon string, caller slip.Caller) slip.Object {
	name = strings.ToLower(name)
	hm, ok := obj.(HasMethods)
	if !ok {
		slip.PanicInvalidMethod(obj, slip.Symbol(daemon), slip.Symbol(name),
			"Can not define a direct method, %s on class %s.", name, obj.Name())
	}
	mm := hm.Methods()
	var (
		addMethod bool
		addCombo  bool
	)
	m := mm[name]
	if m == nil {
		m = &slip.Method{Name: name, Doc: &slip.FuncDoc{Name: name, Kind: slip.MethodSymbol}}
		addMethod = true
	}
	// Override existing method documentation if provided by the caller.
	switch tc := caller.(type) {
	case slip.HasFuncDocs:
		if fd := tc.FuncDocs(); fd != nil && 0 < len(fd.Text) {
			fd.Kind = slip.MethodSymbol
			if !addMethod && name != ":init" {
				m.CompareArgs(fd)
			}
			m.Doc = fd
		}
	case HasDocs:
		if text := tc.Docs(); 0 < len(text) {
			m.Doc = &slip.FuncDoc{Name: name, Text: text, Kind: slip.MethodSymbol}
		}
	}
	// If there is a combination for this class it will be the first on the
	// list.
	var c *slip.Combination
	if 0 < len(m.Combinations) && m.Combinations[0].From == obj {
		c = m.Combinations[0]
	} else {
		addCombo = true
		c = &slip.Combination{From: obj}
	}
	daemon = strings.ToLower(daemon)
	switch daemon {
	case ":primary", "":
		c.Primary = caller
	case ":before":
		c.Before = caller
	case ":after":
		c.After = caller
	case ":whopper", ":wrapper":
		c.Wrap = caller
	default:
		slip.PanicInvalidMethod(obj, slip.Symbol(daemon), slip.Symbol(name), "")
	}
	if addMethod {
		mm[name] = m
	}
	if addCombo {
		m.Combinations = append([]*slip.Combination{c}, m.Combinations...)
	}
	if addCombo {
		// If there are supers that inherit from this flavor then insert
		// the new method into the method combinations.
		for _, ac := range slip.CurrentPackage.AllClasses() {
			if ac.Inherits(obj) {
				insertMethod(ac, obj, m, c)
			}
		}
	}
	return m
}

func insertMethod(class, super slip.Class, method *slip.Method, combo *slip.Combination) {
	var mm map[string]*slip.Method
	if hm, ok := class.(HasMethods); ok {
		mm = hm.Methods()
	}
	m := mm[method.Name]
	if m == nil {
		m = &slip.Method{
			Name:         method.Name,
			Doc:          method.Doc,
			Combinations: []*slip.Combination{combo},
		}
		mm[method.Name] = m
		return
	}
	var pos int
	if pos < len(m.Combinations) && m.Combinations[pos].From == class {
		pos++
	}
	for _, f := range class.InheritsList() {
		if len(m.Combinations) <= pos || m.Combinations[pos].From == super {
			break
		}
		if m.Combinations[pos].From == f {
			pos++
		}
	}
	m.Combinations = append(append(m.Combinations[:pos], combo), m.Combinations[pos:]...)
}

// DefCallerMethod defines a method for a caller. The caller should implement
// the slip.HasFuncDocs interface. The key should be the specializers names
// separated by '|'.
func DefCallerMethod(qualifier string, caller slip.Caller, fd *slip.FuncDoc) *slip.Method {
	var aux *Aux
	if fi := slip.FindFunc(fd.Name); fi != nil {
		if aux, _ = fi.Aux.(*Aux); aux == nil {
			slip.PanicProgram("%s already names an ordinary function or macro.", fd.Name)
		}
	}
	if aux == nil {
		gfd := slip.FuncDoc{
			Name:   fd.Name,
			Kind:   slip.GenericFunctionSymbol,
			Args:   make([]*slip.DocArg, len(fd.Args)),
			Return: "object",
		}
		for i, da := range fd.Args {
			gfd.Args[i] = &slip.DocArg{Name: da.Name}
		}
		aux = NewAux(&gfd)
		_ = slip.CurrentPackage.Define(
			func(args slip.List) slip.Object {
				f := genfun{Function: slip.Function{Name: fd.Name, Args: args}, aux: aux}
				f.Self = &f
				return &f
			},
			&gfd,
			aux,
		)
	}
	var key []byte
	for i, da := range fd.Args {
		if da.Name[0] == '&' {
			break
		}
		if 0 < i {
			key = append(key, '|')
		}
		key = append(key, da.Type...)
	}
	return addMethodCaller(aux, fd.Name, qualifier, string(key), caller, fd)
}

func defGenericMethod(s *slip.Scope, fname slip.Symbol, args slip.List, aux *Aux) slip.Object {
	var qual string
	if sym, ok := args[0].(slip.Symbol); ok {
		switch strings.ToLower(string(sym)) {
		case ":before", ":after", ":around":
			qual = string(sym)
		default:
			slip.PanicInvalidMethod(nil, sym, fname, "")
		}
		args = args[1:]
	}
	ll, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("specialize-lambda-list", args[0], "list")
	}
	if aux == nil {
		aux = newGfAux(fname, ll)
	}
	// Set the function docs for the method mostly for a call to describe.
	fd := slip.FuncDoc{
		Name:   string(fname),
		Kind:   slip.MethodSymbol,
		Args:   make([]*slip.DocArg, len(ll)),
		Return: "object",
	}
	for i, v := range ll {
		switch tv := v.(type) {
		case slip.Symbol:
			fd.Args[i] = &slip.DocArg{Name: string(tv)}
		case slip.List:
			if 1 < len(tv) {
				if sym, _ := tv[0].(slip.Symbol); 0 < len(sym) {
					fd.Args[i] = &slip.DocArg{Name: string(sym), Default: tv[1]}
					continue
				}
			}
			slip.PanicType("specialize-lambda-list element", tv, "symbol", "list")
		default:
			slip.PanicType("specialize-lambda-list element", tv, "symbol", "list")
		}
	}
	args = args[1:]
	if 0 < len(args) {
		if ss, ok := args[0].(slip.String); ok {
			fd.Text = string(ss)
			args = args[1:]
		}
	}
	lam := &slip.Lambda{
		Doc:   &fd,
		Forms: args,
	}
	lam.Compile(s)
	key := formMethKey(ll[:aux.reqCnt])

	return addMethodCaller(aux, string(fname), qual, key, lam, &fd)
}

func addMethodCaller(aux *Aux, fname, qualifier, key string, caller slip.Caller, fd *slip.FuncDoc) *slip.Method {
	meth := aux.methods[key]
	if meth == nil {
		meth = &slip.Method{Name: fname}
		aux.methods[key] = meth
	}
	var c *slip.Combination
	if 0 < len(meth.Combinations) {
		c = meth.Combinations[0]
	} else {
		c = &slip.Combination{}
		meth.Combinations = []*slip.Combination{c}
	}
	if meth.Doc == nil {
		meth.Doc = fd
	}
	switch qualifier {
	case "":
		c.Primary = caller
	case ":before":
		c.Before = caller
	case ":after":
		c.After = caller
	case ":around":
		c.Wrap = caller
	}
	if 0 < len(aux.cache) {
		aux.cache = map[string]*slip.Method{}
	}
	return meth
}

func newGfAux(fname slip.Symbol, ll slip.List) *Aux {
	fd := slip.FuncDoc{
		Name:   string(fname),
		Kind:   slip.GenericFunctionSymbol,
		Args:   make([]*slip.DocArg, len(ll)),
		Return: "object",
	}
	for i, v := range ll {
		switch tv := v.(type) {
		case slip.Symbol:
			fd.Args[i] = &slip.DocArg{Name: string(tv)}
		case slip.List:
			if 1 < len(tv) {
				if sym, _ := tv[0].(slip.Symbol); 0 < len(sym) {
					fd.Args[i] = &slip.DocArg{Name: string(sym)}
					continue
				}
			}
			slip.PanicType("specialize-lambda-list element", tv, "symbol", "list")
		default:
			slip.PanicType("specialize-lambda-list element", tv, "symbol", "list")
		}
	}
	aux := NewAux(&fd)
	_ = slip.CurrentPackage.Define(
		func(args slip.List) slip.Object {
			f := genfun{Function: slip.Function{Name: string(fname), Args: args}, aux: aux}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
	return aux
}

func formMethKey(ll slip.List) string {
	var key []byte
	for i, v := range ll {
		if 0 < i {
			key = append(key, '|')
		}
		switch tv := v.(type) {
		case slip.Symbol:
			key = append(key, 't')
		case slip.List:
			if 1 < len(tv) {
				if tv[1] == nil {
					key = append(key, 't')
				} else if sym, ok := tv[1].(slip.Symbol); ok {
					key = append(key, sym...)
				} else if tv[1] == slip.True {
					key = append(key, 't')
				} else {
					slip.PanicType("parameter-specializer-name", tv[1], "symbol")
				}
			} else {
				slip.PanicType("specialize-lambda-list element", tv, "symbol", "list")
			}
		default:
			slip.PanicType("specialize-lambda-list element", tv, "symbol", "list")
		}
	}
	return string(key)
}
