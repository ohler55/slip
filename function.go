// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

const (
	// BuiltInSymbol is the symbol with a value of "built-in".
	BuiltInSymbol = Symbol("built-in")
	// FunctionSymbol is the symbol with a value of "function".
	FunctionSymbol = Symbol("function")
	// MacroSymbol is the symbol with a value of "macro".
	MacroSymbol = Symbol("macro")
	// LambdaSymbol is the symbol with a value of "lambda".
	LambdaSymbol = Symbol("lambda")
)

// Function is the base type for most if not all functions.
type Function struct {

	// Name of the function.
	Name string

	// Args are the un-evaluated and un-compiled arguments.
	Args List

	// Self points to the encapsulating object.
	Self Caller

	// SkipEval is a slice of flags indicating which arguments should be
	// evaluated before calling Self.Call(). The last bool is the value used
	// for &rest arguments if present.
	SkipEval []bool

	Pkg *Package
}

// Define a new golang function. If the package is provided the function is
// added to that package otherwise it is added to CurrentPackage (*package*).
func Define(creator func(args List) Object, doc *FuncDoc, pkgs ...*Package) {
	pkg := CurrentPackage
	if 0 < len(pkgs) {
		pkg = pkgs[0]
	}
	pkg.Define(creator, doc)
}

// NewFunc creates a new instance of the named function with the arguments
// provided.
func NewFunc(name string, args List, pkgs ...*Package) Funky {
	fi := MustFindFunc(name, pkgs...)
	f, _ := fi.Create(args).(Funky)
	f.setPkg(fi.Pkg)

	return f
}

// MustFindFunc finds the FuncInfo for a provided name or panics if none exists.
func MustFindFunc(name string, pkgs ...*Package) *FuncInfo {
	if fi := FindFunc(name, pkgs...); fi != nil {
		return fi
	}
	pkg := CurrentPackage
	if 0 < len(pkgs) {
		pkg = pkgs[0]
	}
	panic(NewUndefinedFunction(Symbol(name), "Function %s is not defined in %s.",
		printer.caseName(name), pkg.Name))
}

// FindFunc finds the FuncInfo for a provided name or return nil if none exists.
func FindFunc(name string, pkgs ...*Package) (fi *FuncInfo) {
	// private indicates non-exported okay, referenced with ::
	pkg, vname, private := UnpackName(name)
	if pkg == nil {
		pkg = CurrentPackage
		if 0 < len(pkgs) {
			pkg = pkgs[0]
		}
	}
	if fi = pkg.funcs[vname]; fi == nil {
		vname = strings.ToLower(vname)
		fi = pkg.funcs[vname]
	}
	if fi != nil {
		if private || fi.Export || CurrentPackage == fi.Pkg {
			return fi
		}
		fi = nil
	}
	return
}

// Eval the object.
func (f *Function) Eval(s *Scope, depth int) (result Object) {
	beforeEval(s, f.Name, f.Args, depth)
	defer afterEval(s, f.Name, f.Args, depth, &result)

	args := make(List, len(f.Args))
	d2 := depth + 1
	si := -1
	var update []int
	for i, arg := range f.Args {
		si++
		skip := false
		if 0 < len(f.SkipEval) {
			if len(f.SkipEval) <= si {
				if f.SkipEval[len(f.SkipEval)-1] {
					skip = true
					args[i] = arg
					if _, ok := arg.(List); ok {
						update = append(update, i)
					}
					continue
				}
			} else if f.SkipEval[si] {
				skip = true
				args[i] = arg
				if _, ok := arg.(List); ok {
					update = append(update, i)
				}
				continue
			}
		}
		if list, ok := arg.(List); ok {
			arg = ListToFunc(s, list, depth+1)
			f.Args[i] = arg
		}
		v := s.Eval(arg, d2)
		if vs, ok := v.(Values); ok && !skip {
			v = vs[0]
		}
		args[i] = v
	}
	result = f.Self.Call(s, args, depth)

	// If there are any .Args that need updating to the function version do
	// that by taking the compiled version from the args.
	for _, u := range update {
		a := args[u]
		if _, ok := a.(Funky); ok {
			f.Args[u] = a
		}
	}
	return
}

// SkipArgEval returns true if the argument eval should be skipped.
func (f *Function) SkipArgEval(i int) (skip bool) {
	if 0 < len(f.SkipEval) {
		if i < len(f.SkipEval) {
			skip = f.SkipEval[i]
		} else {
			skip = f.SkipEval[len(f.SkipEval)-1]
		}
	}
	return
}

// Apply evaluates with the need to evaluate the args.
func (f *Function) Apply(s *Scope, args List, depth int) (result Object) {
	beforeEval(s, f.Name, args, depth)
	defer afterEval(s, f.Name, args, depth, &result)

	return f.Self.Call(s, args, depth)
}

// String representation of the Object.
func (f *Function) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Function) Append(b []byte) []byte {
	b = append(b, '(')
	b = printer.Append(b, Symbol(f.Name), 0)
	for _, a := range f.Args {
		b = append(b, ' ')
		b = Append(b, a)
	}
	return append(b, ')')
}

// Simplify the function.
func (f *Function) Simplify() any {
	simple := make([]any, 0, len(f.Args)+1)
	simple = append(simple, f.Name)
	for _, a := range f.Args {
		simple = append(simple, Simplify(a))
	}
	return simple
}

// Equal returns true if this Object and the other are equal in value.
func (f *Function) Equal(other Object) bool {
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (f *Function) Hierarchy() []Symbol {
	for _, skip := range f.SkipEval {
		if skip {
			return []Symbol{MacroSymbol, TrueSymbol}
		}
	}
	return []Symbol{FunctionSymbol, TrueSymbol}
}

// GetArgs returns the function arguments.
func (f *Function) GetArgs() List {
	return f.Args
}

// GetName returns the function name.
func (f *Function) GetName() string {
	return f.Name
}

// ListToFunc converts a list to a function.
func ListToFunc(s *Scope, list List, depth int) Object {
	if len(list) == 0 {
		return nil
	}
	switch ta := list[0].(type) {
	case Symbol:
		return NewFunc(string(ta), list[1:])
	case List:
		if 1 < len(ta) {
			if sym, ok := ta[0].(Symbol); ok {
				if strings.EqualFold("lambda", string(sym)) {
					lambdaDef := ListToFunc(s, ta, depth+1)
					lc := s.Eval(lambdaDef, depth).(*Lambda)
					return &Dynamic{
						Function: Function{
							Self: lc,
							Args: list[1:],
						},
					}
				}
			}
		}
	}
	p := NewError("|%s| is not a function", ObjectString(list[0]))
	p.stack = []string{list.String()}
	panic(p)
}

// CompileArgs for the function.
func (f *Function) CompileArgs() {
	si := -1
	for i := 0; i < len(f.Args); i++ {
		si++
		arg := f.Args[i]
		if 0 < len(f.SkipEval) {
			if len(f.SkipEval) <= si {
				if !f.SkipEval[len(f.SkipEval)-1] {
					if alist, ok := arg.(List); ok {
						f.Args[i] = CompileList(alist)
					}
				}
			} else if !f.SkipEval[si] {
				if alist, ok := arg.(List); ok {
					f.Args[i] = CompileList(alist)
				}
			}
		} else if alist, ok := arg.(List); ok {
			f.Args[i] = CompileList(alist)
		}
	}
}

// Caller returns the function's Caller (Self).
func (f *Function) Caller() Caller {
	return f.Self
}

// CompileList a list into a function or an undefined function.
func CompileList(list List) (f Object) {
	if 0 < len(list) {
		switch ta := list[0].(type) {
		case Symbol:
			name := strings.ToLower(string(ta))
			if fi := CurrentPackage.funcs[name]; fi != nil {
				f = fi.Create(list[1:])
			} else {
				lc := Lambda{
					Doc: &FuncDoc{
						Name: name,
						Args: []*DocArg{},
					},
					Forms: List{Undefined(name)},
				}
				CurrentPackage.lambdas[name] = &lc
				fc := func(args List) Object {
					return &Dynamic{
						Function: Function{
							Name: name,
							Self: &lc,
						},
					}
				}
				CurrentPackage.funcs[name] = &FuncInfo{Create: fc, Pkg: CurrentPackage, Export: true}
				f = fc(list[1:])
			}
			if funk, ok := f.(Funky); ok {
				funk.CompileArgs()
			}
		case List:
			if 1 < len(ta) {
				if sym, ok := ta[0].(Symbol); ok {
					if strings.EqualFold("lambda", string(sym)) {
						s := NewScope()
						lambdaDef := ListToFunc(s, ta, 0)
						lc := s.Eval(lambdaDef, 0).(*Lambda)
						return &Dynamic{
							Function: Function{
								Self: lc,
								Args: list[1:],
							},
						}
					}
				}
			}
		}
	}
	return
}

// DescribeFunction returns the documentation for the function bound to the
// sym argument.
func DescribeFunction(sym Symbol) *FuncDoc {
	name := strings.ToLower(string(sym))
	if fi, has := CurrentPackage.funcs[name]; has {
		return fi.Doc
	}
	return nil
}

// EvalArg converts lists arguments to functions and replaces the
// argument. Then the argument is evaluated and returned. Non-list arguments
// are just evaluated.
func EvalArg(s *Scope, args List, index, depth int) (v Object) {
	if list, ok := args[index].(List); ok {
		args[index] = ListToFunc(s, list, depth+1)
	}
	v = s.Eval(args[index], depth)
	if list, ok := v.(List); ok && len(list) == 0 {
		v = nil
	}
	return
}

// GetArgsKeyValue returns the value for a key in args. Args must be the
// arguments after any required or optional arguments.
func GetArgsKeyValue(args List, key Symbol) (value Object, has bool) {
	for pos := 0; pos < len(args); pos += 2 {
		sym, ok := args[pos].(Symbol)
		if !ok {
			PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			panic(fmt.Sprintf("%s missing an argument", sym))
		}
		if strings.EqualFold(string(key), string(sym)) {
			value = args[pos+1]
			has = true
			break
		}
	}
	return
}

// MustBeString returns a string if the arg is a symbol or string. If not a
// type error is raised with the name argument as the expected field in the
// error.
func MustBeString(arg Object, name string) (str string) {
	switch ta := arg.(type) {
	case String:
		str = string(ta)
	case Symbol:
		if 0 < len(ta) && ta[0] == ':' {
			str = string(ta[1:])
		} else {
			str = string(ta)
		}
	default:
		PanicType(name, arg, "string", "symbol")
	}
	return
}

func (f *Function) setPkg(p *Package) {
	f.Pkg = p
}

// GetPkg returns the package the function was defined in.
func (f *Function) GetPkg() *Package {
	return f.Pkg
}

// PackageFromArg returns a package from an argument or panics.
func PackageFromArg(arg Object) (pkg *Package) {
	switch tv := arg.(type) {
	case Symbol:
		if 0 < len(tv) && tv[0] == ':' {
			pkg = FindPackage(string(tv[1:]))
		} else {
			pkg = FindPackage(string(tv))
		}
	case String:
		pkg = FindPackage(string(tv))
	case *Package:
		pkg = tv
	default:
		PanicType("package", tv, "symbol", "string", "package")
	}
	return
}
