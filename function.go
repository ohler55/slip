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
func NewFunc(name string, args List, pkgs ...*Package) Object {
	return FindFunc(name, pkgs...).Create(args)
}

// FindFunc finds the FuncInfo for a provided name or panics if none exists.
func FindFunc(name string, pkgs ...*Package) *FuncInfo {
	pkg := CurrentPackage
	if i := strings.IndexByte(name, ':'); 0 < i {
		if pkg = FindPackage(name[:i]); pkg == nil {
			panic(fmt.Sprintf("package %s is not defined.", printer.caseName(name[:i])))
		}
		i++
		if i < len(name) && name[i] == ':' {
			i++
		}
		name = name[i:]
	} else if 0 < len(pkgs) {
		pkg = pkgs[0]
	}
	fi := pkg.Funcs[name]
	if fi == nil {
		name = strings.ToLower(name)
		fi = pkg.Funcs[name]
	}
	if fi != nil {
		return fi
	}
	panic(fmt.Sprintf("Function %s is not defined.", printer.caseName(name)))
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

// Apply evaluates with the need to evaluate the args.
func (f *Function) Apply(s *Scope, args List, depth int) (result Object) {
	beforeEval(s, f.Name, args, depth)
	defer afterEval(s, f.Name, args, depth, &result)

	return f.Self.Call(s, args, depth)
}

// EvalArg converts lists arguments to functions and replaces the
// argument. Then the argument is evaluated and returned. Non-list arguments
// are just evaluated.
func (f *Function) EvalArg(s *Scope, args List, index, depth int) (v Object) {
	if list, ok := args[index].(List); ok {
		args[index] = ListToFunc(s, list, depth+1)
	}
	v = s.Eval(args[index], depth)
	if list, ok := v.(List); ok && len(list) == 0 {
		v = nil
	}
	return
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
func (f *Function) Simplify() interface{} {
	simple := make([]interface{}, 0, len(f.Args)+1)
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
	panic(&Panic{
		Message: fmt.Sprintf("|%s| is not a function", ObjectString(list[0])),
		Stack:   []string{list.String()},
	})
}

// CompileArgs for the function.
func (f *Function) CompileArgs() {
	si := -1
	for i := len(f.Args) - 1; 0 <= i; i-- {
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
			if fi := CurrentPackage.Funcs[name]; fi != nil {
				f = fi.Create(list[1:])
			} else {
				lc := Lambda{
					Doc: &FuncDoc{
						Name: name,
						Args: []*DocArg{},
					},
					Forms: List{Undefined(name)},
				}
				CurrentPackage.Lambdas[name] = &lc
				fc := func(args List) Object {
					return &Dynamic{
						Function: Function{
							Name: name,
							Self: &lc,
						},
					}
				}
				CurrentPackage.Funcs[name] = &FuncInfo{Create: fc, Pkg: CurrentPackage}
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
	if fi, has := CurrentPackage.Funcs[name]; has {
		return fi.Doc
	}
	return nil
}
