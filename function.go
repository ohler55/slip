// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

// FunctionSymbol is the symbol with a value of "function".
const FunctionSymbol = Symbol("function")

var funcCreators = map[string]func(args List) Object{}

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
func Define(creator func(args List) Object, doc *FuncDoc, pkg ...*Package) {
	name := strings.ToLower(doc.Name)
	if _, has := funcCreators[name]; has {
		Warning("redefining %s", printer.caseName(name))
	}
	// TBD use CurrentPackage unless pkg is provided
	funcCreators[name] = creator
	funcDocs[name] = doc
}

// NewFunc creates a new instance of the named function with the arguments
// provided.
func NewFunc(name string, args List) Object {
	name = strings.ToLower(name)
	if create := funcCreators[name]; create != nil {
		return create(args)
	}
	panic(fmt.Sprintf("Function %s is not defined.", printer.caseName(name)))
}

// Eval the object.
func (f *Function) Eval(s *Scope, depth int) (result Object) {
	s.before(s, f.Name, f.Args, depth)
	defer s.after(s, f.Name, f.Args, depth)

	args := make(List, len(f.Args))
	d2 := depth + 1
	si := -1
	var update []int
	for i := len(f.Args) - 1; 0 <= i; i-- {
		si++
		arg := f.Args[i]
		if 0 < len(f.SkipEval) {
			if len(f.SkipEval) <= si {
				if f.SkipEval[len(f.SkipEval)-1] {
					args[i] = arg
					if _, ok := arg.(List); ok {
						update = append(update, i)
					}
					continue
				}
			} else if f.SkipEval[si] {
				args[i] = arg
				if _, ok := arg.(List); ok {
					update = append(update, i)
				}
				continue
			}
		}
		if list, ok := arg.(List); ok {
			arg = ListToFunc(list)
			f.Args[i] = arg
		}
		args[i] = s.Eval(arg, d2)
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
func (f *Function) Apply(s *Scope, args List, depth int) Object {
	s.before(s, f.Name, args, depth)
	defer s.after(s, f.Name, args, depth)

	return f.Self.Call(s, args, depth)
}

// EvalArg converts lists arguments to functions and replaces the
// argument. Then the argument is evaluated and returned. Non-list arguments
// are just evaluated.
func (f *Function) EvalArg(s *Scope, args List, index, depth int) Object {
	if list, ok := args[index].(List); ok {
		args[index] = ListToFunc(list)
	}
	return s.Eval(args[index], depth)
}

// String representation of the Object.
func (f *Function) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Function) Append(b []byte) []byte {
	b = append(b, '(')
	b = append(b, f.Name...)
	for i := len(f.Args) - 1; 0 <= i; i-- {
		b = append(b, ' ')
		b = Append(b, f.Args[i])
	}
	return append(b, ')')
}

// Simplify the function.
func (f *Function) Simplify() interface{} {
	simple := make([]interface{}, 0, len(f.Args)+1)
	simple = append(simple, f.Name)
	for i := len(f.Args) - 1; 0 <= i; i-- {
		simple = append(simple, Simplify(f.Args[i]))
	}
	return simple
}

// Equal returns true if this Object and the other are equal in value.
func (f *Function) Equal(other Object) bool {
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (f *Function) Hierarchy() []Symbol {
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
func ListToFunc(list List) Object {
	if len(list) == 0 {
		return nil
	}
	switch ta := list[len(list)-1].(type) {
	case Symbol:
		return NewFunc(string(ta), list[:len(list)-1])
	case List:
		// TBD maybe lambda
	}
	panic(fmt.Sprintf("%s is not a function", ObjectString(list[0])))
}
