// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"sort"
	"strings"
	"sync"
)

// PackageSymbol is the symbol with a value of "package".
const PackageSymbol = Symbol("package")

var (
	// SetHook is called after setting a variable or after a defvar or
	// defparmeter is called.
	SetHook = func(p *Package, key string) {}

	// UnsetHook is called when a variable is unset or removed.
	UnsetHook = func(p *Package, key string) {}

	// DefunHook is called after a function is added.
	DefunHook = func(p *Package, key string) {}

	packages = map[string]*Package{
		CLPkg.Name:   &CLPkg,
		UserPkg.Name: &UserPkg,
	}
)

func init() {
	DefConstant(PackageSymbol, PackageSymbol, `A _package_ represents a namespace.`)

	CurrentPackage = &UserPkg
}

// Package represents a LISP package.
type Package struct {
	Name      string
	Nicknames []string
	Doc       string
	Imports   map[string]*Import
	Uses      []*Package
	Users     []*Package

	// Lambdas is a map of all Lambdas defined with either a call to defun or
	// implicitly by referencing a function not yet defined. In that case the
	// caller in the map will have Forms list of length 1 and the single form
	// will have an Object that evaluates to an undefined function panic.
	lambdas map[string]*Lambda
	funcs   map[string]*FuncInfo
	vars    map[string]*VarVal
	PreSet  func(p *Package, name string, value Object) (string, Object)
	mu      sync.Mutex
	Locked  bool
}

// DefPackage creates a new package. Calling Import() and Use() after creation
// is expected.
func DefPackage(name string, nicknames []string, doc string) *Package {
	pkg := Package{
		Name:      strings.ToLower(name),
		Nicknames: nicknames,
		Doc:       doc,
		vars:      map[string]*VarVal{},
		Imports:   map[string]*Import{},
		lambdas:   map[string]*Lambda{},
		funcs:     map[string]*FuncInfo{},
		PreSet:    DefaultPreSet,
	}
	packages[pkg.Name] = &pkg
	addFeature(pkg.Name)

	return &pkg
}

// DefaultPreSet is the default function for package PreSet.
func DefaultPreSet(p *Package, name string, value Object) (string, Object) {
	return strings.ToLower(name), value
}

// AddPackage adds a package.
func AddPackage(pkg *Package) {
	packages[pkg.Name] = pkg
	addFeature(pkg.Name)
}

// Initialize the package.
func (obj *Package) Initialize(vars map[string]*VarVal) {
	if obj.funcs == nil {
		obj.funcs = map[string]*FuncInfo{}
	}
	if obj.lambdas == nil {
		obj.lambdas = map[string]*Lambda{}
	}
	if obj.vars == nil {
		obj.vars = map[string]*VarVal{}
	}
	for k, vv := range vars {
		vv.Pkg = obj
		obj.vars[k] = vv
	}
}

// Use another package
func (obj *Package) Use(pkg *Package) {
	obj.mu.Lock()
	pkg.mu.Lock()
	defer func() {
		obj.mu.Unlock()
		pkg.mu.Unlock()
	}()
	for _, p := range obj.Uses {
		if pkg.Name == p.Name {
			return
		}
	}
	obj.Uses = append(obj.Uses, pkg)
	pkg.Users = append(pkg.Users, obj)
	if obj.vars == nil {
		obj.vars = map[string]*VarVal{}
	}
	for name, vv := range pkg.vars {
		obj.vars[name] = vv
	}
	for name, fi := range pkg.funcs {
		obj.funcs[name] = fi
	}
}

// Import another package variable
func (obj *Package) Import(pkg *Package, varName string) {
	obj.mu.Lock()
	pkg.mu.Lock()
	defer func() {
		obj.mu.Unlock()
		pkg.mu.Unlock()
	}()
	name := strings.ToLower(varName)
	if vv := pkg.vars[name]; vv != nil {
		obj.vars[name] = vv
		obj.Imports[name] = &Import{Pkg: pkg, Name: name}
	} else if fi := pkg.funcs[name]; fi != nil {
		obj.funcs[name] = fi
		obj.Imports[name] = &Import{Pkg: pkg, Name: name}
	} else {
		PanicPackage(obj, "%s is not a variable or function in %s", name, pkg)
	}
}

// Set a variable.
func (obj *Package) Set(name string, value Object) *VarVal {
	name, value = obj.PreSet(obj, name, value)
	if _, has := ConstantValues[name]; has {
		PanicPackage(obj, "%s is a constant and thus can't be set", name)
	}
	obj.mu.Lock()
	if vv, has := obj.vars[name]; has {
		obj.mu.Unlock()
		if vv.Set != nil {
			vv.Set(value)
		} else {
			vv.Val = value
		}
		SetHook(vv.Pkg, name)
		return vv
	}
	if obj.Locked {
		obj.mu.Unlock()
		PanicPackage(obj, "Package %s is locked thus no new variables can be set.", obj.Name)
	}
	vv := &VarVal{Val: value, Pkg: obj}
	obj.vars[name] = vv
	for _, u := range obj.Users {
		if _, has := u.vars[name]; !has {
			u.vars[name] = vv
		}
	}
	obj.mu.Unlock()
	SetHook(obj, name)

	return vv
}

// Get a variable.
func (obj *Package) Get(name string) (value Object, has bool) {
	obj.mu.Lock()
	defer obj.mu.Unlock()
	if vv := obj.getVarVal(name); vv != nil {
		if vv.Get != nil {
			value = vv.Get()
		} else {
			value = vv.Val
		}
		has = true
	}
	return
}

// GetVarVal a variable.
func (obj *Package) GetVarVal(name string) (vv *VarVal) {
	obj.mu.Lock()
	vv = obj.getVarVal(name)
	obj.mu.Unlock()
	return
}

func (obj *Package) getVarVal(name string) (vv *VarVal) {
	if vv, _ = obj.vars[name]; vv != nil {
		return vv
	}
	name = strings.ToLower(name)
	if vv, _ = obj.vars[name]; vv != nil {
		return vv
	}
	return nil
}

// JustGet a variable.
func (obj *Package) JustGet(name string) (value Object) {
	value, _ = obj.Get(name)
	return
}

// Remove a variable.
func (obj *Package) Remove(name string) (removed bool) {
	if obj.Locked {
		PanicPackage(obj, "Package %s is locked.", obj.Name)
	}
	name = strings.ToLower(name)
	obj.mu.Lock()
	if _, has := obj.vars[name]; has {
		delete(obj.vars, name)
		removed = true
		for _, u := range obj.Users {
			if vv, _ := u.vars[name]; vv != nil && vv.Pkg == obj {
				delete(u.vars, name)
			}
		}
	}
	obj.mu.Unlock()
	UnsetHook(obj, name)
	return
}

// Has a variable.
func (obj *Package) Has(name string) (has bool) {
	obj.mu.Lock()
	_, has = obj.vars[strings.ToLower(name)]
	obj.mu.Unlock()
	return
}

// String representation of the Object.
func (obj *Package) String() string {
	return string(obj.Append([]byte{}))
}

// Define a new golang function.
func (obj *Package) Define(creator func(args List) Object, doc *FuncDoc) {
	name := strings.ToLower(doc.Name)
	fi := FuncInfo{
		Name:   name,
		Create: creator,
		Doc:    doc,
		Pkg:    obj,
		Kind:   doc.Kind,
	}
	if len(fi.Kind) == 0 {
		fi.Kind = BuiltInSymbol
	}
	obj.mu.Lock()
	if obj.funcs == nil {
		obj.funcs = map[string]*FuncInfo{}
	}
	if _, has := obj.funcs[name]; has {
		Warn("redefining %s", printer.caseName(name))
	}
	if obj.funcs == nil {
		fmt.Printf("*** pkg: %s define %s\n", obj.Name, name)
	}
	obj.funcs[name] = &fi
	for _, pkg := range obj.Users {
		pkg.mu.Lock()
		pkg.funcs[name] = &fi
		pkg.mu.Unlock()
	}
	obj.mu.Unlock()
	DefunHook(obj, name)
}

// Undefine a function.
func (obj *Package) Undefine(name string) {
	name = strings.ToLower(name)
	obj.mu.Lock()
	if obj.funcs != nil {
		delete(obj.funcs, name)
	}
	obj.mu.Unlock()
	// TBD define hook
	// UndefineHook(obj, name)
}

// Append a buffer with a representation of the Object.
func (obj *Package) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj *Package) Simplify() interface{} {
	nicknames := make([]interface{}, len(obj.Nicknames))
	for i, nn := range obj.Nicknames {
		nicknames[i] = nn
	}
	vars := map[string]interface{}{}
	obj.mu.Lock()
	defer obj.mu.Unlock()
	for k, vv := range obj.vars {
		vars[k] = vv.Simplify()
	}
	imports := map[string]interface{}{}
	for k, imp := range obj.Imports {
		imports[k] = imp.Simplify()
	}
	uses := make([]interface{}, len(obj.Uses))
	for i, p := range obj.Uses {
		uses[i] = p.Name
	}
	funcs := make([]string, 0, len(obj.funcs))
	for name := range obj.funcs {
		funcs = append(funcs, name)
		// TBD maybe show package defined in?
	}
	sort.Strings(funcs)

	return map[string]interface{}{
		"name":      obj.Name,
		"nicknames": nicknames,
		"doc":       obj.Doc,
		"vars":      vars,
		"imports":   imports,
		"uses":      uses,
		"functions": funcs,
	}
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Package) Equal(other Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Package) Hierarchy() []Symbol {
	return []Symbol{PackageSymbol, TrueSymbol}
}

// Eval returns self.
func (obj *Package) Eval(s *Scope, depth int) Object {
	return obj
}

// PackageNames returns a sorted list of package names.
func PackageNames() (names List) {
	for name := range packages {
		names = append(names, String(name))
	}
	sort.Slice(names,
		func(i, j int) bool {
			si := string(names[i].(String))
			sj := string(names[j].(String))
			return 0 > strings.Compare(si, sj)
		})
	return
}

// FindPackage returns the package matching the provided name.
func FindPackage(name string) *Package {
	// Try the direct way first.
	if pkg := packages[name]; pkg != nil {
		return pkg
	}
	name = strings.ToLower(name)
	if pkg := packages[name]; pkg != nil {
		return pkg
	}
	for _, pkg := range packages {
		for _, nn := range pkg.Nicknames {
			if nn == name {
				return pkg
			}
		}
	}
	return nil
}

// Describe the instance in detail.
func (obj *Package) Describe(b []byte, indent, right int, ansi bool) []byte {
	obj.mu.Lock()
	defer obj.mu.Unlock()
	b = append(b, indentSpaces[:indent]...)
	b = append(b, "Name: "...)
	b = append(b, obj.Name...)
	b = append(b, '\n')

	b = append(b, indentSpaces[:indent]...)
	b = append(b, "Nicknames: ("...)
	for _, name := range obj.Nicknames {
		b = Append(b, Symbol(name))
		b = append(b, ' ')
	}
	b[len(b)-1] = ')'
	b = append(b, '\n')

	b = append(b, indentSpaces[:indent]...)
	b = append(b, "Documentation:\n"...)
	b = AppendDoc(b, obj.Doc, indent+2, right, ansi)
	b = append(b, '\n')

	if 0 < len(obj.Imports) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Imports:\n"...)
		for _, imp := range obj.Imports {
			b = append(b, indentSpaces[:indent+2]...)
			b = append(b, imp.Name...)
			b = append(b, " from "...)
			b = append(b, imp.Pkg.Name...)
			b = append(b, '\n')
		}
	}
	if 0 < len(obj.Uses) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Uses:\n"...)
		for _, p := range obj.Uses {
			b = append(b, indentSpaces[:indent+2]...)
			b = append(b, p.Name...)
			b = append(b, '\n')
		}
	}
	if 0 < len(obj.Users) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Used By:\n"...)
		for _, p := range obj.Users {
			b = append(b, indentSpaces[:indent+2]...)
			b = append(b, p.Name...)
			b = append(b, '\n')
		}
	}
	var keys []string
	for k, vv := range obj.vars {
		if obj != vv.Pkg {
			continue
		}
		keys = append(keys, k)
	}
	if 0 < len(keys) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Variables:\n"...)
		sort.Strings(keys)
		for _, k := range keys {
			vv := obj.vars[k]
			b = append(b, indentSpaces[:indent+2]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = Append(b, vv.Value())
			b = append(b, '\n')
		}
	}
	var names []string
	max := 0
	for k, fi := range obj.funcs {
		if obj != fi.Pkg {
			continue
		}
		names = append(names, k)
		if max < len(k) {
			max = len(k)
		}
	}
	if 0 < len(names) {
		b = append(b, indentSpaces[:indent]...)
		b = append(b, "Functions:\n"...)
		sort.Strings(names)
		cols := (right - indent - 2) / (max + 1)
		col := 0
		for _, k := range names {
			if col == 0 {
				b = append(b, indentSpaces[:indent+2]...)
			}
			col++
			b = append(b, k...)
			if cols < col {
				b = append(b, '\n')
				col = 0
			} else {
				b = append(b, indentSpaces[:max-len(k)+1]...)
			}
		}
	}
	return append(b, '\n')
}

// EachFuncName call the cb for each function name in the package.
func (obj *Package) EachFuncName(cb func(name string)) {
	obj.mu.Lock()
	for name := range obj.funcs {
		cb(name)
	}
	obj.mu.Unlock()
}

// EachFuncInfo call the cb for each function in the package.
func (obj *Package) EachFuncInfo(cb func(fi *FuncInfo)) {
	obj.mu.Lock()
	for _, fi := range obj.funcs {
		cb(fi)
	}
	obj.mu.Unlock()
}

// EachVarName call the cb for each function name in the package.
func (obj *Package) EachVarName(cb func(name string)) {
	obj.mu.Lock()
	for name := range obj.vars {
		cb(name)
	}
	obj.mu.Unlock()
}

// EachVarVal call the cb for each var in the package.
func (obj *Package) EachVarVal(cb func(name string, vv *VarVal)) {
	obj.mu.Lock()
	for name, vv := range obj.vars {
		cb(name, vv)
	}
	obj.mu.Unlock()
}

// GetFunc returns the FuncInfo associated with the name which must be
// lowercase.
func (obj *Package) GetFunc(name string) (fi *FuncInfo) {
	obj.mu.Lock()
	fi = obj.funcs[name]
	obj.mu.Unlock()
	return
}

// DefLambda registers a named lambda function. This is called by defun.
func (obj *Package) DefLambda(name string, lam *Lambda, fc func(args List) Object, kind Symbol) (fi *FuncInfo) {
	obj.mu.Lock()
	obj.lambdas[name] = lam
	obj.funcs[name] = &FuncInfo{
		Name:   name,
		Doc:    lam.Doc,
		Create: fc,
		Pkg:    obj,
		Kind:   kind,
	}
	obj.mu.Unlock()
	return
}
