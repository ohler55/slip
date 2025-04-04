// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"reflect"
	"sort"
	"strings"
	"sync"
)

// PackageSymbol is the symbol with a value of "package".
const PackageSymbol = Symbol("package")

var packages = []*Package{
	&CLPkg,
	&UserPkg,
}

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
	path    string
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
	packages = append(packages, &pkg)
	addFeature(pkg.Name)

	return &pkg
}

// DefaultPreSet is the default function for package PreSet.
func DefaultPreSet(p *Package, name string, value Object) (string, Object) {
	return strings.ToLower(name), value
}

// AddPackage adds a package.
func AddPackage(pkg *Package) {
	packages = append(packages, pkg)
	addFeature(pkg.Name)
}

// RemovePackage deletes a package.
func RemovePackage(pkg *Package) {
	if pkg != nil {
		for i, p := range packages {
			if pkg == p {
				packages = append(packages[:i], packages[i+1:]...)
				break
			}
		}
		for _, u := range pkg.Uses {
			pkg.Unuse(u)
		}
		pkg.Name = ""
	}
}

// Initialize the package.
func (obj *Package) Initialize(vars map[string]*VarVal, local ...any) {
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
		vv.name = k
		obj.vars[k] = vv
	}
	if len(obj.path) == 0 && 0 < len(local) {
		obj.path = reflect.TypeOf(local[0]).Elem().PkgPath()
		obj.Locked = true
	}
}

// Use another package
func (obj *Package) Use(pkg *Package) {
	if obj.Locked {
		PanicPackage(obj, "Package %s is locked and can not be modified.", obj)
	}
	if obj != pkg {
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
			if vv.Export {
				obj.vars[name] = vv
			}
		}
		if obj.funcs == nil {
			obj.funcs = map[string]*FuncInfo{}
		}
		for name, fi := range pkg.funcs {
			if fi.Export {
				obj.funcs[name] = fi
			}
		}
	}
}

// Unuse a package
func (obj *Package) Unuse(pkg *Package) {
	if obj.Locked {
		PanicPackage(obj, "Package %s is locked and can not be modified.", obj)
	}
	if obj != pkg {
		obj.mu.Lock()
		pkg.mu.Lock()
		defer func() {
			obj.mu.Unlock()
			pkg.mu.Unlock()
		}()
		for i, p := range obj.Uses {
			if pkg.Name == p.Name {
				obj.Uses = append(obj.Uses[:i], obj.Uses[i+1:]...)
				break
			}
		}
		for i, p := range pkg.Users {
			if obj.Name == p.Name {
				pkg.Users = append(pkg.Users[:i], pkg.Users[i+1:]...)
				break
			}
		}
		// Rebuild to make sure use tree branches are removed as well.
		obj.vars = map[string]*VarVal{}
		obj.funcs = map[string]*FuncInfo{}
		for _, p := range obj.Uses {
			for name, vv := range p.vars {
				obj.vars[name] = vv
			}
			for name, fi := range p.funcs {
				obj.funcs[name] = fi
			}
		}
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
func (obj *Package) Set(name string, value Object, privates ...bool) (vv *VarVal) {
	name, value = obj.PreSet(obj, name, value)
	if _, has := ConstantValues[name]; has {
		PanicPackage(obj, "%s is a constant and thus can't be set", name)
	}
	private := 0 < len(privates) && privates[0]
	if vv = obj.SetIfHas(name, value, private); vv == nil {
		if obj.Locked {
			PanicPackage(obj, "Package %s is locked thus no new variables can be set.", obj.Name)
		}
		vv = &VarVal{Val: value, Pkg: obj, name: name}
		obj.mu.Lock()
		obj.vars[name] = vv
		obj.mu.Unlock()
	}
	callSetHooks(vv.Pkg, name)

	return
}

// SetIfHas sets a variable if the package has that variable.
func (obj *Package) SetIfHas(name string, value Object, private bool) (vv *VarVal) {
	obj.mu.Lock()
	unlock := true
	defer func() {
		if unlock {
			obj.mu.Unlock()
		}
	}()
	if vv = obj.vars[name]; vv != nil {
		if vv.Export || CurrentPackage == obj || private {
			if vv.Set != nil {
				unlock = false
				obj.mu.Unlock()
				vv.Set(value)
			} else {
				vv.Val = value
			}
			for _, u := range obj.Users {
				u.mu.Lock()
				if _, has := u.vars[name]; !has {
					u.vars[name] = vv
				}
				u.mu.Unlock()
			}
		}
	}
	return
}

// Get a variable.
func (obj *Package) Get(name string) (value Object, has bool) {
	obj.mu.Lock()
	defer obj.mu.Unlock()
	if vv := obj.getVarVal(name); vv != nil {
		if vv.Export || CurrentPackage == obj {
			if vv.Get != nil {
				value = vv.Get()
			} else {
				value = vv.Val
			}
			has = true
		}
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
	if vv = obj.vars[name]; vv != nil {
		return vv
	}
	name = strings.ToLower(name)
	if vv = obj.vars[name]; vv != nil {
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
			if vv := u.vars[name]; vv != nil && vv.Pkg == obj {
				delete(u.vars, name)
			}
		}
	}
	obj.mu.Unlock()
	for _, h := range unsetHooks {
		h.fun(obj, name)
	}
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
	if obj.Locked {
		f := creator(nil)
		pp := reflect.TypeOf(f).Elem().PkgPath()
		if pp != obj.path {
			PanicPackage(obj, "Can not define %s in package %s. Package %s is locked.",
				f, obj.Name, obj.Name)
		}
	}
	fi := FuncInfo{
		Name:   name,
		Create: creator,
		Doc:    doc,
		Pkg:    obj,
		Kind:   doc.Kind,
		Export: !doc.NoExport,
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
	obj.funcs[name] = &fi
	for _, pkg := range obj.Users {
		pkg.mu.Lock()
		pkg.funcs[name] = &fi
		pkg.mu.Unlock()
	}
	obj.mu.Unlock()
	for _, h := range defunHooks {
		h.fun(obj, name)
	}
}

// Export a function.
func (obj *Package) Export(name string) {
	name = strings.ToLower(name)
	obj.mu.Lock()
	if obj.funcs != nil {
		if fi := obj.funcs[name]; fi != nil {
			fi.Export = true
			for _, u := range obj.Users {
				u.mu.Lock()
				if xf := u.funcs[name]; xf == nil {
					u.funcs[name] = fi
				}
				u.mu.Unlock()
			}
		}
	}
	if obj.vars != nil {
		if vv := obj.vars[name]; vv != nil {
			vv.Export = true
			for _, u := range obj.Users {
				u.mu.Lock()
				if xv := u.vars[name]; xv == nil {
					u.vars[name] = vv
				}
				u.mu.Unlock()
			}
		} else {
			vv := newUnboundVar(name)
			vv.Export = true
			obj.vars[name] = vv
		}
	}
	obj.mu.Unlock()
}

// Unexport a function.
func (obj *Package) Unexport(name string) {
	name = strings.ToLower(name)
	obj.mu.Lock()
	if obj.funcs != nil {
		if fi := obj.funcs[name]; fi != nil {
			fi.Export = false
			for _, u := range obj.Users {
				u.mu.Lock()
				if xf := u.funcs[name]; xf != nil && obj == xf.Pkg {
					delete(u.funcs, name)
				}
				u.mu.Unlock()
			}
		}
	}
	if obj.vars != nil {
		if vv := obj.vars[name]; vv != nil {
			vv.Export = false
			for _, u := range obj.Users {
				u.mu.Lock()
				if xv := u.vars[name]; xv != nil && obj == xv.Pkg {
					delete(u.vars, name)
				}
				u.mu.Unlock()
			}
		}
	}
	obj.mu.Unlock()
}

// Undefine a function.
func (obj *Package) Undefine(name string) {
	name = strings.ToLower(name)
	obj.mu.Lock()
	if obj.funcs != nil {
		delete(obj.funcs, name)
	}
	obj.mu.Unlock()
	for _, h := range unsetHooks {
		h.fun(obj, name)
	}
}

// Append a buffer with a representation of the Object.
func (obj *Package) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj *Package) Simplify() any {
	nicknames := make([]any, len(obj.Nicknames))
	for i, nn := range obj.Nicknames {
		nicknames[i] = nn
	}
	vars := map[string]any{}
	obj.mu.Lock()
	defer obj.mu.Unlock()
	for k, vv := range obj.vars {
		vars[k] = vv.Simplify()
	}
	imports := map[string]any{}
	for k, imp := range obj.Imports {
		imports[k] = imp.Simplify()
	}
	uses := make([]any, len(obj.Uses))
	for i, p := range obj.Uses {
		uses[i] = p.Name
	}
	funcs := make([]string, 0, len(obj.funcs))
	for name := range obj.funcs {
		funcs = append(funcs, name)
	}
	sort.Strings(funcs)

	return map[string]any{
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
	for _, pkg := range packages {
		names = append(names, String(pkg.Name))
	}
	sort.Slice(names,
		func(i, j int) bool {
			si := string(names[i].(String))
			sj := string(names[j].(String))
			return 0 > strings.Compare(si, sj)
		})
	return
}

// AllPackages returns a list of all packages.
func AllPackages() []*Package {
	pkgs := make([]*Package, len(packages))
	copy(pkgs, packages)
	return pkgs
}

// FindPackage returns the package matching the provided name.
func FindPackage(name string) *Package {
	for _, pkg := range packages {
		if strings.EqualFold(name, pkg.Name) {
			return pkg
		}
	}
	for _, pkg := range packages {
		for _, nn := range pkg.Nicknames {
			if strings.EqualFold(name, nn) {
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
	for i, name := range obj.Nicknames {
		if 0 < i {
			b = append(b, ' ')
		}
		b = Append(b, Symbol(name))
	}
	b = append(b, ')', '\n')

	b = append(b, indentSpaces[:indent]...)
	b = append(b, "Documentation:\n"...)
	b = AppendDoc(b, obj.Doc, indent+2, right, ansi)
	b = append(b, '\n')
	if obj.Locked {
		b = append(b, "Locked\n"...)
	}
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
			if cols <= col {
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
	if xlam := obj.lambdas[name]; xlam != nil {
		xlam.Doc = lam.Doc
		xlam.Forms = lam.Forms
		xlam.Closure = lam.Closure
		xlam.Macro = lam.Macro
	} else {
		obj.lambdas[name] = lam
	}
	if fi := obj.funcs[name]; fi != nil {
		fi.Doc = lam.Doc
		fi.Create = fc
		fi.Pkg = obj
		fi.Kind = kind
	} else {
		fi := FuncInfo{
			Name:   name,
			Doc:    lam.Doc,
			Create: fc,
			Pkg:    obj,
			Kind:   kind,
		}
		obj.funcs[name] = &fi
		if vv := obj.vars[name]; vv != nil && Unbound == vv.Val && vv.Export {
			fi.Export = true
			delete(obj.vars, name)
		}
	}
	obj.mu.Unlock()
	if 0 < len(name) && !strings.EqualFold(name, "lambda") {
		for _, h := range defunHooks {
			h.fun(obj, name)
		}
	}
	return
}
