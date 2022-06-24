// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"sort"
	"strings"
)

// PackageSymbol is the symbol with a value of "package".
const PackageSymbol = Symbol("package")

var (
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
	Vars      map[string]*VarVal
	Imports   map[string]*Import
	Uses      []*Package
	Users     []*Package

	// Lambdas is a map of all Lambdas defined with either a call to defun or
	// implicitly by referencing a function not yet defined. In that case the
	// caller in the map will have Forms list of length 1 and the single form
	// will have an Object that evaluates to an undefined function panic.
	Lambdas map[string]*Lambda
	Funcs   map[string]*FuncInfo
	Locked  bool
}

// DefPackage creates a new package. Calling Import() and Use() after creation
// is expected.
func DefPackage(name string, nicknames []string, doc string) *Package {
	pkg := Package{
		Name:      strings.ToLower(name),
		Nicknames: nicknames,
		Doc:       doc,
		Vars:      map[string]*VarVal{},
		Imports:   map[string]*Import{},
		Lambdas:   map[string]*Lambda{},
		Funcs:     map[string]*FuncInfo{},
	}
	packages[pkg.Name] = &pkg

	return &pkg
}

// AddPackage adds a package.
func AddPackage(pkg *Package) {
	packages[pkg.Name] = pkg
}

// Use another package
func (obj *Package) Use(pkg *Package) {
	for _, p := range obj.Uses {
		if pkg.Name == p.Name {
			return
		}
	}
	obj.Uses = append(obj.Uses, pkg)
	pkg.Users = append(pkg.Users, obj)
	for name, vv := range pkg.Vars {
		obj.Vars[name] = vv
	}
	for name, fi := range pkg.Funcs {
		obj.Funcs[name] = fi
	}
}

// Import another package variable
func (obj *Package) Import(pkg *Package, varName string) {
	name := strings.ToLower(varName)
	if vv := pkg.Vars[name]; vv != nil {
		obj.Vars[name] = vv
		obj.Imports[name] = &Import{Pkg: pkg, Name: name}
	} else if fi := pkg.Funcs[name]; fi != nil {
		obj.Funcs[name] = fi
		obj.Imports[name] = &Import{Pkg: pkg, Name: name}
	} else {
		panic(fmt.Sprintf("%s is not a variable or function in %s", name, pkg))
	}
}

// Set a variable.
func (obj *Package) Set(name string, value Object) {
	name = strings.ToLower(name)
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if vv, has := obj.Vars[name]; has {
		if vv.Set != nil {
			vv.Set(value)
		} else {
			vv.Val = value
		}
		return
	}
	vv := &VarVal{Val: value, Pkg: obj}
	obj.Vars[name] = vv
	for _, u := range obj.Users {
		if _, has := u.Vars[name]; !has {
			u.Vars[name] = vv
		}
	}
}

// Get a variable.
func (obj *Package) Get(name string) (value Object, has bool) {
	name = strings.ToLower(name)
	var vv *VarVal
	if vv, has = obj.Vars[name]; has {
		if vv.Get != nil {
			value = vv.Get()
		} else {
			value = vv.Val
		}
		return
	}
	return nil, false
}

// Remove a variable.
func (obj *Package) Remove(name string) {
	name = strings.ToLower(name)
	if vv, has := obj.Vars[name]; has {
		if vv.Get != nil {
			panic(fmt.Sprintf("%s can not be removed.", name))
		}
		delete(obj.Vars, name)
		for _, u := range obj.Users {
			if vv, has := u.Vars[name]; has && vv.Pkg == obj {
				delete(u.Vars, name)
			}
		}
	}
}

// Has a variable.
func (obj *Package) Has(name string) (has bool) {
	_, has = obj.Vars[strings.ToLower(name)]
	return
}

// String representation of the Object.
func (obj *Package) String() string {
	return string(obj.Append([]byte{}))
}

// Define a new golang function.
func (obj *Package) Define(creator func(args List) Object, doc *FuncDoc) {
	name := strings.ToLower(doc.Name)
	if _, has := obj.Funcs[name]; has {
		Warning("redefining %s", printer.caseName(name))
	}
	fi := FuncInfo{
		Name:    name,
		Create:  creator,
		Doc:     doc,
		Pkg:     obj,
		BuiltIn: true,
	}
	obj.Funcs[name] = &fi
	for _, pkg := range obj.Users {
		pkg.Funcs[name] = &fi
	}
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
	for k, vv := range obj.Vars {
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
	funcs := make([]string, 0, len(obj.Funcs))
	for name := range obj.Funcs {
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
			return 0 < strings.Compare(si, sj)
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
