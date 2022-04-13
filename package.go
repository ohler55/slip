// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

// PackageSymbol is the symbol with a value of "package".
const PackageSymbol = Symbol("package")

var (
	clPkg = Package{
		Name:      "COMMON-LISP",
		Nicknames: []string{"CL"},
		Doc:       "Home of symbols defined by the ANSI language spcification.",
		Vars:      map[string]Object{ // TBD
		},
		varGets: map[string]func() Object{
			"*package*": getCurrentPackage,
		},
		varSets: map[string]func(Object){
			"*package*": setCurrentPackage,
		},
		varDocs: map[string]string{
			"*package*": "the current package",
		},
	}
	userPkg = Package{
		Name:      "COMMON-LISP-USER",
		Nicknames: []string{"CL-USER", "USER"},
		Doc:       "The default package for user code and variables.",
		Vars:      map[string]Object{},
		Imports:   map[string]*Import{},
		Uses:      []*Use{{Pkg: &clPkg}},
	}
	packages = map[string]*Package{
		clPkg.Name:   &clPkg,
		userPkg.Name: &userPkg,
	}
	currentPackage *Package
)

func init() {
	DefConstant(PackageSymbol, PackageSymbol, `A _package_ represents a namespace.`)

	currentPackage = &userPkg
}

// Import is used to identify what package variables are imported.
type Import struct {
	Pkg  *Package
	Name string
}

// Use is used with Package to list the used packages,
type Use struct {
	Pkg     *Package
	Imports map[string]bool
}

// Package represents a LISP package.
type Package struct {
	Name      string
	Nicknames []string
	Doc       string
	Vars      map[string]Object
	Imports   map[string]*Import
	Uses      []*Use
	varGets   map[string]func() Object
	varSets   map[string]func(Object)
	varDocs   map[string]string
}

// String representation of the Object.
func (obj *Package) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Package) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj *Package) Simplify() interface{} {
	panic(fmt.Sprintf("Can not simplify %s.", obj))
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
	return
}

// FindPackage returns the package matching the provided name.
func FindPackage(name string) *Package {
	// Try the direct way first.
	if pkg := packages[name]; pkg != nil {
		return pkg
	}
	name = strings.ToUpper(name)
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

func getCurrentPackage() Object {
	return currentPackage
}

func setCurrentPackage(value Object) {
	if pkg, ok := value.(*Package); ok {
		currentPackage = pkg
	} else {
		PanicType("*package*", value, "package")
	}
}
