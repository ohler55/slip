// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strings"
)

// KeywordPkg is the KEYWORD package.
var (
	keywordPkg *Package
	KeywordPkg = Package{
		Name:      "keyword",
		Nicknames: []string{},
		Doc:       "Home of keyword symbols.",
		vars: map[string]*VarVal{
			// Add a few examples...
			":yes": {Val: Symbol(":yes")},
			":no":  {Val: Symbol(":no")},
		},
		lambdas: map[string]*Lambda{},
		funcs:   map[string]*FuncInfo{},
		PreSet:  keywordPreSet,
	}
)

func init() {
	for _, vv := range KeywordPkg.vars {
		vv.Pkg = &KeywordPkg
	}
	KeywordPkg.Set("keyword", &KeywordPkg)
	AddPackage(&KeywordPkg)
	UserPkg.Use(&KeywordPkg)
	keywordPkg = &KeywordPkg
}

func keywordPreSet(p *Package, name string, value Object) (string, Object) {
	if len(name) == 0 {
		PanicPackage(keywordPkg, "An empty symbol is not a valid keyword.")
	}
	if name[0] != ':' {
		name = ":" + name
	}
	name = strings.ToLower(name)
	if vv, has := p.vars[name]; has {
		if ObjectEqual(value, vv.Val) {
			return name, value
		}
		PanicPackage(keywordPkg, "%s is a constant and thus can't be set", name)
	}
	value = Symbol(name)

	return name, value
}
