// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

// KeywordPkg is the KEYWORD package.
var (
	KeywordPkg = Package{
		Name:      "keyword",
		Nicknames: []string{},
		Doc:       "Home of keyword symbols.",
		Vars: map[string]*VarVal{
			// Add a few examples...
			":yes": {Val: Symbol(":yes")},
			":no":  {Val: Symbol(":no")},
		},
		Lambdas: map[string]*Lambda{},
		Funcs:   map[string]*FuncInfo{},
		PreSet:  keywordPreSet,
	}
)

func init() {
	for _, vv := range KeywordPkg.Vars {
		vv.Pkg = &KeywordPkg
	}
	KeywordPkg.Set("keyword", &KeywordPkg)
	AddPackage(&KeywordPkg)
	UserPkg.Use(&KeywordPkg)
}

func keywordPreSet(p *Package, name string, value Object) (string, Object) {
	if len(name) == 0 {
		panic("An empty symbol is not a valid keyword.")
	}
	if name[0] != ':' {
		name = ":" + name
	}
	name = strings.ToLower(name)
	if vv, has := p.Vars[name]; has {
		if ObjectEqual(value, vv.Val) {
			return name, value
		}
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	value = Symbol(name)

	return name, value
}
