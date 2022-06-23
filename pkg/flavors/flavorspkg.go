// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

var (
	// FlavorsPkg is the Flavors package.
	FlavorsPkg = slip.Package{
		Name:      "flavors",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the Flavors object model.",
		Vars: map[string]*slip.VarVal{
			"*all-flavor-names*": {
				Get: getAllFlavorNames,
				Set: setPanic,
				Doc: "the names of all the defined Flavors.",
			},
		},
		Lambdas: map[string]*slip.Lambda{},
		Funcs:   map[string]*slip.FuncInfo{},
	}
)

func init() {
	slip.AddPackage(&FlavorsPkg)
	slip.UserPkg.Use(&FlavorsPkg)
	for _, vv := range FlavorsPkg.Vars {
		vv.Pkg = &FlavorsPkg
	}
}

func getAllFlavorNames() slip.Object {
	keys := make([]string, 0, len(allFlavors))
	for key := range allFlavors {
		keys = append(keys, key)
	}
	sort.Slice(keys, func(i, j int) bool { return 0 < strings.Compare(keys[i], keys[j]) })
	names := make(slip.List, 0, len(allFlavors))
	for _, name := range keys {
		names = append(names, slip.Symbol(name))
	}
	return names
}

func setPanic(value slip.Object) {
	panic("*all-flavor-names* can not be set.")
}
