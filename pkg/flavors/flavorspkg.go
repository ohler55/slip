// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

var (
	// FlavorsPkg is the Flavors package.
	FlavorsPkg = slip.Package{
		Name:      "FLAVORS",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the Flavors object model.",
		Vars: map[string]*slip.VarVal{
			"*ALL-FLAVOR-NAMES*": {
				Get: getAllFlavorNames,
				Set: setPanic,
				Doc: "the names of all the defined Flavors.",
			},
		},
		LispCallers: map[string]*slip.LispCaller{},
		Funcs:       map[string]*slip.FuncInfo{},
	}
)

func init() {
	slip.AddPackage(&FlavorsPkg)
	slip.UserPkg.Use(&FlavorsPkg)
}

func getAllFlavorNames() slip.Object {
	names := make(slip.List, 0, len(allFlavors))
	for name := range allFlavors {
		names = append(names, slip.Symbol(name))
	}
	return names
}

func setPanic(value slip.Object) {
	panic("*all-flavor-names* can not be set.")
}
