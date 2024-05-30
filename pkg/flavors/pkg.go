// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

var (
	// Pkg is the Flavors package.
	Pkg = slip.Package{
		Name:      "flavors",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the Flavors object model.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(map[string]*slip.VarVal{
		"*all-flavor-names*": {
			Get:    getAllFlavorNames,
			Set:    setPanic,
			Doc:    "the names of all the defined Flavors.",
			Export: true,
		},
		"*flavors*":  {Val: &Pkg, Doc: Pkg.Doc, Export: true},
		vanilla.name: {Val: &vanilla, Doc: vanilla.docs, Export: true},
	})
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}

func getAllFlavorNames() slip.Object {
	keys := make([]string, 0, len(allFlavors))
	for key := range allFlavors {
		keys = append(keys, key)
	}
	sort.Slice(keys, func(i, j int) bool { return 0 > strings.Compare(keys[i], keys[j]) })
	names := make(slip.List, 0, len(allFlavors))
	for _, name := range keys {
		names = append(names, slip.Symbol(name))
	}
	return names
}

func setPanic(value slip.Object) {
	slip.NewPanic("*all-flavor-names* can not be set.")
}
