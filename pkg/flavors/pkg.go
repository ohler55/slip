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
			Export: true,
			Doc:    "The names of all the defined Flavors.",
		},
		"*flavors*": {
			Val:    &Pkg,
			Doc:    Pkg.Doc,
			Const:  true,
			Export: true,
		},
		vanilla.Name(): {
			Val:    &vanilla,
			Const:  true,
			Export: true,
			Doc:    "The vanilla flavor.",
		},
		string(FlavorSymbol): {
			Val:    FlavorSymbol,
			Const:  true,
			Export: true,
			Doc: `A _flavor_ encapsulates a class of objects or instances.
The _flavor_ itself is an instance and can be sent a limited set of methods.`,
		},
		string(InstanceSymbol): {
			Val:    InstanceSymbol,
			Const:  true,
			Export: true,
			Doc:    `An _instance_ of a _flavor_.`,
		},
		string(slip.WhopLocSymbol): {
			Val:    slip.WhopLocSymbol,
			Const:  true,
			Export: true,
			Doc:    `A whopper location. Private for continue-whopper.`,
		},
	})
	Pkg.RegisterClass(vanilla.name, &vanilla)

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
