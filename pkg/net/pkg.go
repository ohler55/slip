// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"sort"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	// Pkg is the net package.
	Pkg = slip.Package{
		Name:      "net",
		Nicknames: []string{"networking", "network"},
		Doc: `Home of symbols defined for the net (networking) package. This includes
socket classes and functions modeled after the SBCL networking package. Some additional
functions are included and key arguments to some functions such as socket-send differ but
most functions are the same.
`,
		PreSet: slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*net*": {
				Val:    &Pkg,
				Doc:    Pkg.Doc,
				Const:  true,
				Export: true,
			},
			"*wildcard-host*": {
				Val:    slip.Octets{0, 0, 0, 0},
				Export: true,
			},
			"*auto-port*": {
				Val:    slip.Fixnum(0),
				Export: true,
			},
		},
	)
	defNameServiceError()
	for _, f := range []*flavors.Flavor{
		defClient(),
		defRequest(),
		defResponse(),
		defResponseWriter(),
		defServer(),
		defSocket(),
		defHostent(),
	} {
		vv := Pkg.GetVarVal(f.Name())
		vv.Const = true
	}
	Pkg.Initialize(nil, &bodyWrap{}) // lock
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}

func getSockArgValue(name string, arg slip.Object, argMap map[slip.Symbol]int) int {
	sym, _ := arg.(slip.Symbol)
	if _, has := argMap[sym]; !has {
		keys := make([]string, 0, len(argMap))
		for sym := range argMap {
			keys = append(keys, string(sym))
		}
		sort.Strings(keys)
		slip.PanicType(name, arg, keys...)
	}
	return argMap[sym]
}

func socketArgText(name string, argMap map[slip.Symbol]int) string {
	var b []byte
	b = append(b, "the socket "...)
	b = append(b, name...)
	b = append(b, ". Valid options are:"...)
	keys := make([]string, 0, len(argMap))
	for sym := range argMap {
		keys = append(keys, string(sym))
	}
	sort.Strings(keys)
	for _, key := range keys {
		b = append(b, ' ')
		b = append(b, key...)
	}
	return string(b)
}
