// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"sort"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/generic"
)

var flavor *flavors.Flavor

type entry struct {
	key   uint64
	value slip.Object
}

type ordered []entry

// Length returns the length of the set.
func (or ordered) Length() int {
	return len(or)
}

func (or ordered) sort() {
	sort.Slice(or, func(i, j int) bool { return or[i].key < or[j].key })
}

func (or ordered) getEntry(key uint64) (*entry, int) {
	if len(or) == 0 {
		return nil, -1
	}
	lo := 0
	lok := or[lo].key
	if lok == key {
		return &or[lo], lo
	}
	if key < lok {
		return nil, -1
	}
	hi := len(or) - 1
	hik := or[hi].key
	if hik == key {
		return &or[hi], hi
	}
	if hik < key {
		return nil, -1
	}
	for lo < hi {
		i := lo + int((float64(hi-lo)*float64(key-lok))/float64(hik-lok))
		if i == lo {
			i++
			if hi == i {
				break
			}
		}
		k := or[i].key
		if key < k {
			hi = i
			hik = k
			continue
		}
		if k < key {
			lo = i
			lok = k
			continue
		}
		return &or[i], i
	}
	return nil, -1
}

func defWos() *flavors.Flavor {
	Pkg.Initialize(nil)
	flavor = flavors.DefFlavor("wos-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A wos is TBD`),
			},
		},
		&Pkg,
	)
	generic.DefClassMethod(flavor, ":init", "", initCaller{})
	generic.DefClassMethod(flavor, ":get", "", getCaller{})
	generic.DefClassMethod(flavor, ":set", "", setCaller{})
	generic.DefClassMethod(flavor, ":has", "", hasCaller{})
	generic.DefClassMethod(flavor, ":clear", "", clearCaller{})
	generic.DefClassMethod(flavor, ":remove", "", removeCaller{})

	// TBD other methods
	// - mapwos (function wos) - function take 2 args, index and value
	// - with-wos-iterator (next wos) *forms
	//  - next is a function that return a values of 3; has-next, index, value
	// - wos-add (wos &rest index-values)
	//  - index-values can be assoc or property list, detect by first value or allow a mix?
	//  - more efficient way to bulk add data, caveat is the data has to be trusted to not have duplicate keys

	return flavor
}

// Flavor returns the wos-flavor.
func Flavor() *flavors.Flavor {
	return flavor
}

type initCaller struct{}

func (caller initCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	self.Any = ordered{}

	return nil
}

func (caller initCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Initialized the instance when _make-instance_ is called.",
	}
}
