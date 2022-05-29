// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"
	"io"
	"sort"
	"strings"
	"unsafe"

	"github.com/ohler55/slip"
)

var vanilla = Flavor{
	name:        "vanilla-flavor",
	docs:        "A Flavor that implements the standard methods.",
	defaultVars: map[string]slip.Object{"self": nil},
	methods: map[string][]*method{
		":describe":            []*method{{name: ":describe", primary: describeCaller(true)}},
		":init":                []*method{{name: ":init", primary: initCaller(true)}},
		":id":                  []*method{{name: ":id", primary: idCaller(true)}},
		":operation-handler-p": []*method{{name: ":operation-handler-p", primary: hasOpCaller(true)}},
		":print-self":          []*method{{name: ":print-self", primary: printCaller(true)}},
		":send-if-handles":     []*method{{name: ":send-if-handles", primary: sendIfCaller(true)}},
		":which-operations":    []*method{{name: ":which-operations", primary: whichOpsCaller(true)}},
	},
}

func init() {
	for _, ma := range vanilla.methods {
		ma[0].from = &vanilla
	}
	FlavorsPkg.Set(vanilla.name, &vanilla)
}

type initCaller bool

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Does nothing.
	return nil
}

type describeCaller bool

func (caller describeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	b := obj.Append([]byte{})
	b = append(b, ", an object of flavor "...)
	b = append(b, obj.flavor.name...)
	b = append(b, ",\n  has instance variable values:\n"...)
	keys := make([]string, 0, len(obj.Vars))
	for k := range obj.Vars {
		if k != "self" {
			keys = append(keys, k)
		}
	}
	sort.Strings(keys)
	for _, k := range keys {
		b = append(b, "    "...)
		b = append(b, k...)
		b = append(b, ": "...)
		b = slip.ObjectAppend(b, obj.Vars[k])
		b = append(b, '\n')
	}
	_, _ = slip.StandardOutput.(io.Writer).Write(b)

	return nil
}

type idCaller bool

func (caller idCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	return slip.Fixnum(uintptr(unsafe.Pointer(obj)))
}

type hasOpCaller bool

func (caller hasOpCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	if len(args) != 1 {
		panic(fmt.Sprintf("Method operation-handler-p expects 1 argument but received %d.", len(args)))
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		if _, has := obj.flavor.methods[string(sym)]; has {
			return slip.True
		}
	}
	return nil
}

type printCaller bool

func (caller printCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	// has 3 args
	// maybe don't implement?
	return nil
}

type sendIfCaller bool

func (caller sendIfCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	return nil
}

type whichOpsCaller bool

func (caller whichOpsCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	names := make([]string, 0, len(obj.flavor.methods))
	for k := range obj.flavor.methods {
		names = append(names, k)
	}
	sort.Slice(names, func(i, j int) bool { return 0 < strings.Compare(names[i], names[j]) })
	methods := make(slip.List, 0, len(names))
	for _, name := range names {
		methods = append(methods, slip.Symbol(name))
	}
	return methods
}
