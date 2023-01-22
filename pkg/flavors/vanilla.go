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
		":describe":             {{name: ":describe", primary: describeCaller(true)}},
		":eval-inside-yourself": {{name: ":eval-inside-yourself", primary: insideCaller(true)}},
		":flavor":               {{name: ":flavor", primary: flavorCaller(true)}},
		":init":                 {{name: ":init", primary: initCaller(true)}},
		":id":                   {{name: ":id", primary: idCaller(true)}},
		":inspect":              {{name: ":inspect", primary: inspectCaller(true)}},
		":operation-handler-p":  {{name: ":operation-handler-p", primary: hasOpCaller(true)}},
		":print-self":           {{name: ":print-self", primary: printCaller(true)}},
		":send-if-handles":      {{name: ":send-if-handles", primary: sendIfCaller(true)}},
		":which-operations":     {{name: ":which-operations", primary: whichOpsCaller(true)}},
	},
}

func init() {
	for _, ma := range vanilla.methods {
		ma[0].from = &vanilla
	}
	Pkg.Set(vanilla.name, &vanilla)
}

type initCaller bool

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Does nothing.
	return nil
}

type describeCaller bool

func (caller describeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	b := obj.Describe([]byte{}, 0, right, ansi)
	_, _ = slip.StandardOutput.(io.Writer).Write(b)

	return nil
}

type idCaller bool

func (caller idCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	return slip.Fixnum(uintptr(unsafe.Pointer(obj)))
}

type flavorCaller bool

func (caller flavorCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	return obj.Flavor
}

type hasOpCaller bool

func (caller hasOpCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	if len(args) != 1 {
		panic(fmt.Sprintf("Method operation-handler-p expects 1 argument but received %d.", len(args)))
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		if _, has := obj.Flavor.methods[string(sym)]; has {
			return slip.True
		}
	}
	return nil
}

type printCaller bool

func (caller printCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Args should be stream printdepth escape-p. The second two arguments are
	// ignored.
	obj := s.Get("self").(*Instance)
	if len(args) != 3 {
		panic(fmt.Sprintf("Method print-self expects 3 argument but received %d.", len(args)))
	}
	w, ok := args[len(args)-1].(io.Writer)
	if !ok {
		panic(fmt.Sprintf("Method print-self expects the first argument to be an output-stream not a %T.",
			args[len(args)-1]))
	}
	if _, err := w.Write(obj.Append([]byte{})); err != nil {
		panic(err)
	}
	return nil
}

type sendIfCaller bool

func (caller sendIfCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*Instance)
	if len(args) == 0 {
		panic("Method send-if-handles expects at least 1 argument but received 0.")
	}
	pos := len(args) - 1
	if sym, ok := args[pos].(slip.Symbol); ok {
		if _, has := obj.Flavor.methods[string(sym)]; has {
			return obj.Receive(string(sym), args[:pos], depth+1)
		}
	}
	return nil
}

type whichOpsCaller bool

func (caller whichOpsCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	names := make([]string, 0, len(obj.Flavor.methods))
	for k := range obj.Flavor.methods {
		names = append(names, k)
	}
	sort.Slice(names, func(i, j int) bool { return 0 < strings.Compare(names[i], names[j]) })
	methods := make(slip.List, 0, len(names))
	for _, name := range names {
		methods = append(methods, slip.Symbol(name))
	}
	return methods
}

type insideCaller bool

func (caller insideCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		panic(fmt.Sprintf("Method eval-inside-yourself expects 1 argument but received %d.", len(args)))
	}
	return s.Eval(args[0], depth+1)
}

type inspectCaller bool

func (caller inspectCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*Instance)
	cf := allFlavors["bag-flavor"]
	inst := cf.MakeInstance()
	inst.Any = obj.Simplify()

	return inst
}
