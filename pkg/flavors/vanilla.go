// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
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
	methods: map[string][]*Method{
		":describe":             {{Name: ":describe", primary: describeCaller(true)}},
		":eval-inside-yourself": {{Name: ":eval-inside-yourself", primary: insideCaller(true)}},
		":flavor":               {{Name: ":flavor", primary: flavorCaller(true)}},
		":init":                 {{Name: ":init", primary: initCaller(true)}},
		":id":                   {{Name: ":id", primary: idCaller(true)}},
		":inspect":              {{Name: ":inspect", primary: inspectCaller(true)}},
		":operation-handled-p":  {{Name: ":operation-handled-p", primary: hasOpCaller(true)}},
		":print-self":           {{Name: ":print-self", primary: printCaller(true)}},
		":send-if-handles":      {{Name: ":send-if-handles", primary: sendIfCaller(true)}},
		":which-operations":     {{Name: ":which-operations", primary: whichOpsCaller(true)}},
	},
}

func init() {
	for _, ma := range vanilla.methods {
		ma[0].From = &vanilla
	}
}

// VanillaMethods returns the vanilla methods. (used for the clos standard methods)
func VanillaMethods() map[string][]*Method {
	return vanilla.methods
}

type initCaller bool

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Does nothing.
	return nil
}

func (caller initCaller) Docs() string {
	return `Does nothing but is a placeholder for daemons in sub-flavors.
`
}

type describeCaller bool

func (caller describeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	b := obj.Describe([]byte{}, 0, right, ansi)
	w := s.Get("*standard-output*").(io.Writer)
	if 0 < len(args) {
		if 1 < len(args) {
			PanicMethodArgCount(obj, ":describe", len(args), 0, 1)
		}
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType(":describe output-stream", args[0], "output-stream")
		}
	}
	_, _ = w.Write(b)

	return nil
}

func (caller describeCaller) Docs() string {
	return `__:describe__ &optional _output-stream_
  _output-stream_ [output-stream] to write to. (default: _*standard-output*_)


Writes a description of the instance to _output-stream_.
`
}

type idCaller bool

func (caller idCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	return slip.Fixnum(uintptr(unsafe.Pointer(obj)))
}

func (caller idCaller) Docs() string {
	return `__:id__ => _string_


Returns the identifier of the instance.
`
}

type flavorCaller bool

func (caller flavorCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	return obj.Type
}

func (caller flavorCaller) Docs() string {
	return `__:flavor__ => _flavor_


Returns the flavor of the instance.
`
}

type hasOpCaller bool

func (caller hasOpCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	if len(args) != 1 {
		PanicMethodArgChoice(obj, ":has", len(args), "1")
	}
	if sym, ok := args[0].(slip.Symbol); ok && obj.HasMethod(string(sym)) {
		return slip.True
	}
	return nil
}

func (caller hasOpCaller) Docs() string {
	return `__:operation-handled-p__ _method_ => _boolean_
  _method_ [keyword] to check.


Returns _t_ if the instance handles the method and _nil_ otherwise.
`
}

type printCaller bool

func (caller printCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Args should be stream print-depth escape-p. The second two arguments are
	// ignored.
	obj := s.Get("self").(*Instance)
	so := s.Get("*standard-output*")
	ss, _ := so.(slip.Stream)
	w := so.(io.Writer)
	if 0 < len(args) {
		var ok bool
		ss, _ = args[0].(slip.Stream)
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType(":describe output-stream", args[0], "output-stream")
		}
	}
	if _, err := w.Write(obj.Append(nil)); err != nil {
		slip.PanicStream(ss, "%s", err)
	}
	return nil
}

func (caller printCaller) Docs() string {
	return `__:print-self__ &optional _stream_ &rest _ignored_
  _stream_ [output-stream] to write the description to. The default is _*standard-output*_


Writes a description of the instance to the _stream_.
`
}

type sendIfCaller bool

func (caller sendIfCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*Instance)
	if len(args) == 0 {
		PanicMethodArgCount(obj, ":send-if-handles", len(args), 1, -1)
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		if _, has := obj.Methods[string(sym)]; has {
			return obj.Receive(s, string(sym), args[1:], depth+1)
		}
	}
	return nil
}

func (caller sendIfCaller) Docs() string {
	return `__:send-if-handles__ _method_ _arguments*_ => _object_
  _method_ [keyword] to send to the instance if the instance has the _method_.
  _arguments*_ to pass to the _method_ call.


Sends to the instance if the instance has the _method_.
`
}

type whichOpsCaller bool

func (caller whichOpsCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*Instance)
	names := make([]string, 0, len(obj.Methods))
	for k := range obj.Methods {
		names = append(names, k)
	}
	sort.Slice(names, func(i, j int) bool { return 0 > strings.Compare(names[i], names[j]) })
	methods := make(slip.List, 0, len(names))
	for _, name := range names {
		methods = append(methods, slip.Symbol(name))
	}
	return methods
}

func (caller whichOpsCaller) Docs() string {
	return `__:which-operations__ => _list_


Returns a list of all the methods the instance handles.
`
}

type insideCaller bool

func (caller insideCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		obj := s.Get("self").(*Instance)
		PanicMethodArgChoice(obj, ":eval-inside-yourself", len(args), "1")
	}
	return s.Eval(args[0], depth+1)
}

func (caller insideCaller) Docs() string {
	return `__:eval-inside-yourself__ _form_ => _object_
  _form_ [object] to evaluate in the scope of the instance.


Evaluates the _form_ in the instance's scope.
`
}

type inspectCaller bool

func (caller inspectCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*Instance)
	cf := allFlavors["bag-flavor"]
	inst := cf.MakeInstance().(*Instance)
	inst.Any = obj.Simplify()

	return inst
}

func (caller inspectCaller) Docs() string {
	return `__:inspect__ => _bag_


Returns a _bag_ with the details of the instance.
`
}
