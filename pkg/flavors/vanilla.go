// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"io"
	"unsafe"

	"github.com/ohler55/slip"
)

var vanilla = Flavor{
	name:        "vanilla-flavor",
	docs:        "A Flavor that implements the standard methods.",
	defaultVars: map[string]slip.Object{"self": nil},
	methods: map[string][]*Method{
		":describe":             {{Name: ":describe", primary: describeCaller{}}},
		":equal":                {{Name: ":equal", primary: equalCaller{}}},
		":eval-inside-yourself": {{Name: ":eval-inside-yourself", primary: insideCaller{}}},
		":flavor":               {{Name: ":flavor", primary: flavorCaller{}}},
		":init":                 {{Name: ":init", primary: initCaller{}}},
		":id":                   {{Name: ":id", primary: idCaller{}}},
		":inspect":              {{Name: ":inspect", primary: inspectCaller{}}},
		":operation-handled-p":  {{Name: ":operation-handled-p", primary: hasOpCaller{}}},
		":print-self":           {{Name: ":print-self", primary: printCaller{}}},
		":send-if-handles":      {{Name: ":send-if-handles", primary: sendIfCaller{}}},
		":which-operations":     {{Name: ":which-operations", primary: whichOpsCaller{}}},
		// The next methods are not standard vanilla-flavor methods but added
		// to support the CLOS change-class function.
		":change-class":      {{Name: ":change-class", primary: changeClassCaller{}}},
		":change-flavor":     {{Name: ":change-flavor", primary: changeClassCaller{}}},
		":shared-initialize": {{Name: ":shared-initialize", primary: sharedInitializeCaller{}}},
		":update-instance-for-different-class": {{
			Name:    ":update-instance-for-different-class",
			primary: updateInstanceForDifferentClassCaller{}}},
	},
	defaultHandler: defHand{},
	pkg:            &Pkg,
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

type initCaller struct{}

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Does nothing.
	return nil
}

func (caller initCaller) Docs() string {
	return `Does nothing but is a placeholder for daemons in sub-flavors.
`
}

type describeCaller struct{}

func (caller describeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	b := self.Describe([]byte{}, 0, right, ansi)
	w := s.Get("*standard-output*").(io.Writer)
	if 0 < len(args) {
		if 1 < len(args) {
			PanicMethodArgCount(self, ":describe", len(args), 0, 1)
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

type idCaller struct{}

func (caller idCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	return slip.Fixnum(uintptr(unsafe.Pointer(self)))
}

func (caller idCaller) Docs() string {
	return `__:id__ => _string_


Returns the identifier of the instance.
`
}

type flavorCaller struct{}

func (caller flavorCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	return self.Type
}

func (caller flavorCaller) Docs() string {
	return `__:flavor__ => _flavor_


Returns the flavor of the instance.
`
}

type hasOpCaller struct{}

func (caller hasOpCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	if len(args) != 1 {
		PanicMethodArgChoice(self, ":has", len(args), "1")
	}
	if sym, ok := args[0].(slip.Symbol); ok && self.HasMethod(string(sym)) {
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

type printCaller struct{}

func (caller printCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Args should be stream print-depth escape-p. The second two arguments are
	// ignored.
	self := s.Get("self").(*Instance)
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
	if _, err := w.Write(self.Append(nil)); err != nil {
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

type sendIfCaller struct{}

func (caller sendIfCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*Instance)
	if len(args) == 0 {
		PanicMethodArgCount(self, ":send-if-handles", len(args), 1, -1)
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		if hm, ok := self.Type.(HasMethods); ok && 0 < len(hm.GetMethod(string(sym))) {
			return self.Receive(s, string(sym), args[1:], depth+1)
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

type whichOpsCaller struct{}

func (caller whichOpsCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)

	return self.Type.MethodNames()
}

func (caller whichOpsCaller) Docs() string {
	return `__:which-operations__ => _list_


Returns a list of all the methods the instance handles.
`
}

type insideCaller struct{}

func (caller insideCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		self := s.Get("self").(*Instance)
		PanicMethodArgChoice(self, ":eval-inside-yourself", len(args), "1")
	}
	return s.Eval(args[0], depth+1)
}

func (caller insideCaller) Docs() string {
	return `__:eval-inside-yourself__ _form_ => _object_
  _form_ [object] to evaluate in the scope of the instance.


Evaluates the _form_ in the instance's scope.
`
}

type inspectCaller struct{}

func (caller inspectCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*Instance)
	cf := allFlavors["bag-flavor"]
	inst := cf.MakeInstance().(*Instance)
	inst.Any = self.Simplify()

	return inst
}

func (caller inspectCaller) Docs() string {
	return `__:inspect__ => _bag_


Returns a _bag_ with the details of the instance.
`
}

type equalCaller struct{}

func (caller equalCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	CheckMethodArgCount(self, ":equal", len(args), 1, 1)
	if self.Equal(args[0]) {
		return slip.True
	}
	return nil
}

func (caller equalCaller) Docs() string {
	return `__:equal__ _other_ => _boolean_
   _other_ [object] other object to compare to _self_.


Returns _t_ if the instance is of the same flavor as _other_ and has the same content.
`
}

type changeClassCaller struct{}

func (caller changeClassCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*Instance)
	CheckMethodArgCount(self, ":change-class", len(args), 1, -1)
	var nf *Flavor
	switch ta := args[0].(type) {
	case *Flavor:
		nf = ta
	case slip.Symbol:
		if nf = Find(string(ta)); nf == nil {
			slip.PanicClassNotFound(ta, "%s is not a defined class or flavor.", ta)
		}
	default:
		slip.PanicType("new-class", args[0], "flavor")
	}
	prev := self.Dup()
	self.ChangeFlavor(nf)
	_ = self.Receive(s, ":update-instance-for-different-class", append(slip.List{prev}, args[1:]...), depth)

	return self
}

func (caller changeClassCaller) Docs() string {
	return `__:change-class__ _new-class_ &key &allow-other-keys) => _instance_
   _new-class_ [flavor] the new flavor for the instance.


Returns __self__ after changing the flavor of the instance. When called a copy
of the instance is created and the _:update-instance-for-different-class_ is
called on the original after the _flavor_ has been changed to the new
_flavor_. The _previous_ is a copy of the original instance. The original
instance has already been changed and the slots adjusted for the new
flavor. This validates the keywords and then calls the _:shared-initialize_
method.


This method is an extension of the original flavors.
`
}

type sharedInitializeCaller struct{}

func (caller sharedInitializeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	rest := args[1:]
	for _, name := range args[0].(slip.List) {
		sym := name.(slip.Symbol)
		if val, has := slip.GetArgsKeyValue(rest, ":"+sym); has {
			self.Let(sym, val)
		}
	}
	return self
}

func (caller sharedInitializeCaller) Docs() string {
	return `__:shared-initialize__ _slot-names_ &rest _initargs_ &key &allow-other-keys) => _instance_
   _slot-names_ [list] a list of the slot names in the re-flavored instance.
   _initargs_ [list] additional arguments are ignored by the default method.


Returns __self__ after processing the key arguments to set the slots in the instance.


This method is an extension of the original flavors.
`
}

type updateInstanceForDifferentClassCaller struct{}

func (caller updateInstanceForDifferentClassCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	// args[0] is previous
	rest := args[1:]
	flavor := self.Type.(*Flavor)
	for i := 0; i < len(rest)-1; i += 2 {
		if sym, ok := rest[i].(slip.Symbol); ok && 1 < len(sym) {
			if _, has := flavor.defaultVars[string(sym[1:])]; has {
				continue
			}
		}
		slip.NewPanic("%s is not a valid initialize keyword for %s.", rest[i], flavor.Name())
	}
	var names slip.List
	for k := range self.Vars {
		names = append(names, slip.Symbol(k))
	}
	self.Receive(s, ":shared-initialize", append(slip.List{names}, rest...), 0)

	return nil
}

func (caller updateInstanceForDifferentClassCaller) Docs() string {
	return `__:update-instance-for-different-class__ _previous_ &rest _initargs_ &key &allow-other-keys)
   _previous_ [instance] a copy of the original instance.
   _initargs_ [list] additional arguments are ignored by the default method.


When __change-class__ is called a copy of the instance is created and the
_:update-instance-for-different-class_ is called on the original after the
_flavor_ has been changed to the new _flavor_. The _previous_ is a copy of the
original instance. The original instance has already been changed and the
slots adjusted for the new flavor. This validates the keywords and then calls
the _:shared-initialize_ method.


This method is an extension of the original flavors.
`
}
