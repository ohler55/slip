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
	methods: map[string]*slip.Method{
		":describe":             {Combinations: []*slip.Combination{{Primary: describeCaller{}}}},
		":equal":                {Combinations: []*slip.Combination{{Primary: equalCaller{}}}},
		":eval-inside-yourself": {Combinations: []*slip.Combination{{Primary: insideCaller{}}}},
		":flavor":               {Combinations: []*slip.Combination{{Primary: flavorCaller{}}}},
		":init":                 {Combinations: []*slip.Combination{{Primary: initCaller{}}}},
		":id":                   {Combinations: []*slip.Combination{{Primary: idCaller{}}}},
		":inspect":              {Combinations: []*slip.Combination{{Primary: inspectCaller{}}}},
		":operation-handled-p":  {Combinations: []*slip.Combination{{Primary: hasOpCaller{}}}},
		":print-self":           {Combinations: []*slip.Combination{{Primary: printCaller{}}}},
		":send-if-handles":      {Combinations: []*slip.Combination{{Primary: sendIfCaller{}}}},
		":which-operations":     {Combinations: []*slip.Combination{{Primary: whichOpsCaller{}}}},
		// The next methods are not standard vanilla-flavor methods but added
		// to support the CLOS change-class function.
		":change-class":      {Combinations: []*slip.Combination{{Primary: changeClassCaller{}}}},
		":change-flavor":     {Combinations: []*slip.Combination{{Primary: changeClassCaller{}}}},
		":shared-initialize": {Combinations: []*slip.Combination{{Primary: sharedInitializeCaller{}}}},
		":update-instance-for-different-class": {
			Combinations: []*slip.Combination{{Primary: updateInstanceForDifferentClassCaller{}}},
		},
	},
	defaultHandler: defHand{},
	precedence:     []slip.Symbol{slip.Symbol("vanilla-flavor"), InstanceSymbol, slip.TrueSymbol},
	pkg:            &Pkg,
}

func init() {
	for name, m := range vanilla.methods {
		m.Name = name
		for _, c := range m.Combinations { // only one
			c.From = &vanilla
			if prime, ok := c.Primary.(slip.HasFuncDocs); ok {
				m.Doc = prime.FuncDocs()
			}
		}
	}
}

// VanillaMethods returns the vanilla methods. (used for the clos standard methods)
func VanillaMethods() map[string]*slip.Method {
	return vanilla.methods
}

type initCaller struct{}

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// Does nothing.
	return nil
}

func (caller initCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Does nothing but is a placeholder for daemons in sub-flavors.",
	}
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
			slip.PanicMethodArgCount(self, ":describe", len(args), 0, 1)
		}
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType(":describe output-stream", args[0], "output-stream")
		}
	}
	_, _ = w.Write(b)

	return nil
}

func (caller describeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":describe",
		Text: "Writes a description of the instance to _output-stream_.",
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name:    "output-stream",
				Type:    "output-stream",
				Text:    "output-stream to write to.",
				Default: slip.Symbol("*standard-output*"),
			},
		},
	}
}

type idCaller struct{}

func (caller idCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	return slip.Fixnum(uintptr(unsafe.Pointer(self)))
}

func (caller idCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":id",
		Text:   "Returns the identifier of the instance.",
		Return: "fixnum",
	}
}

type flavorCaller struct{}

func (caller flavorCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	return self.Type
}

func (caller flavorCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":flavor",
		Text:   "Returns the flavor of the instance.",
		Return: "flavor",
	}
}

type hasOpCaller struct{}

func (caller hasOpCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	if len(args) != 1 {
		slip.PanicMethodArgChoice(self, ":has", len(args), "1")
	}
	if sym, ok := args[0].(slip.Symbol); ok && self.HasMethod(string(sym)) {
		return slip.True
	}
	return nil
}

func (caller hasOpCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":operation-handled-p",
		Text: "Returns _t_ if the instance handles the method and _nil_ otherwise.",
		Args: []*slip.DocArg{
			{
				Name: "method",
				Type: "keyword",
				Text: "Symbol to check.",
			},
		},
		Return: "boolean",
	}
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

func (caller printCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":print-self",
		Text: "Writes a description of the instance to the _stream_.",
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name:    "output-stream",
				Type:    "output-stream",
				Text:    "output-stream to write the description to.",
				Default: slip.Symbol("*standard-output*"),
			},
			{Name: "&rest"},
			{Name: "rest", Text: "_ignored_"},
		},
	}
}

type sendIfCaller struct{}

func (caller sendIfCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*Instance)
	if len(args) == 0 {
		slip.PanicMethodArgCount(self, ":send-if-handles", len(args), 1, -1)
	}
	if sym, ok := args[0].(slip.Symbol); ok {
		if self.Type.GetMethod(string(sym)) != nil {
			return self.Receive(s, string(sym), args[1:], depth+1)
		}
	}
	return nil
}

func (caller sendIfCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":send-if-handles",
		Text: "Sends to the instance if the instance has the _method_.",
		Args: []*slip.DocArg{
			{
				Name: "method",
				Type: "keyword",
				Text: "Method to send to the instance if the instance has the _method_.",
			},
			{Name: "&rest"},
			{
				Name: "arguments*",
				Text: "Argument to pass to the _method_ call.",
			},
		},
		Return: "object",
	}
}

type whichOpsCaller struct{}

func (caller whichOpsCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)

	return self.Type.MethodNames()
}

func (caller whichOpsCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":which-operations",
		Text:   "Returns a list of all the methods the instance handles.",
		Return: "list",
	}
}

type insideCaller struct{}

func (caller insideCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		self := s.Get("self").(*Instance)
		slip.PanicMethodArgChoice(self, ":eval-inside-yourself", len(args), "1")
	}
	return s.Eval(args[0], depth+1)
}

func (caller insideCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":eval-inside-yourself",
		Text: "Evaluates the _form_ in the instance's scope.",
		Args: []*slip.DocArg{
			{
				Name: "form",
				Type: "object",
				Text: "Form to evaluate in the scope of the instance.",
			},
		},
		Return: "object",
	}
}

type inspectCaller struct{}

func (caller inspectCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*Instance)
	cf := allFlavors["bag-flavor"]
	inst := cf.MakeInstance().(*Instance)
	inst.Any = self.Simplify()

	return inst
}

func (caller inspectCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":inspect",
		Text:   "Returns a _bag_ with the details of the instance.",
		Return: "bag",
	}
}

type equalCaller struct{}

func (caller equalCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	slip.CheckMethodArgCount(self, ":equal", len(args), 1, 1)
	if self.Equal(args[0]) {
		return slip.True
	}
	return nil
}

func (caller equalCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":equal",
		Text: "Returns _t_ if the instance is of the same flavor as _other_ and has the same content.",
		Args: []*slip.DocArg{
			{
				Name: "other",
				Type: "object",
				Text: "Other object to compare to _self_.",
			},
		},
		Return: "boolean",
	}
}

type changeClassCaller struct{}

func (caller changeClassCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*Instance)
	slip.CheckMethodArgCount(self, ":change-class", len(args), 1, -1)
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

func (caller changeClassCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":change-class",
		Text: `Returns __self__ after changing the flavor of the instance. When called a copy
of the instance is created and the _:update-instance-for-different-class_ is
called on the original after the _flavor_ has been changed to the new
_flavor_. The _previous_ is a copy of the original instance. The original
instance has already been changed and the slots adjusted for the new
flavor. This validates the keywords and then calls the _:shared-initialize_
method.


This method is an extension of the original flavors.
`,
		Args: []*slip.DocArg{
			{
				Name: "new-class",
				Type: "flavor",
				Text: "The new flavor for the instance.",
			},
		},
		Return: "instance",
	}
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

func (caller sharedInitializeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":shared-initialize",
		Text: `Returns __self__ after processing the key arguments to set the slots in the instance.


This method is an extension of the original flavors.
`,
		Args: []*slip.DocArg{
			{
				Name: "slot-names",
				Type: "list",
				Text: "A list of the slot names in the re-flavored instance.",
			},
			{Name: "&rest"},
			{
				Name: "initargs",
				Type: "list",
				Text: "Additional arguments are ignored by the default method.",
			},
			{Name: "&key"},
			{Name: "&allow-other-keys"},
		},
		Return: "instance",
	}
}

type updateInstanceForDifferentClassCaller struct{}

func (caller updateInstanceForDifferentClassCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*Instance)
	// args[0] is previous
	rest := args[1:]
	for i := 0; i < len(rest)-1; i += 2 {
		if sym, ok := rest[i].(slip.Symbol); ok && 1 < len(sym) {
			if _, has := self.Type.defaultVars[string(sym[1:])]; has {
				continue
			}
		}
		slip.NewPanic("%s is not a valid initialize keyword for %s.", rest[i], self.Type.name)
	}
	var names slip.List
	for k := range self.Vars {
		names = append(names, slip.Symbol(k))
	}
	self.Receive(s, ":shared-initialize", append(slip.List{names}, rest...), 0)

	return nil
}

func (caller updateInstanceForDifferentClassCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":update-instance-for-different-class",
		Text: `When __change-class__ is called a copy of the instance is created and the
_:update-instance-for-different-class_ is called on the original after the
_flavor_ has been changed to the new _flavor_. The _previous_ is a copy of the
original instance. The original instance has already been changed and the
slots adjusted for the new flavor. This validates the keywords and then calls
the _:shared-initialize_ method.


This method is an extension of the original flavors.
`,
		Args: []*slip.DocArg{
			{
				Name: "previous",
				Type: "instance",
				Text: "a copy of the original instance.",
			},
			{Name: "&rest"},
			{
				Name: "initargs",
				Type: "list",
				Text: "Additional arguments are ignored by the default method.",
			},
			{Name: "&key"},
			{Name: "&allow-other-keys"},
		},
		Return: "instance",
	}
}
