// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/generic"
)

func defDefclass() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defclass{Function: slip.Function{Name: "defclass", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "defclass",
			Args: []*slip.DocArg{
				{
					Name: "class-name",
					Type: "symbol",
					Text: "The name of the class to define.",
				},
				{
					Name: "superclass-names",
					Type: "list",
					Text: `A list of superclass names where each member of the list is a symbol.`,
				},
				{
					Name: "slot-specifiers",
					Type: "list",
					Text: `A list of slot specifiers which can be a symbol that is the name of a slot
or a list of the slot name and a property list of slot options. Slot options are:
  __:reader__ [symbol] the name of a generic function to read the value of the slot.
  __:writer__ [symbol] the name of a generic function to set the value of the slot.
  __:accessor__ [symbol] the name of a generic function to read the value of
the slot that can also be used with __setf__.
  __:allocation__ [:instance_|_:class] indicates where the slot is located.
  __:initarg__ [symbol] initial argument keys words. (multiple are allowed)
  __:initform__ [form] an expression or object to evaluate on instance initialization.
  __:type__ [type-specifier] a type specification to validate initial values.
  __:documentation__ [string] documentation for the slot.
  __:gettable__ [boolean] if true a direct method with the slot name prefixed with a ':' will return the slot value.
This is an addition to the CLOS specification.
  __:settable__ [boolean] if true a direct method with the slot name prefixed with ':set-' will set the slot value.
This is an addition to the CLOS specification.
`,
				},
				{Name: slip.AmpRest},
				{
					Name: "class-options",
					Type: "list",
					Text: `Each option must be a list with the first element being one of the
following keywords:
  __:documentation__ followed by a strings becomes the documentation for the class.
  __:default-initargs__ followed by alternating initarg names and forms that are evaluated on each call.
  __:metaclass__ not supported. Ignored.

`,
				},
			},
			Return: "standard-class",
			Text: `__defclass__ defines a new class and returns the new class. If the named
class already exists it is over-written but existing objects continue to reference the original
class.`,
			Examples: []string{
				"(defclass berry () (x y)) => #<standard-class berry>",
			},
		}, &Pkg)
}

// Defclass represents the defclass function.
type Defclass struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defclass) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 3, 6)
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "class-name", args[0], "symbol")
	}
	var (
		supers    slip.List
		slotSpecs slip.List
	)
	if supers, ok = args[1].(slip.List); !ok {
		slip.TypePanic(s, depth, "superclass-names", args[1], "list")
	}
	if slotSpecs, ok = args[2].(slip.List); !ok {
		slip.TypePanic(s, depth, "slot-specifiers", args[2], "list")
	}
	if c := slip.FindClass(string(name)); c != nil {
		if sc, ok := c.(*StandardClass); !ok || sc.Final {
			slip.ErrorPanic(s, depth, "Can not redefine class %s, a %s.", name, c.Metaclass())
		}
	}
	return DefStandardClass(s, string(name), supers, slotSpecs, args[3:], depth)
}

// DefStandardClass defines a standard-class.
func DefStandardClass(s *slip.Scope, name string, supers, slotSpecs, classOptions slip.List, depth int) *StandardClass {
	sc := StandardClass{
		name:            name,
		defname:         "defclass",
		slotDefs:        map[string]*SlotDef{},
		pkg:             slip.CurrentPackage,
		supers:          make([]slip.Symbol, len(supers)),
		defaultInitArgs: map[string]slip.Object{},
		initArgs:        map[string]*SlotDef{},
		initForms:       map[string]*SlotDef{},
		methods:         map[string]*slip.Method{},
		inheritCheck: func(c slip.Class) *StandardClass {
			sc, ok := c.(*StandardClass)
			if !ok && c != nil {
				slip.TypePanic(s, depth, "superclass", c, "standard-class")
			}
			return sc
		},
		baseClass: StandardObjectSymbol,
	}
	for i, super := range supers {
		if sym, ok := super.(slip.Symbol); ok {
			sc.supers[i] = sym
		} else {
			slip.TypePanic(s, depth, "super", super, "symbol")
		}
	}
	sc.Init(false)

	for _, opt := range classOptions {
		list, ok := opt.(slip.List)
		if !ok || len(list) < 2 {
			slip.TypePanic(s, depth, "class-options", opt, "list")
		}
		switch list[0] {
		case slip.Symbol(":documentation"):
			if docs, ok := list[1].(slip.String); ok {
				sc.docs = string(docs)
			} else {
				slip.TypePanic(s, depth, "class-options :documentation", list[1], "string")
			}
		case slip.Symbol(":default-initargs"):
			fillMapFromKeyArgs(s, list[1:], sc.defaultInitArgs, depth)
		case slip.Symbol(":metaclass"):
			// ignore
		default:
			slip.TypePanic(s, depth, "class-options directive", list[0], ":documentation", "default-initargs", ":metaclass")
		}
	}
	for _, ss := range slotSpecs {
		sd := NewSlotDef(s, ss, depth)
		sd.class = &sc
		sc.slotDefs[sd.name] = sd
		if sd.classStore {
			sc.vars[sd.name] = sd.initform
		}
		if sd.gettable {
			generic.DefClassMethod(&sc, ":"+sd.name, "", getter(sd.name))
		}
		if sd.settable {
			generic.DefClassMethod(&sc, ":set-"+sd.name, "", setter(sd.name))
		}
		sd.defReaderMethods(name)
		sd.defWriterMethods(name)
		sd.defAccessorMethods(name)
	}
	_ = sc.mergeSupers()
	slip.RegisterClass(sc.name, &sc)

	makeClassesReady(slip.CurrentPackage)
	classChanged(&sc, slip.CurrentPackage)

	return &sc
}

func fillMapFromKeyArgs(s *slip.Scope, args slip.List, m map[string]slip.Object, depth int) {
	if len(args)%2 != 0 {
		slip.ErrorPanic(s, depth, "Odd number of &key arguments.")
	}
	for i := 0; i < len(args); i++ {
		sym, ok := args[i].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[i], "symbol")
		}
		key := strings.ToLower(string(sym))
		i++
		m[key] = args[i]
	}
}

type getter string

// Call returns the value of a variable in the instance.
func (g getter) Call(scope *slip.Scope, args slip.List, _ int) slip.Object {
	self := scope.Get("self").(slip.Instance)
	value, _ := self.SlotValue(slip.Symbol(g))

	return value
}

type setter string

// Call returns the value of a variable in the instance.
func (s setter) Call(scope *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 {
		slip.ErrorPanic(scope, depth, "no value given for set-%s.", s)
	}
	self := scope.Get("self").(slip.Instance)
	_ = self.SetSlotValue(slip.Symbol(s), args[0])

	return args[0]
}
