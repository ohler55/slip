// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"strings"

	"github.com/ohler55/slip"
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
	slip.ArgCountCheck(f, args, 3, 6)
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("class-name", args[0], "symbol")
	}
	var (
		supers    slip.List
		slotSpecs slip.List
	)
	if supers, ok = args[1].(slip.List); !ok {
		slip.PanicType("superclass-names", args[1], "list")
	}
	if slotSpecs, ok = args[2].(slip.List); !ok {
		slip.PanicType("slot-specifiers", args[2], "list")
	}
	if c := slip.FindClass(string(name)); c != nil {
		if sc, ok := c.(*StandardClass); ok && sc.Final {
			slip.NewPanic("Can not redefine class %s.", name)
		}
	}
	return DefStandardClass(string(name), supers, slotSpecs, args[3:])
}

// DefStandardClass defines a standard-class.
func DefStandardClass(name string, supers, slotSpecs, classOptions slip.List) *StandardClass {
	sc := StandardClass{
		name:            name,
		defname:         "defclass",
		slotDefs:        map[string]*SlotDef{},
		pkg:             slip.CurrentPackage,
		supers:          make([]slip.Symbol, len(supers)),
		defaultInitArgs: map[string]slip.Object{},
		initArgs:        map[string]*SlotDef{},
		initForms:       map[string]*SlotDef{},
		inheritCheck: func(c slip.Class) *StandardClass {
			sc, ok := c.(*StandardClass)
			if !ok && c != nil {
				slip.PanicType("superclass", c, "standard-class")
			}
			return sc
		},
		baseClass: StandardObjectSymbol,
	}
	for i, super := range supers {
		if sym, ok := super.(slip.Symbol); ok {
			sc.supers[i] = sym
		} else {
			slip.PanicType("super", super, "symbol")
		}
	}
	sc.Init(false)

	for _, opt := range classOptions {
		list, ok := opt.(slip.List)
		if !ok || len(list) < 2 {
			slip.PanicType("class-options", opt, "list")
		}
		switch list[0] {
		case slip.Symbol(":documentation"):
			if docs, ok := list[1].(slip.String); ok {
				sc.docs = string(docs)
			} else {
				slip.PanicType("class-options :documentation", list[1], "string")
			}
		case slip.Symbol(":default-initargs"):
			fillMapFromKeyArgs(list[1:], sc.defaultInitArgs)
		case slip.Symbol(":metaclass"):
			// ignore
		default:
			slip.PanicType("class-options directive", list[0], ":documentation", "default-initargs", ":metaclass")
		}
	}
	for _, ss := range slotSpecs {
		sd := NewSlotDef(ss)
		sd.class = &sc
		sc.slotDefs[sd.name] = sd
		if sd.classStore {
			sc.vars[sd.name] = sd.initform
		}
	}
	_ = sc.mergeSupers()
	slip.RegisterClass(sc.name, &sc)

	makeClassesReady(slip.CurrentPackage)
	classChanged(&sc, slip.CurrentPackage)

	return &sc
}

func fillMapFromKeyArgs(args slip.List, m map[string]slip.Object) {
	if len(args)%2 != 0 {
		slip.NewPanic("Odd number of &key arguments.")
	}
	for i := 0; i < len(args); i++ {
		sym, ok := args[i].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[i], "symbol")
		}
		key := strings.ToLower(string(sym))
		i++
		m[key] = args[i]
	}
}
