// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defDefineCondition() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DefineCondition{Function: slip.Function{Name: "define-condition", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "define-condition",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the condition class to define.",
				},
				{
					Name: "parent-types",
					Type: "list",
					Text: `A list of parent type names where each member of the list is a symbol.`,
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
  __:documentation__ [string] documentation for the slot. This is an addition to the common-lisp specification.
`,
				},
				{Name: slip.AmpRest},
				{
					Name: "options",
					Type: "list",
					Text: `Each option must be a list with the first element being one of the
following keywords:
  __:documentation__ followed by a strings becomes the documentation for the class.
  __:default-initargs__ followed by alternating initarg names and forms that are evaluated on each call.
  __:report__ followed by a function name or lambda where the function will be called with two arguments,
the condition and a stream. The function is called to report the condition when __print-object__ is
called.
`,
				},
			},
			Return: "condition-class",
			Text: `__define-condition__ defines a new condition class and returns the new class. If
the named class already exists it is over-written but existing objects continue to reference the
original class.`,
			Examples: []string{
				"(define-condition my-error (error) (message)) => #<condition-class my-error>",
			},
		}, &Pkg)
}

// DefineCondition represents the define-condition function.
type DefineCondition struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *DefineCondition) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 6)
	name, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("name", args[0], "symbol")
	}
	var (
		supers    slip.List
		slotSpecs slip.List
	)
	if supers, ok = args[1].(slip.List); !ok {
		slip.PanicType("parent-types", args[1], "list")
	}
	if slotSpecs, ok = args[2].(slip.List); !ok {
		slip.PanicType("slot-specifiers", args[2], "list")
	}
	return DefConditionClass(string(name), supers, slotSpecs, args[3:])
}

// DefConditionClass defines a standard-class.
func DefConditionClass(name string, supers, slotSpecs, classOptions slip.List) *ConditionClass {
	cc := ConditionClass{
		StandardClass: StandardClass{
			name:            name,
			defname:         "define-condition",
			slotDefs:        map[string]*SlotDef{},
			pkg:             slip.CurrentPackage,
			supers:          make([]slip.Symbol, len(supers)),
			defaultInitArgs: map[string]slip.Object{},
			initArgs:        map[string]*SlotDef{},
			initForms:       map[string]*SlotDef{},
			inheritCheck: func(c slip.Class) *StandardClass {
				if c == nil {
					return nil
				}
				cc, ok := c.(*ConditionClass)
				if !ok {
					slip.PanicType("parent-type", c, "condition-class")
				}
				return &cc.StandardClass
			},
		},
	}
	for i, super := range supers {
		if sym, ok := super.(slip.Symbol); ok {
			cc.supers[i] = sym
		} else {
			slip.PanicType("super", super, "symbol")
		}
	}
	cc.Vars = map[string]slip.Object{}
	cc.locker = slip.NoOpLocker{}

	for _, opt := range classOptions {
		list, ok := opt.(slip.List)
		if !ok || len(list) < 2 {
			slip.PanicType("options", opt, "list")
		}
		switch list[0] {
		case slip.Symbol(":documentation"):
			if docs, ok := list[1].(slip.String); ok {
				cc.docs = string(docs)
			} else {
				slip.PanicType("options :documentation", list[1], "string")
			}
		case slip.Symbol(":default-initargs"):
			fillMapFromKeyArgs(list[1:], cc.defaultInitArgs)
		case slip.Symbol(":report"):
			if list[1] != nil {
				cc.Vars["report"] = list[1].Eval(slip.NewScope(), 0)
			}
		default:
			slip.PanicType("options directive", list[0], ":documentation", "default-initargs", ":report")
		}
	}
	for _, ss := range slotSpecs {
		sd := NewSlotDef(ss)
		sd.class = &cc
		cc.slotDefs[sd.name] = sd
		if sd.classStore {
			cc.Vars[sd.name] = sd.initform
		}
	}
	_ = cc.mergeSupers()
	slip.RegisterClass(cc.name, &cc)

	makeClassesReady(slip.CurrentPackage)
	classChanged(&cc, slip.CurrentPackage)

	return &cc
}
