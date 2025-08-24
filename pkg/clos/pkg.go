// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

const (
	bold         = "\x1b[1m"
	colorOff     = "\x1b[m"
	indentSpaces = "                                                                                "
)

var (
	// Pkg is the Class package.
	Pkg = slip.Package{
		Name:      "clos",
		Nicknames: []string{"clos"},
		Doc:       "Home of symbols defined for the CLOS functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*clos*": {
				Val:    &Pkg,
				Doc:    Pkg.Doc,
				Const:  true,
				Export: true,
			},
		},
		&ClassName{},
	)
	defBuiltIns()
	defAllocateInstance()
	defChangeClass()
	defClassName()
	defClassOf()
	defDefclass()
	defDefineCondition()
	defFindClass()
	defMakeInstance()
	defSlotExistsp()
	defSlotMakunbound()
	defSlotBoundp()
	defSlotValue()
	defClassSupers()
	defClassPrecedence()
	defClassMetaclass()
	defListAllClasses()
	defWithSlots()
	defSharedInitialize()
	defInitializeInstance()

	defConditions()

	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}

func slotMissing(s *slip.Scope, obj slip.Object, name slip.Symbol, op string, depth int) slip.Object {
	fi := slip.FindFunc("slot-missing")
	panicFormat := "When attempting to read the slot's value (%s), the slot %s is missing from the object %s."
	if op == "setf" {
		panicFormat = "When attempting to set the slot's value (%s), the slot %s is missing from the object %s."
	}
	if fi == nil || obj == nil {
		slip.CellPanic(s, depth, name, panicFormat, op, name, obj)
	}
	class := slip.FindClass(string(obj.Hierarchy()[0]))
	if class.Metaclass() == slip.Symbol("built-in-class") {
		slip.CellPanic(s, depth, name, panicFormat, op, name, obj)
	}
	args := slip.List{
		slip.FindClass(string(obj.Hierarchy()[0])),
		obj,
		name,
		slip.Symbol(op),
	}
	f, _ := fi.Create(args).(slip.Funky)

	return f.Caller().Call(s, args, 0)
}
