// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

// SlotDef encapsulates the definition of a slot on a class.
type SlotDef struct {
	name       string
	initargs   []slip.Symbol
	readers    []slip.Symbol
	writers    []slip.Symbol
	accessors  []slip.Symbol
	initform   slip.Object // TBD this should be evaluated on each make-instance
	argType    slip.Symbol
	docs       string
	classStore bool
}

// NewSlotDef creates a new SlotDef from a slot-specification provided to
// defclass.
func NewSlotDef(def slip.Object) *SlotDef {
	var sd SlotDef
	switch td := def.(type) {
	case slip.Symbol:
		sd.name = string(td)
	case slip.List:
		// If length is less than 1 then it would be nil and not a list.
		if sym, ok := td[0].(slip.Symbol); ok {
			sd.name = string(sym)
		} else {
			slip.PanicType("slot-name", td[0], "symbol")
		}
		if len(td)%2 != 1 { // not an even number of pairs plus then slot-name
			slip.NewPanic("slot-specification for %s must have a slot-name followed by pairs of key and value.",
				sd.name)
		}
		for i := 1; i < len(td); i++ {
			switch td[i] {
			case slip.Symbol(":reader"):
				sd.readers = appendSymbol(":reader", sd.readers, td[i+1])
			case slip.Symbol(":writer"):
				sd.writers = appendSymbol(":writer", sd.writers, td[i+1])
			case slip.Symbol(":accessor"):
				sd.accessors = appendSymbol(":accessor", sd.accessors, td[i+1])
			case slip.Symbol(":initarg"):
				sd.initargs = appendSymbol(":initarg", sd.initargs, td[i+1])
			case slip.Symbol(":allocation"):
				switch td[i+1] {
				case slip.Symbol(":instance"):
					// the default, nothing to do
				case slip.Symbol(":class"):
					sd.classStore = true
				default:
					slip.PanicType(":allocation", td[i+1], ":instance", ":class")
				}
			case slip.Symbol(":initform"):
				if sd.initform != nil {
					slip.NewPanic("For slot %s, the :initform option can only be specified once.", sd.name)
				}
				sd.initform = td[i+1]
			case slip.Symbol(":type"):
				if 0 < len(sd.argType) {
					slip.NewPanic("For slot %s, the :type option can only be specified once.", sd.name)
				}
				if sym, ok := td[i+1].(slip.Symbol); ok {
					sd.argType = sym
				} else {
					slip.PanicType(":type", td[i+1], "symbol") // TBD allow more complex types
				}
			case slip.Symbol(":documentation"):
				if 0 < len(sd.docs) {
					slip.NewPanic("For slot %s, the :documentation option can only be specified once.", sd.name)
				}
				if doc, ok := td[i+1].(slip.String); ok {
					sd.docs = string(doc)
				} else {
					slip.PanicType(":documentation", td[i+1], "string")
				}
			default:
				slip.PanicType("slot-option", td[i], ":reader", ":writer", ":accessor",
					":allocation", ":initarg", ":initform", ":type", ":documentation")
			}
		}
	default:
		slip.PanicType("slot-specification", td, "symbol", "list")
	}
	return &sd
}

func (sd *SlotDef) Simplify() any {
	alloc := "instance"
	if sd.classStore {
		alloc = "class"
	}
	return map[string]any{
		"name":       sd.name,
		"type":       sd.argType,
		"initform":   slip.SimpleObject(sd.initform),
		"docs":       sd.docs,
		"intargs":    simplifySymList(sd.initargs),
		"readers":    simplifySymList(sd.readers),
		"writers":    simplifySymList(sd.writers),
		"accessors":  simplifySymList(sd.accessors),
		"allocation": alloc,
	}
}

func appendSymbol(option string, sa []slip.Symbol, v slip.Object) []slip.Symbol {
	sym, ok := v.(slip.Symbol)
	if !ok {
		slip.PanicType(option, v, "symbol")
	}
	return append(sa, sym)
}

func simplifySymList(symList []slip.Symbol) []string {
	sa := make([]string, len(symList))
	for i, sym := range symList {
		sa[i] = string(sym)
	}
	return sa
}
