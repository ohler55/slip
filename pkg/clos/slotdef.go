// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"fmt"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/generic"
)

// SlotDef encapsulates the definition of a slot on a class.
type SlotDef struct {
	name       string
	class      slip.Class
	initargs   []slip.Symbol
	readers    []slip.Symbol
	writers    []slip.Symbol
	accessors  []slip.Symbol
	initform   slip.Object
	argType    slip.Object
	docs       string
	classStore bool
	gettable   bool
	settable   bool
}

// NewSlotDef creates a new SlotDef from a slot-specification provided to
// defclass.
func NewSlotDef(s *slip.Scope, def slip.Object, depth int) *SlotDef {
	sd := SlotDef{initform: slip.Unbound}
	switch td := def.(type) {
	case slip.Symbol:
		sd.name = string(td)
	case slip.List:
		// If length is less than 1 then it would be nil and not a list.
		if sym, ok := td[0].(slip.Symbol); ok {
			sd.name = string(sym)
		} else {
			slip.TypePanic(s, depth, "slot-name", td[0], "symbol")
		}
		if len(td)%2 != 1 { // not an even number of pairs plus then slot-name
			slip.NewPanic("slot-specification for %s must have a slot-name followed by pairs of key and value.",
				sd.name)
		}
		for i := 1; i < len(td); i += 2 {
			switch td[i] {
			case slip.Symbol(":reader"):
				sd.readers = appendSymbol(s, depth, ":reader", sd.readers, td[i+1])
			case slip.Symbol(":writer"):
				sd.writers = appendSymbol(s, depth, ":writer", sd.writers, td[i+1])
			case slip.Symbol(":accessor"):
				sd.accessors = appendSymbol(s, depth, ":accessor", sd.accessors, td[i+1])
			case slip.Symbol(":initarg"):
				sd.initargs = appendSymbol(s, depth, ":initarg", sd.initargs, td[i+1])
			case slip.Symbol(":allocation"):
				switch td[i+1] {
				case slip.Symbol(":instance"):
					// the default, nothing to do
				case slip.Symbol(":class"):
					sd.classStore = true
				default:
					slip.TypePanic(s, depth, ":allocation", td[i+1], ":instance", ":class")
				}
			case slip.Symbol(":initform"):
				if sd.initform != slip.Unbound {
					slip.NewPanic("For slot %s, the :initform option can only be specified once.", sd.name)
				}
				sd.initform = td[i+1]
			case slip.Symbol(":type"):
				if sd.argType != nil {
					slip.NewPanic("For slot %s, the :type option can only be specified once.", sd.name)
				}
				if sym, ok := td[i+1].(slip.Symbol); ok {
					sd.argType = sym
				} else {
					slip.TypePanic(s, depth, ":type", td[i+1], "symbol") // TBD allow more complex types
				}
			case slip.Symbol(":documentation"):
				if 0 < len(sd.docs) {
					slip.NewPanic("For slot %s, the :documentation option can only be specified once.", sd.name)
				}
				if doc, ok := td[i+1].(slip.String); ok {
					sd.docs = string(doc)
				} else {
					slip.TypePanic(s, depth, ":documentation", td[i+1], "string")
				}
			case slip.Symbol(":gettable"):
				sd.gettable = td[i+1] != nil
			case slip.Symbol(":settable"):
				sd.settable = td[i+1] != nil
			default:
				slip.TypePanic(s, depth, "slot-option", td[i], ":reader", ":writer", ":accessor",
					":allocation", ":initarg", ":initform", ":type", ":documentation")
			}
		}
	default:
		slip.TypePanic(s, depth, "slot-specification", td, "symbol", "list")
	}
	return &sd
}

// LoadForm returns a list as that could be used in a defclass
// slot-specifications.
func (sd *SlotDef) LoadForm() slip.Object {
	def := slip.List{slip.Symbol(sd.name)}
	for _, sym := range sd.initargs {
		def = append(def, slip.Symbol(":initarg"), sym)
	}
	for _, sym := range sd.readers {
		def = append(def, slip.Symbol(":readers"), sym)
	}
	for _, sym := range sd.writers {
		def = append(def, slip.Symbol(":writers"), sym)
	}
	for _, sym := range sd.accessors {
		def = append(def, slip.Symbol(":accessors"), sym)
	}
	if 0 < len(sd.docs) {
		def = append(def, slip.Symbol(":documentation"), slip.String(sd.docs))
	}
	if sd.classStore {
		def = append(def, slip.Symbol(":allocation"), slip.Symbol(":class"))
	}
	if sd.initform != slip.Unbound {
		def = append(def, slip.Symbol(":initform"), sd.initform)
	}
	if sd.argType != nil {
		def = append(def, slip.Symbol(":type"), sd.argType)
	}
	if len(def) == 1 {
		return def[0]
	}
	return def
}

func (sd *SlotDef) defReaderMethods(cname string) {
	for _, sym := range sd.readers {
		_ = generic.DefCallerMethod(
			"",
			readSlot(sd.name),
			&slip.FuncDoc{
				Name: string(sym),
				Args: []*slip.DocArg{
					{Name: "object", Type: cname},
				},
				Return: "object",
				Text:   fmt.Sprintf("Return the %s slot value.", sd.name),
				Kind:   slip.MethodSymbol,
			})
	}
}

type readSlot string

// Call returns the value of a variable in the instance.
func (rs readSlot) Call(_ *slip.Scope, args slip.List, _ int) (value slip.Object) {
	if inst, _ := args[0].(slip.Instance); inst != nil {
		value, _ = inst.SlotValue(slip.Symbol(rs))
	}
	return
}

func (sd *SlotDef) defWriterMethods(cname string) {
	for _, sym := range sd.writers {
		_ = generic.DefCallerMethod(
			"",
			writeSlot(sd.name),
			&slip.FuncDoc{
				Name: string(sym),
				Args: []*slip.DocArg{
					{Name: "object", Type: cname},
					{Name: "value", Type: "t"},
				},
				Return: "object",
				Text:   fmt.Sprintf("Sets the value of slot %s with the provided value.", sd.name),
				Kind:   slip.MethodSymbol,
			})
	}
}

type writeSlot string

// Call returns the value of a variable in the instance.
func (ws writeSlot) Call(_ *slip.Scope, args slip.List, _ int) (value slip.Object) {
	if inst, _ := args[0].(slip.Instance); inst != nil {
		_ = inst.SetSlotValue(slip.Symbol(ws), args[1])
	}
	return args[1]
}

func (sd *SlotDef) defAccessorMethods(cname string) {
	for _, sym := range sd.accessors {
		_ = generic.DefCallerMethod(
			"",
			readSlot(sd.name),
			&slip.FuncDoc{
				Name: string(sym),
				Args: []*slip.DocArg{
					{Name: "object", Type: cname},
				},
				Return: "object",
				Text:   fmt.Sprintf("Return the %s slot value.", sd.name),
				Kind:   slip.MethodSymbol,
			})
		_ = generic.DefCallerMethod(
			"",
			writeSlot(sd.name),
			&slip.FuncDoc{
				Name: fmt.Sprintf("(setf %s)", sym),
				Args: []*slip.DocArg{
					{Name: "object", Type: cname},
					{Name: "value", Type: "t"},
				},
				Return: "object",
				Text:   fmt.Sprintf("Sets the value of slot %s with the provided value.", sd.name),
				Kind:   slip.MethodSymbol,
			})
	}
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

func (sd *SlotDef) Describe(b []byte, class slip.Class, indent, right int, ansi bool) []byte {
	b = append(b, sd.name...)
	if sd.class != nil && class.Name() != sd.class.Name() {
		b = append(b, ' ', '(')
		b = append(b, sd.class.Name()...)
		b = append(b, ')')
	}
	b = append(b, '\n')
	i2 := indent + 2
	if 0 < len(sd.initargs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "initargs:"...)
		for _, sym := range sd.initargs {
			b = append(b, ' ')
			b = append(b, sym...)
		}
		b = append(b, '\n')
	}
	if sd.initform != slip.Unbound {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "initform: "...)
		b = slip.Append(b, sd.initform)
		b = append(b, '\n')
	}
	if 0 < len(sd.readers) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "readers:"...)
		for _, sym := range sd.readers {
			b = append(b, ' ')
			b = append(b, sym...)
		}
		b = append(b, '\n')
	}
	if 0 < len(sd.writers) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "writers:"...)
		for _, sym := range sd.writers {
			b = append(b, ' ')
			b = append(b, sym...)
		}
		b = append(b, '\n')
	}
	if 0 < len(sd.accessors) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "accessors:"...)
		for _, sym := range sd.accessors {
			b = append(b, ' ')
			b = append(b, sym...)
		}
		b = append(b, '\n')
	}
	if 0 < len(sd.docs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "documentation: "...)
		b = slip.AppendDoc(b, sd.docs, 21, right, ansi, 0)
		b = append(b, '\n')
	}
	if sd.classStore {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "allocation: class\n"...)
	}
	if sd.argType != nil {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "type: "...)
		b = slip.Append(b, sd.argType)
		b = append(b, '\n')
	}
	return b
}

func appendSymbol(s *slip.Scope, depth int, option string, sa []slip.Symbol, v slip.Object) []slip.Symbol {
	sym, ok := v.(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, option, v, "symbol")
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
