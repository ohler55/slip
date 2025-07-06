// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

// SlotDef encapsulates the definition of a slot on a class.
type SlotDef struct {
	name       string
	initArgs   []slip.Symbol
	readers    []slip.Symbol
	writers    []slip.Symbol
	accessors  []slip.Symbol
	initForm   slip.Object // TBD this should be evaluated on each make-instance
	argType    slip.Symbol
	docs       string
	classStore bool
}

func (sd *SlotDef) Simplify() any {
	return map[string]any{
		"name":      sd.name,
		"initArgs":  sd.initArgs,
		"type":      sd.argType,
		"initForm":  slip.SimpleObject(sd.initForm),
		"docs":      sd.docs,
		"intArgs":   simplifySymList(sd.initArgs),
		"readers":   simplifySymList(sd.readers),
		"writers":   simplifySymList(sd.writers),
		"accessors": simplifySymList(sd.accessors),
	}
}

func simplifySymList(symList []slip.Symbol) []string {
	sa := make([]string, len(symList))
	for i, sym := range symList {
		sa[i] = string(sym)
	}
	return sa
}
