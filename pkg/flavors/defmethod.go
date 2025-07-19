// Copyright (c) 2025, Peter Ohler, All rights reserved.

package flavors

import (
	"strings"

	"github.com/ohler55/slip"
)

// DefMethod adds a method to the class or flavor.
func DefMethod(obj slip.Class, name, daemon string, caller slip.Caller) {
	name = strings.ToLower(name)
	hm, ok := obj.(HasMethods)
	if !ok {
		slip.PanicInvalidMethod(obj, slip.Symbol(daemon), slip.Symbol(name),
			"Can not define a direct method, %s on class %s.", name, obj.Name())
	}
	mm := hm.Methods()
	var (
		addMethod bool
		addCombo  bool
	)
	m := mm[name]
	if m == nil {
		m = &slip.Method{Name: name, Doc: &slip.FuncDoc{Name: name, Kind: slip.MethodSymbol}}
		addMethod = true
	}
	// Override existing method documentation if provided by the caller.
	switch tc := caller.(type) {
	case slip.HasFuncDocs:
		if fd := tc.FuncDocs(); fd != nil && 0 < len(fd.Text) {
			fd.Kind = slip.MethodSymbol
			if !addMethod && name != ":init" {
				m.CompareArgs(fd)
			}
			m.Doc = fd
		}
	case HasDocs:
		if text := tc.Docs(); 0 < len(text) {
			m.Doc = &slip.FuncDoc{Name: name, Text: text, Kind: slip.MethodSymbol}
		}
	}
	// If there is a combination for this class it will be the first on the
	// list.
	var c *slip.Combination
	if 0 < len(m.Combinations) && m.Combinations[0].From == obj {
		c = m.Combinations[0]
	} else {
		addCombo = true
		c = &slip.Combination{From: obj}
	}
	daemon = strings.ToLower(daemon)
	switch daemon {
	case ":primary", "":
		c.Primary = caller
	case ":before":
		c.Before = caller
	case ":after":
		c.After = caller
	case ":whopper", ":wrapper":
		c.Wrap = caller
	default:
		slip.PanicInvalidMethod(obj, slip.Symbol(daemon), slip.Symbol(name), "")
	}
	if addMethod {
		mm[name] = m
	}
	if addCombo {
		m.Combinations = append([]*slip.Combination{c}, m.Combinations...)
	}
	if addCombo {
		// If there are supers that inherit from this flavor then insert
		// the new method into the method combinations.
		for _, ac := range slip.CurrentPackage.AllClasses() {
			if ac.Inherits(obj) {
				insertMethod(ac, obj, m, c)
			}
		}
	}
}

func insertMethod(class, super slip.Class, method *slip.Method, combo *slip.Combination) {
	var mm map[string]*slip.Method
	if hm, ok := class.(HasMethods); ok {
		mm = hm.Methods()
	}
	m := mm[method.Name]
	if m == nil {
		m = &slip.Method{
			Name:         method.Name,
			Doc:          method.Doc,
			Combinations: []*slip.Combination{combo},
		}
		mm[method.Name] = m
		return
	}
	var pos int
	if pos < len(m.Combinations) && m.Combinations[pos].From == class {
		pos++
	}
	for _, f := range class.InheritsList() {
		if len(m.Combinations) <= pos || m.Combinations[pos].From == super {
			break
		}
		if m.Combinations[pos].From == f {
			pos++
		}
	}
	m.Combinations = append(append(m.Combinations[:pos], combo), m.Combinations[pos:]...)
}
