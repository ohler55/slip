// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
)

// NameServiceErrorSymbol is the symbol with a value of "NameServiceError".
const NameServiceErrorSymbol = slip.Symbol("name-service-error")

var nameServiceErrorHierarchy = []slip.Symbol{
	NameServiceErrorSymbol,
	slip.ErrorSymbol,
	slip.SeriousConditionSymbol,
	slip.ConditionSymbol,
	slip.TrueSymbol,
}

func defNameServiceError() {
	_ = clos.DefClass("name-service-error",
		"A name service error",
		map[string]slip.Object{"message": nil}, // slots
		[]*clos.Class{clos.Find("error")},      // supers
		true)

	slip.RegisterCondition("name-service-error", makeNameServiceError)

}

type nameServiceError struct {
	slip.Panic
}

func makeNameServiceError(args slip.List) slip.Condition {
	c := nameServiceError{} // TBD hierarchy
	c.SetHierarchy(nameServiceErrorHierarchy)
	kv := slip.ParseInitList(args)
	if val, has := kv[":message"]; has {
		if msg, ok := val.(slip.String); ok {
			c.Message = string(msg)
		}
	}
	return &c
}
