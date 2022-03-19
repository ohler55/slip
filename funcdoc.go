// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

var funcDocs = map[string]*FuncDoc{}

// DocArg describes a function argument.
type DocArg struct {
	Name     string
	Type     string
	Text     string
	Optional bool
}

// FuncDoc describes a function.
type FuncDoc struct {
	Name     string
	Args     []*DocArg
	Return   string // return type
	Text     string
	Examples []string
	HasKeys  bool
	HasRest  bool
}
