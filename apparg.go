// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"flag"
	"fmt"
)

// AppArg are used to specify then variables that will be set from the command
// line arguments.
type AppArg struct {
	// Flag is the command line flag such as 'v' which matches '-v' on the
	// command line.
	Flag string

	// Doc is the documentation or description of the flag as it appears in
	// the help display that is triggered a '-h' on the command line.
	Doc string

	// Default is the default SLIP object that will be assigned to the
	// variable associated with the flag unless over-ridden by a command line
	// option.
	Default Object

	// Type is the SLIP type to coerce the command line option value into.
	Type string

	// Var is the name of the variable to that is bound to the flag value.
	Var string

	strValue   string
	intValue   int64
	floatValue float64
	boolValue  bool
}

// SetFlag sets the command line flag option for the argument.
func (aa *AppArg) SetFlag(fs *flag.FlagSet, scope *Scope) {
	switch aa.Type {
	case "fixnum", "integer":
		var dv int64
		if num, ok := aa.Default.(Integer); ok {
			dv = num.Int64()
		}
		fs.Int64Var(&aa.intValue, aa.Flag, dv, aa.Doc)
	case "float", "short-float", "double-float", "single-float":
		var dv float64
		if num, ok := aa.Default.(Real); ok {
			dv = num.RealValue()
		}
		fs.Float64Var(&aa.floatValue, aa.Flag, dv, aa.Doc)
	case "boolean":
		if aa.Default == nil {
			fs.BoolVar(&aa.boolValue, aa.Flag, false, aa.Doc)
		} else {
			fs.BoolVar(&aa.boolValue, aa.Flag, true, aa.Doc)
		}
	case "eval-before":
		fs.Func(aa.Flag, aa.Doc, func(str string) error {
			code := ReadString(str, scope)
			var value Object
			for _, obj := range code {
				value = obj.Eval(scope, 0)
			}
			if 0 < len(aa.Var) {
				scope.Let(Symbol(aa.Var), value)
			}
			return nil
		})
	case "eval-after":
		fs.Func(aa.Flag, aa.Doc, func(str string) error {
			aa.strValue = string(append(append([]byte(aa.strValue), str...), '\n'))
			return nil
		})
	default: // any or blank
		fs.StringVar(&aa.strValue, aa.Flag, ObjectString(aa.Default), aa.Doc)
	}
}

// UpdateScope updates the scope with the values in the app arg.
func (aa *AppArg) UpdateScope(scope *Scope) {
	var (
		value Object
		skip  bool
	)
	switch aa.Type {
	case "fixnum", "integer":
		value = Fixnum(aa.intValue)
	case "float", "short-float", "double-float", "single-float":
		value = DoubleFloat(aa.floatValue)
	case "string":
		value = String(aa.strValue)
	case "symbol":
		value = Symbol(aa.strValue)
	case "octets":
		value = Octets(aa.strValue)
	case "boolean":
		if aa.boolValue {
			value = True
		}
		skip = true
	case "eval-before":
		// already handled
		return
	case "eval-after":
		if 0 < len(aa.strValue) {
			code := ReadString(aa.strValue, scope)
			for _, obj := range code {
				value = obj.Eval(scope, 0)
			}
		}
		skip = true
	default: // any or blank
		if code := ReadString(aa.strValue, scope); 0 < len(code) {
			value = code[0]
		}
	}
	if !skip && 0 < len(aa.Type) {
		value = Coerce(value, Symbol(aa.Type))
	}
	if 0 < len(aa.Var) {
		scope.Let(Symbol(aa.Var), value)
	}
}

// DefaultReadable returns the readable string that reads into the default value.
func (aa *AppArg) DefaultReadable() string {
	if aa.Default == nil {
		return "nil"
	}
	return fmt.Sprintf("%T(%s)", aa.Default, ObjectString(aa.Default))
}
