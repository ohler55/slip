// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleFormatter is the interface for all simple conditions and error.
type SimpleFormatter interface {
	error

	// Control return the format-control string for the instance.
	Control() string

	// Arguments return the format-arguments for the instance.
	Arguments() slip.List
}

// SimpleFormatterEmbed add the fields and functions to support the
// SimpleFormatter interface.
type SimpleFormatterEmbed struct {
	ctrl   string
	args   slip.List
	output string
}

// Control return the control string for the instance.
func (sf *SimpleFormatterEmbed) Control() string {
	return sf.ctrl
}

// Arguments return the format-arguments for the instance.
func (sf *SimpleFormatterEmbed) Arguments() slip.List {
	return sf.args
}

func (sf *SimpleFormatterEmbed) formOutput(s *slip.Scope) {
	ctrl := control{scope: s, str: []byte(sf.ctrl), args: sf.args, argPos: 0}
	ctrl.end = len(ctrl.str)
	ctrl.process()
	sf.output = string(ctrl.out)
}
