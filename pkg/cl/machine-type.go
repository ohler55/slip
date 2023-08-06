// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"os/exec"

	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MachineType{Function: slip.Function{Name: "machine-type", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "machine-type",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__machine-type__ returns a string describing the machine type.`,
			Examples: []string{
				`(machine-type) => "MacBook Pro"`,
			},
		}, &slip.CLPkg)
}

// MachineType represents the machine-type function.
type MachineType struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MachineType) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 0, 0)
	// Assume MacOS and try system_profiler. If that fails assume linux and check /proc.
	out, err := exec.Command("system_profiler", "-json", "SPHardwareDataType").Output()
	if err == nil {
		var js any
		if js, err = oj.Parse(out); err == nil {
			result = slip.String(alt.String(jp.C("SPHardwareDataType").N(0).C("machine_name").First(js)))
		}
	}
	return
}
