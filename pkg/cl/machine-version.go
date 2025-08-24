// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"os/exec"

	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MachineVersion{Function: slip.Function{Name: "machine-version", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "machine-version",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__machine-version__ returns a string describing the machine version.`,
			Examples: []string{
				`(machine-version) => "MacBookPro18,4 Z15H00109LL/A"`,
			},
		}, &slip.CLPkg)
}

// MachineVersion represents the machine-version function.
type MachineVersion struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MachineVersion) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 0, 0)
	// Assume MacOS and try system_profiler. If that fails assume linux and check /proc.
	out, err := exec.Command("system_profiler", "-json", "SPHardwareDataType").Output()
	if err == nil {
		var js any
		if js, err = oj.Parse(out); err == nil {
			result = slip.String(fmt.Sprintf("%s %s",
				alt.String(jp.C("SPHardwareDataType").N(0).C("machine_model").First(js)),
				alt.String(jp.C("SPHardwareDataType").N(0).C("model_number").First(js))))
		}
	}
	return
}
