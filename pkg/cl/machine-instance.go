// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"net"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MachineInstance{Function: slip.Function{Name: "machine-instance", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "machine-instance",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__machine-instance__ returns a string describing the machine instance.`,
			Examples: []string{
				`(machine-instance) => "f8:4d:89:6b:2e:d5"`,
			},
		}, &slip.CLPkg)
}

// MachineInstance represents the machine-instance function.
type MachineInstance struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MachineInstance) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 0, 0)
	// Need something unique that works across multiple OSs. A Mac Address should do.
	if ints, err := net.Interfaces(); err == nil && 0 < len(ints) {
		for i := 0; i < len(ints); i++ {
			in := &ints[0]
			if in.Flags&net.FlagUp == 0 || len(in.HardwareAddr) == 0 {
				continue
			}
			if in.HardwareAddr[0]&0x02 == 0 { // externally facing address
				result = slip.String(in.HardwareAddr.String())
			}
		}
	}
	return
}
