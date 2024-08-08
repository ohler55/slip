// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := OctetsToString{Function: slip.Function{Name: "octets-to-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "octets-to-string",
			Args: []*slip.DocArg{
				{
					Name: "octets",
					Type: "octets|list",
					Text: "octets or a list of objects that can be coerced into an octet",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "the start of the _octets_ to convert to a string. default: 0",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "the end of the _octets_ to convert to a string. default: nil (end of the octets)",
				},
			},
			Return: "string",
			Text:   `__octets-to-string__ converts _octets_ to a string`,
			Examples: []string{
				`(octets-to-string '(84 69 83 #\T 32 115 116 114 105 110 103) => "TEST string")`,
				`(octets-to-string (coerce '(84 69 83 84) 'octets)) => "TEST")`,
			},
		}, &Pkg)
}

// OctetsToString represents the octets-to-string function.
type OctetsToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *OctetsToString) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 5)
	start := 0
	end := -1
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":start")); has {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num {
			start = int(num)
		} else {
			slip.PanicType(":start", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":end")); has && v != nil {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num {
			end = int(num)
		} else {
			slip.PanicType(":start", v, "fixnum")
		}
	}
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case nil:
		result = slip.String("")
	case slip.Octets:
		end = f.checkStartEnd(start, end, len(ta))
		result = slip.String(ta[start:end])
	case slip.List:
		end = f.checkStartEnd(start, end, len(ta))
		octs := make(slip.Octets, end-start)
		for i, v := range ta[start:end] {
			octs[i] = byte(cl.ToOctet(v).(slip.Octet))
		}
		result = slip.String(octs)
	case *slip.Vector:
		a0 = ta.AsList()
		goto top
	default:
		slip.PanicType("octets", args[0], "list", "vector", "octets")
	}
	return
}

func (f *OctetsToString) checkStartEnd(start, end, size int) int {
	if end < 0 {
		end = size
	}
	if size < start {
		slip.NewPanic("start, %d is greater than length of %d", start, size)
	}
	if size < end {
		slip.NewPanic("end, %d is greater than length of %d", end, size)
	}
	if end < start {
		slip.NewPanic("end, %d is less start %d", end, start)
	}
	return end
}
