// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	printerFlavor *flavors.Flavor
)

// PrinterFlavor returns the printer flavor.
func PrinterFlavor() *flavors.Flavor {
	Pkg.Initialize(nil)
	printerFlavor = flavors.DefFlavor("watch-printer", map[string]slip.Object{},
		[]string{ClientFlavor().Name()},
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A printer is a watch client that prints change notifications to _*standard-output*.`),
			},
		},
		&Pkg,
	)
	printerFlavor.DefMethod(":changed", ":after", printerChangedCaller{})

	return printerFlavor
}

type printerChangedCaller struct{}

func (caller printerChangedCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	w := s.Get("*standard-output*").(io.Writer)
	_, _ = fmt.Fprintf(w, "%s: %s\n", args[0], args[1])
	return nil
}
