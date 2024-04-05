// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"fmt"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	framerFlavor *flavors.Flavor
)

func init() {
	Pkg.Initialize(nil)
	framerFlavor = flavors.DefFlavor("watch-framer",
		map[string]slip.Object{
			"top":  slip.Fixnum(0),
			"left": slip.Fixnum(0),
		},
		[]string{ClientFlavor().Name()},
		slip.List{
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
			slip.Symbol(":settable-instance-variables"),
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A framer that... TBD.`),
			},
		},
		&Pkg,
	)
	framerFlavor.DefMethod(":changed", ":after", framerChangedCaller{})
}

type framerChangedCaller struct{}

func (caller framerChangedCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	var (
		top  int
		left int
	)
	if num, ok := self.Get("top").(slip.Fixnum); ok {
		top = int(num)
	}
	if num, ok := self.Get("left").(slip.Fixnum); ok {
		left = int(num)
	}
	vars := slip.List{
		slip.List{slip.Symbol("Key-1"), slip.Fixnum(5)},
		slip.List{slip.Symbol("Key-2"), slip.Fixnum(8)},
	}
	for i, v := range vars {
		lv, _ := v.(slip.List)
		setCursor(top+i, left)
		fmt.Printf("\x1b[0K%s: %s", lv[0], lv[1])
		setCursor(top+i+1, left)
	}
	return nil
}

func (caller framerChangedCaller) Docs() string {
	return `__:changed__ _symbol_ _value_
   _:symbol_ [symbol] the symbol that changed.
   _:value_ [object] the new value for the symbol.


Responds to a change event received from the watch-server.
`
}

func setCursor(v, h int) {
	if v < 0 {
		v = 0
	}
	if h < 0 {
		h = 0
	}
	_, _ = fmt.Printf("\x1b[%d;%dH", v, h)
}
