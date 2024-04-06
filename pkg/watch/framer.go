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
	framerFlavor.DefMethod(":init", ":after", framerInitCaller{})
}

type framerInitCaller struct{}

func (caller framerInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	c := self.Any.(*client)
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
	for i := 0; i < len(c.vars); i++ {
		setCursor(top+i, left)
		fmt.Print("\x1b[0K")
	}
	return nil
}

func (caller framerInitCaller) Docs() string {
	return clientInitCaller{}.Docs()
}

type framerChangedCaller struct{}

func (caller framerChangedCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	c := self.Any.(*client)
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
	// TBD just update the one that changed
	for i, v := range c.vars {
		setCursor(top+i, left)
		fmt.Printf("\x1b[0K%s: %s", v.sym, slip.ObjectString(v.val))
		setCursor(top+i+1, left)
	}
	return nil
}

func (caller framerChangedCaller) Docs() string {
	return clientChangedCaller{}.Docs()
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
