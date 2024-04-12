// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/gi"
)

var (
	channelerFlavor *flavors.Flavor
)

// ChannelerFlavor returns the channeler flavor.
func ChannelerFlavor() *flavors.Flavor {
	channelerFlavor = flavors.DefFlavor("watch-channeler",
		map[string]slip.Object{"channel": nil},
		[]string{ClientFlavor().Name()},
		slip.List{
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
			slip.Symbol(":settable-instance-variables"),
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A channeler is a client that places change notification to a channel.
A list of the symbol change and the new value is placed on the channel.`),
			},
		},
		&Pkg,
	)
	channelerFlavor.DefMethod(":changed", ":after", channelerChangedCaller{})

	return channelerFlavor
}

type channelerChangedCaller struct{}

func (caller channelerChangedCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if channel, _ := self.Get("channel").(gi.Channel); channel != nil {
		channel <- args
	}
	return nil
}

func (caller channelerChangedCaller) Docs() string {
	return `__:changed__ _symbol_ _value_
   _:symbol_ [symbol] the symbol that changed.
   _:value_ [object] the new value for the symbol.


Responds to a change event received from the watch-server.
`
}
