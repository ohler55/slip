// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"fmt"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var testFlavor *flavors.Flavor

func init() {
	_ = TestFlavor()
}

// TestFlavor returns the test-flavor.
func TestFlavor() *flavors.Flavor {
	_ = TestableFlavor()
	if testFlavor == nil {
		testFlavor = flavors.DefFlavor("test-flavor",
			map[string]slip.Object{ // variables
				"forms":  nil, // once compiled place on self.Any or maybe replace forms variable
				"result": nil, // :passed, :failed, :skipped (same as nil)
			},
			[]string{"testable-flavor"}, // inherited
			slip.List{ // options
				slip.List{
					slip.Symbol(":documentation"),
					slip.String(`A test is a container for a set of forms.`),
				},
				slip.Symbol(":gettable-instance-variables"),
				slip.List{
					slip.Symbol(":inittable-instance-variables"),
					slip.Symbol("name"),
					slip.Symbol("parent"),
					slip.Symbol("forms"),
				},
			},
		)
		testFlavor.DefMethod(":run", "", testRunCaller(true))
		// flavor.DefMethod(":report", "", reportCaller(true))
		// flavor.DefMethod(":results", "", resultsCaller(true))
		// flavor.DefMethod(":reset", "", testResetCaller(true))
		// flavor.DefMethod(":forms", "", formsCaller(true)) // return the original forms
	}
	return testFlavor
}

type testRunCaller bool

func (caller testRunCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	_, verbose, trace := getRunKeys(args)
	if trace {
		slip.Trace(true)
	}
	defer func() {
		// TBD use *standard-output*
		slip.Trace(false)
		self := s.Get("self").(*flavors.Instance)
		name, _ := s.Get("name").(slip.String)
		var indent []byte
		for t, _ := self.Get("parent").(*flavors.Instance); t != nil; t, _ = t.Get("parent").(*flavors.Instance) {
			indent = append(indent, ' ', ' ')
		}
		if rec := recover(); rec != nil {
			s.Set("result", slip.Symbol(":failed"))
			fmt.Printf("%s%s: FAILED\n%s  %s\n", indent, string(name), indent, rec)
			if p, ok := rec.(*slip.Panic); ok && verbose {
				for _, frame := range p.Stack {
					fmt.Printf("%s  %s\n", indent, frame)
				}
			}
		} else if verbose || trace {
			fmt.Printf("%s%s: Passed\n", indent, string(name))
		}
	}()
	if forms, ok := s.Get("forms").(slip.List); ok {
		for i := range forms {
			_ = slip.EvalArg(s, forms, i, depth)
		}
	}
	s.Set("result", slip.Symbol(":passed"))

	return nil
}

func (caller testRunCaller) Docs() string {
	return `__:run__ &key verbose trace
   _:verbose_ if true the test results will be printed.
   _:trace_ if _(trace t)_ will be called before evaluating the test _forms_.


Runs the test.
`
}
