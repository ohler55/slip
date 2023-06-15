// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

const (
	ansiBold   = "\x1b[1m"
	ansiNormal = "\x1b[m"
	ansiRed    = "\x1b[0;31m"
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
				"result": nil, // :pass, :fail, nil
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
		testFlavor.DefMethod(":report", "", testReportCaller(true))
		testFlavor.DefMethod(":reset", "", testResetCaller(true))
	}
	return testFlavor
}

type testRunCaller bool

func (caller testRunCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	_, verbose, trace := getRunKeys(args)
	if trace {
		slip.Trace(true)
	}
	self := s.Get("self").(*flavors.Instance)
	name, _ := s.Get("name").(slip.String)
	defer func() {
		s.Set("*current-test*", nil)
		w, _ := s.Get("*standard-output*").(io.Writer)
		slip.Trace(false)
		var (
			indent []byte
			bold   = ""
			normal = ""
			red    = ""
		)
		if s.Get("*print-ansi*") != nil {
			bold = ansiBold
			normal = ansiNormal
			red = ansiRed
		}
		for t, _ := self.Get("parent").(*flavors.Instance); t != nil; t, _ = t.Get("parent").(*flavors.Instance) {
			indent = append(indent, ' ', ' ')
		}
		if rec := recover(); rec != nil {
			s.Set("result", slip.Symbol(":fail"))
			fmt.Fprintf(w, "%s%s: %sFAIL%s\n%s  %s%s%s\n",
				indent, string(name), bold, normal, indent, red, rec, normal)
			if p, ok := rec.(*slip.Panic); ok && verbose {
				for _, frame := range p.Stack {
					fmt.Fprintf(w, "%s  %s%s%s\n", indent, red, frame, normal)
				}
			}
		} else if verbose || trace {
			fmt.Fprintf(w, "%s%s: %sPASS%s\n", indent, string(name), bold, normal)
		}
	}()
	if forms, ok := s.Get("forms").(slip.List); ok {
		s.Let("*current-test*", self)
		for i := range forms {
			_ = slip.EvalArg(s, forms, i, depth)
		}
	}
	s.Set("result", slip.Symbol(":pass"))

	return nil
}

func (caller testRunCaller) Docs() string {
	return `__:run__ &key verbose trace
   _:verbose_ if true the test results will be printed.
   _:trace_ if true _(trace t)_ will be called before evaluating the test _forms_.


Runs the test. The _*current-test*_ variable will be set to the test instance
for the duration of the run.
`
}

type testResetCaller bool

func (caller testResetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	s.Set("result", nil)
	return nil
}

func (caller testResetCaller) Docs() string {
	return `__:reset__


Resets the test.
`
}

type testReportCaller bool

func (caller testReportCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	w, _ := s.Get("*standard-output*").(io.Writer)
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			panic(fmt.Sprintf("stream argument to :report must be an output-stream, not %s", args[0]))
		}
	}
	var (
		indent []byte
		bold   = ""
		normal = ""
	)
	if s.Get("*print-ansi*") != nil {
		bold = "\x1b[1;31m" // bright red or bold-red
		normal = ansiNormal
	}
	for t, _ := self.Get("parent").(*flavors.Instance); t != nil; t, _ = t.Get("parent").(*flavors.Instance) {
		indent = append(indent, ' ', ' ')
	}
	name, _ := s.Get("name").(slip.String)
	var result string
	switch s.Get("result") {
	case slip.Symbol(":pass"):
		result = "PASS"
	case slip.Symbol(":fail"):
		result = "FAIL"
	default:
		result = "SKIP"
	}
	fmt.Fprintf(w, "%s%s: %s%s%s\n", indent, string(name), bold, result, normal)

	return nil
}

func (caller testReportCaller) Docs() string {
	return `__:report__ &optional stream
   _stream_ stream to print the results on. Default _*standard-output*_


Reports prints the test result.
`
}
