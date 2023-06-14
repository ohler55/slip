// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var suiteFlavor *flavors.Flavor

func init() {
	_ = SuiteFlavor()
}

// SuiteFlavor returns the suite-flavor.
func SuiteFlavor() *flavors.Flavor {
	_ = TestableFlavor()
	if suiteFlavor == nil {
		suiteFlavor = flavors.DefFlavor("suite-flavor",
			map[string]slip.Object{ // variables
				"children": nil,
			},
			[]string{"testable-flavor"}, // inherited
			slip.List{ // options
				slip.List{
					slip.Symbol(":documentation"),
					slip.String(`A suite is a container for child suites and tests.`),
				},
				slip.Symbol(":gettable-instance-variables"),
			},
		)
		suiteFlavor.DefMethod(":run", "", suiteRunCaller(true))
		suiteFlavor.DefMethod(":reset", "", suiteResetCaller(true))
		suiteFlavor.DefMethod(":report", "", suiteReportCaller(true))
		suiteFlavor.DefMethod(":result", "", suiteResultCaller(true))
		suiteFlavor.DefMethod(":find", "", suiteFindCaller(true))
	}
	return suiteFlavor
}

type suiteRunCaller bool

func (caller suiteRunCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	// TBD run
	// reset all
	// call run on all children matching
	//   let children report as needed
	// if verbose print name at start and result summary

	return nil
}

func (caller suiteRunCaller) Docs() string {
	return `__:run__ &key filter verbose trace
   _:filter_ if present identifies the tests to run by a path. e.g., (top child leaf)
   _:verbose_ if true the test results will be printed.
   _:trace_ if true _(trace t)_ will be called before evaluating the tests.


Runs the test suite.
`
}

type suiteResetCaller bool

func (caller suiteResetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	s.Set("result", nil)
	children, _ := s.Get("children").(slip.List)
	for _, child := range children {
		if ci, _ := child.(*flavors.Instance); ci != nil {
			ci.Receive(":reset", nil, depth+1)
		}
	}
	return nil
}

func (caller suiteResetCaller) Docs() string {
	return `__:reset__


Resets the suite.
`
}

type suiteReportCaller bool

func (caller suiteReportCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {

	// TBD

	return nil
}

func (caller suiteReportCaller) Docs() string {
	return `__:report__ &optional stream
   _stream_ stream to print the results on. Default _*standard-output*_


Reports prints the suite result.
`
}

type suiteResultCaller bool

func (caller suiteResultCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {

	// TBD

	return nil
}

func (caller suiteResultCaller) Docs() string {
	return `__:result__


Result returns the suite results as a tree of lists where the car of each list
is the name of the suite or test.
`
}

type suiteFindCaller bool

func (caller suiteFindCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	// TBD handle multiple args as path
	if 0 < len(args) {
		path, _ := args[0].(slip.List)
		self := s.Get("self").(*flavors.Instance)

		return findChild(self, path, depth)
	}
	return nil
}

func (caller suiteFindCaller) Docs() string {
	return `__:find__ path*
   _path_ to a suite or test where each element of a list of strings identifies
a child of the parent suite.


Finds prints the suite result.
`
}

func findChild(obj *flavors.Instance, path slip.List, depth int) slip.Object {
	if len(path) == 0 {
		return obj
	}
	var name string
	switch tp := path[0].(type) {
	case slip.String:
		name = string(tp)
	case slip.Symbol:
		name = string(tp)
	default:
		return nil
	}
	children, _ := obj.Get("children").(slip.List)
	for _, child := range children {
		if ci, _ := child.(*flavors.Instance); ci != nil {
			cn, _ := ci.Receive(":name", nil, depth+1).(slip.String)
			if name == string(cn) {
				if 1 < len(path) {
					// TBD verify it is a suite or has method :find (need func on instance)
					return ci.Receive(":find", slip.List{path[1:]}, depth+1)
				}
				return ci
			}
		}
	}
	return nil
}
