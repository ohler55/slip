// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"io"

	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/cl"
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
				"setup":    nil, // function to call before running tests
				"teardown": nil, // function to call after running tests
			},
			[]string{"testable-flavor"}, // inherited
			slip.List{ // options
				slip.List{
					slip.Symbol(":documentation"),
					slip.String(`A suite is a container for child suites and tests.`),
				},
				slip.List{
					slip.Symbol(":inittable-instance-variables"),
					slip.Symbol("setup"),
					slip.Symbol("teardown"),
					slip.Symbol("name"),
					slip.Symbol("parent"),
				},
				slip.Symbol(":gettable-instance-variables"),
				slip.List{
					slip.Symbol(":settable-instance-variables"),
					slip.Symbol("setup"),
					slip.Symbol("teardown"),
				},
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
	children, _ := s.Get("children").(slip.List)
	for _, child := range children {
		if ci, _ := child.(*flavors.Instance); ci != nil {
			_ = ci.Receive(s, ":reset", nil, depth+1)
		}
	}
	if setup := s.Get("setup"); setup != nil {
		caller := cl.ResolveToCaller(s, setup, depth+1)
		_ = caller.Call(s, nil, depth+1)
	}
	if teardown := s.Get("teardown"); teardown != nil {
		defer func() {
			caller := cl.ResolveToCaller(s, teardown, depth+1)
			_ = caller.Call(s, nil, depth+1)
		}()
	}
	self := s.Get("self").(*flavors.Instance)
	name, _ := s.Get("name").(slip.String)
	var indent []byte
	for t, _ := self.Get("parent").(*flavors.Instance); t != nil; t, _ = t.Get("parent").(*flavors.Instance) {
		indent = append(indent, ' ', ' ')
	}
	w, _ := s.Get("*standard-output*").(io.Writer)
	filter, verbose, _ := getRunKeys(args)
	if verbose {
		fmt.Fprintf(w, "%s%s:\n", indent, string(name))
	}
	cargs := make(slip.List, len(args))
	copy(cargs, args)
	var childKey slip.Object
	if 0 < len(filter) {
		childKey = filter[0]
		for i, a := range cargs {
			if slip.Symbol(":filter") == a {
				// if args is short it would have been caught in getRunKeys
				cargs[i+1] = filter[1:]
				break
			}
		}
	}
	for _, child := range children {
		if ci, _ := child.(*flavors.Instance); ci != nil {
			if childKey != nil {
				cn, _ := ci.Get("name").(slip.String)
				if !keyMatchName(string(cn), childKey) {
					continue
				}
			}
			_ = ci.Receive(s, ":run", cargs, depth+1)
		}
	}
	if verbose {
		r := getResults(self)
		fmt.Fprintf(w, "%s-------------- %s:\n", indent, string(name))
		fmt.Fprintf(w, "%s  passed:  %d\n", indent, alt.Int(jp.C("pass").First(r)))
		fmt.Fprintf(w, "%s  failed:  %d\n", indent, alt.Int(jp.C("fail").First(r)))
		fmt.Fprintf(w, "%s  skipped: %d\n", indent, alt.Int(jp.C("skip").First(r)))
	}
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
	children, _ := s.Get("children").(slip.List)
	for _, child := range children {
		if ci, _ := child.(*flavors.Instance); ci != nil {
			_ = ci.Receive(s, ":reset", nil, depth+1)
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
	children, _ := s.Get("children").(slip.List)
	self := s.Get("self").(*flavors.Instance)
	name, _ := s.Get("name").(slip.String)
	var indent []byte
	for t, _ := self.Get("parent").(*flavors.Instance); t != nil; t, _ = t.Get("parent").(*flavors.Instance) {
		indent = append(indent, ' ', ' ')
	}
	w, _ := s.Get("*standard-output*").(io.Writer)
	if 0 < len(args) {
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			panic(fmt.Sprintf("stream argument to :report must be an output-stream, not %s", args[0]))
		}
	}
	fmt.Fprintf(w, "%s%s:\n", indent, string(name))
	for _, child := range children {
		if ci, _ := child.(*flavors.Instance); ci != nil {
			_ = ci.Receive(s, ":report", args, depth+1)
		}
	}
	r := getResults(self)
	fmt.Fprintf(w, "%s-------------- %s:\n", indent, string(name))
	fmt.Fprintf(w, "%s  passed:  %d\n", indent, alt.Int(jp.C("pass").First(r)))
	fmt.Fprintf(w, "%s  failed:  %d\n", indent, alt.Int(jp.C("fail").First(r)))
	fmt.Fprintf(w, "%s  skipped: %d\n", indent, alt.Int(jp.C("skip").First(r)))

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
	self := s.Get("self").(*flavors.Instance)
	r := bag.Flavor().MakeInstance()
	r.Any = getResults(self)

	return r
}

func (caller suiteResultCaller) Docs() string {
	return `__:result__


Result returns the suite results as a tree of lists where the car of each list
is the name of the suite or test.
`
}

type suiteFindCaller bool

func (caller suiteFindCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if 0 < len(args) {
		self := s.Get("self").(*flavors.Instance)
		path, ok := args[0].(slip.List)
		if !ok {
			path = args
		}
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

// Return a tree of results.
// {top: {pass: 0 fail: 0 skip: 0 subs: {leaf: pass}}}
func getResults(obj *flavors.Instance) (result any) {
	switch {
	case obj.Flavor == testFlavor:
		switch obj.Get("result") {
		case passSymbol:
			result = "pass"
		case failSymbol:
			result = "fail"
		}
	case obj.Flavor == suiteFlavor:
		var (
			pass int64
			fail int64
			skip int64
		)
		subs := map[string]any{}
		children, _ := obj.Get("children").(slip.List)
		for _, child := range children {
			if ci, _ := child.(*flavors.Instance); ci != nil {
				cn, _ := ci.Get("name").(slip.String)
				r := getResults(ci)
				subs[string(cn)] = r
				switch tr := r.(type) {
				case nil:
					skip++
				case string:
					if tr == "pass" {
						pass++
					} else if tr == "fail" {
						fail++
					}
				case map[string]any:
					pass += alt.Int(jp.C("pass").First(tr))
					fail += alt.Int(jp.C("fail").First(tr))
					skip += alt.Int(jp.C("skip").First(tr))
				}
			}
		}
		result = map[string]any{
			"pass": pass,
			"fail": fail,
			"skip": skip,
			"subs": subs,
		}
	}
	return
}

func findChild(obj *flavors.Instance, path slip.List, depth int) slip.Object {
	if len(path) == 0 {
		return obj
	}
	name := path[0]
	children, _ := obj.Get("children").(slip.List)
	for _, child := range children {
		if ci, _ := child.(*flavors.Instance); ci != nil {
			cn, _ := ci.Get("name").(slip.String)
			if keyMatchName(string(cn), name) {
				if 1 < len(path) {
					var found slip.Object
					if ci.HasMethod(":find") {
						found = ci.Receive(nil, ":find", slip.List{path[1:]}, depth+1)
					}
					return found
				}
				return ci
			}
		}
	}
	return nil
}

func keyMatchName(name string, k slip.Object) bool {
	var sk string
	switch tk := k.(type) {
	case slip.String:
		sk = string(tk)
	case slip.Symbol:
		sk = string(tk)
	default:
		return false
	}
	return name == sk
}
