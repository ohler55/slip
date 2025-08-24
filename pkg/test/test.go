// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"io"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

const (
	ansiBold   = "\x1b[1m"
	ansiNormal = "\x1b[m"
	ansiRed    = "\x1b[0;31m"
	passSymbol = slip.Symbol(":pass")
	failSymbol = slip.Symbol(":fail")
)

var testFlavor *flavors.Flavor

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
			&Pkg,
		)
		testFlavor.DefMethod(":run", "", testRunCaller(true))
		testFlavor.DefMethod(":report", "", testReportCaller(true))
		testFlavor.DefMethod(":reset", "", testResetCaller(true))
	}
	return testFlavor
}

type testRunCaller bool

func (caller testRunCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	_, verbose, trace := getRunKeys(s, args, depth)
	if trace {
		slip.Trace(slip.List{slip.True})
	}
	self := s.Get("self").(*flavors.Instance)
	name, _ := s.Get("name").(slip.String)
	defer func() {
		s.Set("*current-test*", nil)
		w, _ := s.Get("*standard-output*").(io.Writer)
		slip.Untrace(nil)
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
			s.Set("result", failSymbol)
			msg := fmt.Sprintf("%s", rec)
			i2 := append([]byte{'\n', ' ', ' '}, indent...)
			msg = strings.TrimRight(strings.ReplaceAll(msg, "\n", string(i2)), " ")
			_, _ = fmt.Fprintf(w, "%s%s: %sFAIL%s\n%s  %s%s%s\n",
				indent, string(name), bold, normal, indent, red, msg, normal)
			if p, ok := rec.(*slip.Panic); ok && verbose {
				for _, frame := range p.Stack() {
					_, _ = fmt.Fprintf(w, "%s  %s%s%s\n", indent, red, frame, normal)
				}
			}
		} else if verbose || trace {
			_, _ = fmt.Fprintf(w, "%s%s: %sPASS%s\n", indent, string(name), bold, normal)
		}
	}()
	if forms, ok := s.Get("forms").(slip.List); ok {
		s.Let("*current-test*", self)
		for i := range forms {
			_ = slip.EvalArg(s, forms, i, depth)
		}
	}
	s.Set("result", passSymbol)

	return nil
}

func (caller testRunCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":run",
		Text: `Runs the test. The _*current-test*_ variable will be set to the test instance
for the duration of the run.`,
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":verbose",
				Type: "boolean",
				Text: `If true the test results will be printed.`,
			},
			{
				Name: ":trace",
				Type: "boolean",
				Text: `if true _(trace t)_ will be called before evaluating the tests.`,
			},
		},
	}
}

type testResetCaller bool

func (caller testResetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	s.Set("result", nil)
	return nil
}

func (caller testResetCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":reset",
		Text: "Resets the test.",
	}
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
		indent  []byte
		pre     = ""
		normal  = ""
		bold    = ""
		boldRed = ""
	)
	if s.Get("*print-ansi*") != nil {
		bold = ansiBold
		boldRed = "\x1b[1;31m" // bright red or bold-red
		normal = ansiNormal
	}
	for t, _ := self.Get("parent").(*flavors.Instance); t != nil; t, _ = t.Get("parent").(*flavors.Instance) {
		indent = append(indent, ' ', ' ')
	}
	name, _ := s.Get("name").(slip.String)
	var result string
	switch s.Get("result") {
	case passSymbol:
		result = "PASS"
		pre = bold
	case failSymbol:
		result = "FAIL"
		pre = boldRed
	default:
		result = "SKIP"
	}
	_, _ = fmt.Fprintf(w, "%s%s: %s%s%s\n", indent, string(name), pre, result, normal)

	return nil
}

func (caller testReportCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":report",
		Text: "Reports prints the test results.",
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name:    ":stream",
				Type:    "output-stream",
				Text:    `Stream to print the results on.`,
				Default: slip.Symbol("*stadard-output*"),
			},
		},
	}
}
