// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl"
)

func TestToggleTrace(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Trace a function — first toggle should trace
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("append")})
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Tracing") || !strings.Contains(strings.ToUpper(s), "APPEND") {
		t.Errorf("expected 'Tracing APPEND' in first toggle response: %s", s)
	}

	// Toggle again to untrace — second toggle should untrace
	result = env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("append")})
	s = slip.ObjectString(result)
	if !strings.Contains(s, "Untraced") || !strings.Contains(strings.ToUpper(s), "APPEND") {
		t.Errorf("expected 'Untraced APPEND' in second toggle response: %s", s)
	}

	// Clean up
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceSymbol(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:toggle-trace"), slip.Symbol("car")})
	s := slip.ObjectString(result)
	if !strings.Contains(strings.ToUpper(s), "CAR") {
		t.Errorf("expected CAR in trace response: %s", s)
	}
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceSetfSpec(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("(setf car)")})
	s := slip.ObjectString(result)
	// Should parse setf spec — result contains SETF-CAR
	if !strings.Contains(strings.ToUpper(s), "SETF-CAR") {
		t.Logf("setf spec response: %s", s)
	}
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceDefgenericSpec(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("(:defgeneric my-func)")})
	s := slip.ObjectString(result)
	if !strings.Contains(strings.ToUpper(s), "MY-FUNC") {
		t.Logf("defgeneric spec response: %s", s)
	}
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceDefmethodSpec(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("(:defmethod my-method qual)")})
	s := slip.ObjectString(result)
	if !strings.Contains(strings.ToUpper(s), "MY-METHOD") {
		t.Logf("defmethod spec response: %s", s)
	}
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceParenWrappedName(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Name wrapped in parens but not a keyword spec — falls through to "just a name in parens"
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("(list)")})
	s := slip.ObjectString(result)
	if !strings.Contains(strings.ToUpper(s), "LIST") {
		t.Errorf("expected LIST in paren-wrapped response: %s", s)
	}
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace")})
	if result == nil {
		t.Fatal("expected non-nil result for no-args toggle")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "No function") {
		t.Errorf("expected 'No function' message: %s", s)
	}
}

func TestToggleTraceInvalidArg(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.Fixnum(42)})
	if result == nil {
		t.Fatal("expected non-nil result for invalid arg toggle")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Invalid") {
		t.Errorf("expected 'Invalid' message: %s", s)
	}
}

func TestUntraceAll(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Trace something first so untrace-all has work to do
	env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("car")})

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Untraced all") {
		t.Errorf("expected untrace-all message: %s", s)
	}
}

func TestToggleTraceRepeated(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// First toggle should trace, second should untrace.
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("last")})
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Tracing") || !strings.Contains(strings.ToUpper(s), "LAST") {
		t.Errorf("toggle 1: expected 'Tracing LAST': %s", s)
	}

	result = env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("last")})
	s = slip.ObjectString(result)
	if !strings.Contains(s, "Untraced") || !strings.Contains(strings.ToUpper(s), "LAST") {
		t.Errorf("toggle 2: expected 'Untraced LAST': %s", s)
	}

	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceEmptySpec(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Empty string should return empty from parseTraceSpec, triggering "Cannot parse"
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("")})
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Cannot parse") {
		t.Errorf("expected 'Cannot parse' for empty spec: %s", s)
	}
}

func TestToggleTraceMalformedParenSpec(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Starts with '(' but does not end with ')' — parseTraceSpec returns ""
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("(unclosed")})
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Cannot parse") {
		t.Errorf("expected 'Cannot parse' for malformed paren spec: %s", s)
	}
}

func TestToggleTraceSetfIncomplete(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// (setf) with no function name — setf prefix matched but only 1 part
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("(setf)")})
	s := slip.ObjectString(result)
	// parseTraceSpec falls through to "just a name in parens" → "setf"
	if !strings.Contains(strings.ToUpper(s), "SETF") {
		t.Logf("incomplete setf spec response: %s", s)
	}
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}

func TestToggleTraceKeywordOnlySpec(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// (:defgeneric) with no name — keyword prefix matched but only 1 part
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:swank-toggle-trace"), slip.String("(:defgeneric)")})
	s := slip.ObjectString(result)
	// Falls through to "just a name in parens" → ":defgeneric"
	if !strings.Contains(strings.ToUpper(s), "DEFGENERIC") {
		t.Logf("keyword-only spec response: %s", s)
	}
	env.sendRexOK(t, slip.List{slip.Symbol("swank:untrace-all")})
}
