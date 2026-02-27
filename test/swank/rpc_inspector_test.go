// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg/cl"
)

func TestInitInspector(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	resp := env.sendRex(t, slip.List{slip.Symbol("swank:init-inspector")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectFixnum(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("42")})
	if result == nil {
		t.Fatal("expected inspector content")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, ":title") {
		t.Errorf("expected :title in response: %s", s)
	}
	if !strings.Contains(s, "42") {
		t.Errorf("expected 42 in title: %s", s)
	}
	// Fixnum should have hex/octal/binary representations
	if !strings.Contains(s, "Hex") {
		t.Errorf("expected Hex representation: %s", s)
	}
}

func TestInspectString(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Use (symbol-name 'abc) to produce a string for inspection
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("(symbol-name 'abc)")})
	if result == nil {
		t.Fatal("expected inspector content")
	}
	s := slip.ObjectString(result)
	t.Logf("string inspection result: %s", s)
	if !strings.Contains(s, "Length") {
		t.Errorf("expected Length in string inspection: %s", s)
	}
}

func TestInspectList(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("'(a b c)")})
	if result == nil {
		t.Fatal("expected inspector content")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Length: 3") {
		t.Errorf("expected Length: 3 in list inspection: %s", s)
	}
	if !strings.Contains(s, ":value") {
		t.Errorf("expected :value references: %s", s)
	}
}

func TestInspectSymbol(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("'car")})
	if result == nil {
		t.Fatal("expected inspector content")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "Name") {
		t.Errorf("expected Name in symbol inspection: %s", s)
	}
}

func TestInspectNil(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("nil")})
	if result == nil {
		t.Fatal("expected inspector content")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "nil") {
		t.Errorf("expected nil in title: %s", s)
	}
}

func TestInspectNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	resp := env.sendRex(t, slip.List{slip.Symbol("swank:inspect-in-emacs")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectNthPart(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// First inspect a list to populate parts
	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("'(a b c)")})

	// Get nth part (index 0 should be 'a)
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspector-nth-part"), slip.Fixnum(0)})
	if result == nil {
		t.Error("expected part at index 0, got nil — parts not populated after inspect-in-emacs")
	}
}

func TestInspectNthPartNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	resp := env.sendRex(t, slip.List{slip.Symbol("swank:inspector-nth-part")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectNthPartBadIndex(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("'(a)")})

	// Out of bounds
	env.sendRex(t, slip.List{slip.Symbol("swank:inspector-nth-part"), slip.Fixnum(99)})

	// Non-fixnum argument
	env.sendRex(t, slip.List{slip.Symbol("swank:inspector-nth-part"), slip.String("bad")})

	// Negative index
	env.sendRex(t, slip.List{slip.Symbol("swank:inspector-nth-part"), slip.Fixnum(-1)})
}

func TestInspectDrillDown(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Inspect a list
	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("'(a b c)")})

	// Drill into first element
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-nth-part"), slip.Fixnum(0)})
	if result != nil {
		s := slip.ObjectString(result)
		if !strings.Contains(s, ":title") {
			t.Errorf("expected :title after drill-down: %s", s)
		}
	}
}

func TestInspectDrillDownNoArgs(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	resp := env.sendRex(t, slip.List{slip.Symbol("swank:inspect-nth-part")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectDrillDownBadIndex(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("'(a)")})
	// Out of bounds
	env.sendRex(t, slip.List{slip.Symbol("swank:inspect-nth-part"), slip.Fixnum(99)})
	// Non-fixnum
	env.sendRex(t, slip.List{slip.Symbol("swank:inspect-nth-part"), slip.String("bad")})
}

func TestInspectorHistory(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Inspect first object
	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("42")})

	// Inspect second object (pushes 42 to history)
	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("99")})

	// Pop back to 42
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspector-pop")})
	if result == nil {
		t.Fatal("expected inspector content after pop")
	}
	if plist, ok := result.(slip.List); ok {
		for i := 0; i+1 < len(plist); i += 2 {
			if plist[i] == slip.Symbol(":title") {
				if title, ok := plist[i+1].(slip.String); !ok || string(title) != "42" {
					t.Errorf("expected title 42, got %v", plist[i+1])
				}
			}
		}
	}

	// Forward to 99
	result = env.sendRexOK(t, slip.List{slip.Symbol("swank:inspector-next")})
	if result == nil {
		t.Fatal("expected inspector content after next")
	}
	if plist, ok := result.(slip.List); ok {
		for i := 0; i+1 < len(plist); i += 2 {
			if plist[i] == slip.Symbol(":title") {
				if title, ok := plist[i+1].(slip.String); !ok || string(title) != "99" {
					t.Errorf("expected title 99, got %v", plist[i+1])
				}
			}
		}
	}
}

func TestInspectorPopEmpty(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Pop with no history
	resp := env.sendRex(t, slip.List{slip.Symbol("swank:inspector-pop")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectorNextEmpty(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Next with no forward history
	resp := env.sendRex(t, slip.List{slip.Symbol("swank:inspector-next")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectorRange(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("42")})
	resp := env.sendRex(t, slip.List{slip.Symbol("swank:inspector-range"), slip.Fixnum(0), slip.Fixnum(10)})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectorReinspect(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("42")})
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspector-reinspect")})
	if result != nil {
		s := slip.ObjectString(result)
		if !strings.Contains(s, "42") {
			t.Errorf("expected 42 after reinspect: %s", s)
		}
	}
}

func TestQuitInspector(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("42")})
	resp := env.sendRex(t, slip.List{slip.Symbol("swank:quit-inspector")})
	if resp[0] != slip.Symbol(":return") {
		t.Errorf("expected :return, got %v", resp[0])
	}
}

func TestInspectFloat(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("3.14")})
	if result == nil {
		t.Fatal("expected inspector content")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "3.14") {
		t.Errorf("expected 3.14 in float inspection: %s", s)
	}
	// Float should NOT have hex/octal
	if strings.Contains(s, "Hex") {
		t.Errorf("float should not have hex: %s", s)
	}
}

func TestInspectFuncInfo(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Inspect a function info object - #'car should resolve to a FuncInfo
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("#'car")})
	if result != nil {
		s := slip.ObjectString(result)
		if !strings.Contains(s, ":title") {
			t.Errorf("expected :title in funcinfo inspection: %s", s)
		}
	}
}

func TestInspectorReinspectAfterQuit(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("42")})
	env.sendRex(t, slip.List{slip.Symbol("swank:quit-inspector")})

	// Reinspect after quit — current is nil, should return nil plist
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspector-reinspect")})
	if result == nil {
		t.Fatal("expected nil-object plist from reinspect after quit")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "nil") {
		t.Errorf("expected nil title after reinspect on empty inspector: %s", s)
	}
}

func TestInspectRatio(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("(/ 1 3)")})
	if result == nil {
		t.Fatal("expected inspector content for ratio")
	}
	s := slip.ObjectString(result)
	if !strings.Contains(s, "1/3") {
		t.Errorf("expected 1/3 in ratio inspection: %s", s)
	}
	// Ratio must not include Hex representation
	if strings.Contains(s, "Hex") {
		t.Errorf("ratio should not have hex: %s", s)
	}
}

func TestInspectLambda(t *testing.T) {
	env := newTestEnv(t)
	defer env.close()

	// Create and inspect a lambda
	result := env.sendRexOK(t, slip.List{slip.Symbol("swank:inspect-in-emacs"), slip.String("(lambda (x) (* x x))")})
	if result != nil {
		s := slip.ObjectString(result)
		if !strings.Contains(s, ":title") {
			t.Errorf("expected :title in lambda inspection: %s", s)
		}
	}
}
