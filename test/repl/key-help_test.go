// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

// helpLabels opens the help page (tall enough not to scroll) and returns the
// bold labels of the key binding rows.
func helpLabels(t *testing.T) (labels []string) {
	t.Helper()
	inRows := false
	bold := false
	keyEdTestSize(t, 120, 80, []any{
		startSteps,
		provide("\x08"),
		func(s string) bool {
			switch {
			case strings.Contains(s, "bindings are:"):
				inRows = true
			case match(keyBoxEnd, s):
				return true
			case s == "<bold>":
				bold = true
			case bold:
				bold = false
				if inRows {
					labels = append(labels, s)
				}
			}
			return false
		},
		provide("\x01"),
	})
	return
}

// helpDisplayKeys maps help labels that are display names to the key they
// stand for.
var helpDisplayKeys = map[string]string{
	"ENTER": "RET",
}

// Every help row reports an action for its key through repl-key-binding.
// The ENTER display label is mapped to RET, every other label is a key name;
// any other label that is not a key name fails the test. This checks that
// the rows and the tables agree on which keys are bound; matching each
// description to its action name is not possible from outside since the
// descriptions are free text.
func TestKeyHelpRowsAreBound(t *testing.T) {
	withKeyBindings(t, "")
	labels := helpLabels(t)
	checked := 0
	var missing []string
	for _, label := range labels {
		label = strings.TrimSpace(label)
		key := label
		if k, ok := helpDisplayKeys[label]; ok {
			key = k
		}
		scope := slip.NewScope()
		var action string
		var parsed bool
		func() {
			defer func() { _ = recover() }()
			src := fmt.Sprintf(`(repl-key-binding %q)`, key)
			action = slip.ObjectString(slip.ReadString(src, scope).Eval(scope, nil))
			parsed = true
		}()
		if !parsed {
			missing = append(missing, label+" (not a key name)")
			continue
		}
		checked++
		if action == "nil" {
			missing = append(missing, label)
		}
	}
	// Keys without defaults (addendum C) have no rows, and the arrows and
	// shift-tab rows use key names (addendum D5).
	have := map[string]bool{}
	for _, label := range labels {
		label = strings.TrimSpace(label)
		have[label] = true
		switch label {
		case "Home", "End", "Delete", "C-Delete", "<home>", "<end>", "<delete>", "C-<delete>",
			"C-<up>", "C-<down>", "C-<right>", "C-<left>":
			missing = append(missing, label+" (row for a key with no default)")
		}
	}
	for _, label := range []string{"<up>", "<down>", "<right>", "<left>", "S-<tab>"} {
		if !have[label] {
			missing = append(missing, label+" (no row)")
		}
	}
	tt.Equal(t, 0, len(missing), "help rows with no default action: %q (all labels %q)", missing, labels)
	tt.Equal(t, true, 40 < checked, "only %d help rows checked out of %q", checked, labels)
}
