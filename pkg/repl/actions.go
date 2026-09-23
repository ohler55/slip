// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"reflect"
)

// action is a named bindFunc that can be bound to a key.
type action struct {
	name string
	fn   bindFunc
	doc  string
}

var (
	// actions are the bindable actions keyed by name.
	actions map[string]*action
	// funcActions maps a bindFunc pointer to the action for the function.
	funcActions map[uintptr]*action
	// modeSwitchers maps a mode switching bindFunc pointer to the mode it
	// switches to.
	modeSwitchers map[uintptr][]bindFunc
	// basePrefixes are the key sequences that are prefixes in the mode
	// tables.
	basePrefixes map[string]bool
	// defaultKeys are the actions bound to key sequences in the mode tables.
	defaultKeys map[string]*action
	// tabAction and shiftTabAction scroll the help display.
	tabAction      *action
	shiftTabAction *action
	// undefinedAction is used for keys that have been unbound by the user.
	undefinedAction = &action{name: "undefined", fn: bad, doc: "undefined key"}
)

func funcPtr(f bindFunc) uintptr {
	return reflect.ValueOf(f).Pointer()
}

// initActions builds the action registry and walks the mode tables to find
// the base prefixes and default key bindings. A panic occurs if a mode table
// includes a function that is not registered.
func initActions() {
	actions = map[string]*action{}
	funcActions = map[uintptr]*action{}
	for _, a := range []*action{
		{name: "line-begin", fn: lineBegin, doc: "move to line start"},
		{name: "line-end", fn: lineEnd, doc: "move to line end"},
		{name: "back-char", fn: back, doc: "move left one"},
		{name: "forward-char", fn: forward, doc: "move right one"},
		{name: "previous-line", fn: up, doc: "move up one (previous in history)"},
		{name: "next-line", fn: down, doc: "move down one (next in history)"},
		{name: "back-word", fn: backWord, doc: "move back one word"},
		{name: "forward-word", fn: forwardWord, doc: "move forward one word"},
		{name: "form-begin", fn: formBegin, doc: "move to form start"},
		{name: "form-end", fn: formEnd, doc: "move to form end"},
		{name: "delete-char", fn: delForward, doc: "delete one forward (exit)"},
		{name: "delete-forward", fn: delChar, doc: "delete one forward"},
		{name: "delete-back-char", fn: delBack, doc: "delete one back"},
		{name: "delete-word", fn: delForwardWord, doc: "delete one word"},
		{name: "delete-back-word", fn: delBackWord, doc: "delete previous word"},
		{name: "kill-line", fn: delLineEnd, doc: "delete to line end"},
		{name: "swap-chars", fn: swapChar, doc: "swap characters"},
		{name: "collapse-space", fn: collapse, doc: "collapse space"},
		{name: "newline", fn: nl, doc: "insert newline"},
		{name: "newline-after", fn: nlAfter, doc: "insert newline after"},
		{name: "enter", fn: enter, doc: "evaluate form"},
		{name: "eval-form", fn: eval, doc: "evaluate current form"},
		{name: "exit", fn: done, doc: "exit"},
		{name: "help", fn: help, doc: "show this help page"},
		{name: "describe", fn: describe, doc: "describe word"},
		{name: "tab", fn: tab, doc: "word completion or help scroll"},
		{name: "shift-tab", fn: shiftTab, doc: "help scroll back"},
		{name: "clear-form", fn: clearForm, doc: "clear current form"},
		{name: "match-open", fn: matchOpen, doc: "move forward to matching paren"},
		{name: "match-close", fn: matchClose, doc: "move back to matching paren"},
		{name: "edit-form", fn: editForm, doc: "edit current form in $EDITOR"},
		{name: "history-back", fn: historyBack, doc: "previous in history"},
		{name: "history-forward", fn: historyForward, doc: "next in history"},
		{name: "search-history-back", fn: searchBack, doc: "search history backward"},
		{name: "search-history-forward", fn: searchForward, doc: "search history forward"},
		{name: "nth-history", fn: nthHistory, doc: "nth in history"},
		{name: "stash-add", fn: stashAdd, doc: "add to stash"},
		{name: "stash-back", fn: stashBack, doc: "previous in stash"},
		{name: "stash-forward", fn: stashForward, doc: "next in stash"},
		{name: "search-stash-back", fn: searchStashBack, doc: "search stash backward"},
		{name: "search-stash-forward", fn: searchStashForward, doc: "search stash forward"},
		{name: "nth-stash", fn: nthStash, doc: "nth in stash"},
		{name: "enter-unicode", fn: enterUnicode, doc: "enter unicode"},
		{name: "reset-term", fn: resetTerm, doc: "reset terminal"},
		{name: "copy", fn: ccopy, doc: "copy to clipboard (macOS only)"},
		{name: "cut", fn: ccut, doc: "cut to clipboard (macOS only)"},
		{name: "paste", fn: cpaste, doc: "paste from clipboard (macOS only)"},
	} {
		actions[a.name] = a
		funcActions[funcPtr(a.fn)] = a
	}
	tabAction = actions["tab"]
	shiftTabAction = actions["shift-tab"]
	modeSwitchers = map[uintptr][]bindFunc{
		funcPtr(esc):     escMode,
		funcPtr(esc5b):   esc5bMode,
		funcPtr(csi1):    csi1Mode,
		funcPtr(csiMod):  csiModMode,
		funcPtr(csi3):    csi3Mode,
		funcPtr(csi3Mod): csi3ModMode,
		funcPtr(csi2):    csi2Mode,
		funcPtr(termKey): termKeyMode,
	}
	internal := map[uintptr]bool{
		funcPtr(bad):     true,
		funcPtr(addByte): true,
		funcPtr(topUni):  true,
		funcPtr(addUni):  true,
	}
	basePrefixes = map[string]bool{}
	defaultKeys = map[string]*action{}
	var walk func(mode []bindFunc, prefix []byte)
	walk = func(mode []bindFunc, prefix []byte) {
		for i, f := range mode[:256] {
			key := string(append(prefix, byte(i)))
			ptr := funcPtr(f)
			if a := funcActions[ptr]; a != nil {
				defaultKeys[key] = a
			} else if next := modeSwitchers[ptr]; next != nil {
				basePrefixes[key] = true
				walk(next, []byte(key))
			} else if !internal[ptr] {
				panic(fmt.Sprintf("key %s in mode table %q is not a registered action", formatKey([]byte(key)), prefix))
			}
		}
	}
	walk(rootMode, nil)
}

// tableFunc returns the function in the mode tables for a key sequence. All
// but the last byte of the sequence must be a base prefix.
func tableFunc(seq string) bindFunc {
	mode := rootMode
	last := len(seq) - 1
	for i := 0; i < last; i++ {
		mode = modeSwitchers[funcPtr(mode[seq[i]])]
	}
	return mode[seq[last]]
}
