// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"io"
	"strings"
	"sync/atomic"

	"github.com/ohler55/slip"
)

const keyBindingsName = "*repl-key-bindings*"

// userKeymap is a node in the tree of user key bindings that overlays the
// mode tables.
type userKeymap struct {
	binds [256]*action // nil = no user binding at this byte
	next  [256]*userKeymap
}

// keyBinding is a user key binding. A nil act indicates the key has been
// disabled.
type keyBinding struct {
	key  string   // canonical key name
	seqs []string // all key sequences for the key
	act  *action
}

// userBindings is an immutable snapshot of the user key bindings.
type userBindings struct {
	// list of bindings in the order they were added.
	list []keyBinding
	// acts maps a key sequence to the user bound action.
	acts map[string]*action
	// root of the user key bindings tree.
	root *userKeymap
}

var (
	userBinds atomic.Pointer[userBindings]
	// loadingConfig is true while the config file is being evaluated.
	loadingConfig bool
)

func init() {
	setKeyBindings(nil)
}

// setKeyBindings builds a new snapshot of the user key bindings and makes it
// current. The bindings must already have been checked with bindingKey. A
// later binding replaces an earlier one for the same key sequence.
func setKeyBindings(kbs []keyBinding) {
	ub := userBindings{list: kbs, acts: map[string]*action{}, root: &userKeymap{}}
	for _, kb := range kbs {
		a := kb.act
		if a == nil {
			a = undefinedAction
		}
		for _, seq := range kb.seqs {
			km := ub.root
			last := len(seq) - 1
			for i := 0; i < last; i++ {
				b := seq[i]
				if km.next[b] == nil {
					km.next[b] = &userKeymap{}
				}
				km = km.next[b]
			}
			km.binds[seq[last]] = a
			ub.acts[seq] = a
		}
	}
	userBinds.Store(&ub)
}

// parseKeyArg parses a key argument into the canonical key name and the key
// sequences for the key or panics with a lisp error.
func parseKeyArg(s *slip.Scope, depth int, value slip.Object) (string, []string) {
	str, ok := value.(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "key", value, "string")
	}
	name, seqs, err := parseKey(string(str))
	if err != nil {
		slip.ErrorPanic(s, depth, "%s", err)
	}
	return name, seqs
}

// bindingKey parses a key and panics if any of the key sequences is a prefix
// in the mode tables or if any of the leading bytes are a key and not a
// prefix. Since user bindings follow the same rules they can never be a
// prefix of each other.
func bindingKey(s *slip.Scope, depth int, value slip.Object) keyBinding {
	name, seqs := parseKeyArg(s, depth, value)
	for _, seq := range seqs {
		if basePrefixes[seq] {
			slip.ErrorPanic(s, depth, "key %s is a prefix for other keys and can not be bound", name)
		}
		for i := 1; i < len(seq); i++ {
			if !basePrefixes[seq[:i]] {
				// An escape starts another key so it is a chord and not an
				// unknown prefix.
				if seq[i-1] != 0x1b && funcPtr(tableFunc(seq[:i])) == funcPtr(bad) {
					slip.ErrorPanic(s, depth, "key %s can not be bound, %s is not a key prefix the editor knows",
						name, formatKey([]byte(seq[:i])))
				}
				slip.ErrorPanic(s, depth,
					"key %s can not be bound since %s is a key and not a prefix, multi-key chords are not supported",
					name, formatKey([]byte(seq[:i])))
			}
		}
	}
	return keyBinding{key: name, seqs: seqs}
}

// addKeyBinding replaces the binding for the same key or appends the binding
// if there is no binding for the key.
func addKeyBinding(kbs []keyBinding, kb keyBinding) []keyBinding {
	for i, x := range kbs {
		if x.key == kb.key {
			kbs[i] = kb
			return kbs
		}
	}
	return append(kbs, kb)
}

// findAction returns the action with the provided name. Nil is returned for
// a nil name.
func findAction(s *slip.Scope, depth int, value slip.Object) (a *action) {
	switch tv := value.(type) {
	case nil:
	case slip.Symbol:
		if a = actions[strings.ToLower(string(tv))]; a == nil {
			slip.ErrorPanic(s, depth, "unknown action %s", tv)
		}
	default:
		slip.TypePanic(s, depth, "action", value, "symbol", "nil")
	}
	return
}

// keyAction returns the effective action for a key sequence, the user
// binding if there is one otherwise the default binding.
func keyAction(seq string) *action {
	if a := userBinds.Load().acts[seq]; a != nil {
		return a
	}
	return defaultKeys[seq]
}

func actionSymbol(a *action) slip.Object {
	if a == nil || a == undefinedAction {
		return nil
	}
	return slip.Symbol(a.name)
}

func getKeyBindings() slip.Object {
	kbs := userBinds.Load().list
	if len(kbs) == 0 {
		return nil
	}
	list := make(slip.List, len(kbs))
	for i, kb := range kbs {
		key := slip.String(kb.key)
		if kb.act == nil {
			list[i] = slip.List{key}
		} else {
			list[i] = slip.List{key, slip.Tail{Value: slip.Symbol(kb.act.name)}}
		}
	}
	return list
}

// setKeyBindingsVar sets the user key bindings. Normally nothing is changed
// if any entry is invalid but while loading the config file invalid entries
// are dropped with a warning so a bad entry can not stop the REPL from
// starting.
func setKeyBindingsVar(value slip.Object) {
	s := slip.NewScope()
	switch list := value.(type) {
	case nil:
		setKeyBindings(nil)
	case slip.List:
		var kbs []keyBinding
		for _, item := range list {
			if loadingConfig {
				kbs = addLoadedKeyBinding(s, kbs, item)
			} else {
				kbs = addKeyBinding(kbs, listKeyBinding(s, item))
			}
		}
		setKeyBindings(kbs)
	default:
		if loadingConfig {
			warnKeyBindings("value", value, "not a list")
			setKeyBindings(nil)
			return
		}
		slip.TypePanic(s, 0, keyBindingsName, value, "list", "nil")
	}
}

// addLoadedKeyBinding adds a key binding from the config file or writes a
// warning if the entry is invalid.
func addLoadedKeyBinding(s *slip.Scope, kbs []keyBinding, item slip.Object) (result []keyBinding) {
	result = kbs
	defer func() {
		if rec := recover(); rec != nil {
			reason := fmt.Sprint(rec)
			if inst, ok := rec.(slip.Instance); ok {
				reason = (&slip.Panic{Condition: inst}).Error()
			}
			warnKeyBindings("entry", item, reason)
		}
	}()
	return addKeyBinding(kbs, listKeyBinding(s, item))
}

// warnKeyBindings writes a warning about a dropped part of the key bindings
// read from the config file.
func warnKeyBindings(what string, obj slip.Object, reason string) {
	_, _ = fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%swarning: dropped %s %s %s: %s%s\n",
		warnPrefix, keyBindingsName, what, slip.ObjectString(obj), reason, warnSuffix())
}

// listKeyBinding returns the key binding for a (key . action) cons.
func listKeyBinding(s *slip.Scope, item slip.Object) keyBinding {
	cons, ok := item.(slip.List)
	if !ok {
		slip.TypePanic(s, 0, "key binding", item, "cons")
	}
	cdr := cons.Cdr()
	if len(cons) == 2 && cons[1] == nil { // (key . nil) is read as (key nil)
		cdr = nil
	}
	kb := bindingKey(s, 0, cons.Car())
	kb.act = findAction(s, 0, cdr)

	return kb
}

// dispatch a byte first through the user key bindings and then the mode
// tables. The user bindings are only checked when the mode tables are at
// the top level or when a user binding prefix has been read. Returns true if
// the form should be evaluated.
func (ed *editor) dispatch(b byte) bool {
	km := ed.userKey
	if km == nil && ed.atTop() {
		// Start of a new key.
		km = userBinds.Load().root
		ed.keyBytes = ed.keyBytes[:0]
	}
	ed.keyBytes = append(ed.keyBytes, b)
	if km == nil {
		return ed.mode[b](ed, b)
	}
	if next := km.next[b]; next != nil {
		ed.userKey = next
		return false
	}
	ed.userKey = nil
	if a := km.binds[b]; a != nil {
		return a.fn(ed, b)
	}
	// Bytes held for the user bindings are always prefixes in the mode
	// tables so the mode switching functions never return true.
	for _, h := range ed.keyBytes[:len(ed.keyBytes)-1] {
		ed.mode[h](ed, h)
	}
	return ed.mode[b](ed, b)
}

// overrideBinding returns the function bound to the current key while an
// override is active. A user binding takes precedence and is mapped to a
// function with the aliases.
func (ed *editor) overrideBinding(bindings, aliases map[string]bindFunc) bindFunc {
	k := ed.key.buf[:ed.key.cnt]
	if a := userBinds.Load().acts[string(k)]; a != nil {
		return aliases[a.name]
	}
	return bindings[string(k)]
}

// atTop returns true if the mode tables are at the top level.
func (ed *editor) atTop() bool {
	return &ed.mode[0] == &topMode[0]
}

// helpScrollKey returns true if the key read scrolls the help display. A
// user binding for the key is used if there is one otherwise the key is
// checked for tab or shift-tab.
func (ed *editor) helpScrollKey() bool {
	if a := userBinds.Load().acts[string(ed.key.buf[:ed.key.cnt])]; a != nil {
		return a == tabAction || a == shiftTabAction
	}
	return ed.key.buf[0] == 0x09 || (ed.key.buf[0] == 0x1b && ed.key.buf[1] == 0x5b && ed.key.buf[2] == 0x5a)
}
