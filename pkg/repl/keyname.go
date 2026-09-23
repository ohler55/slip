// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"regexp"
	"strings"
)

var namedKeys = []struct {
	name string
	b    byte
}{
	{name: "DEL", b: 0x7f},
	{name: "TAB", b: '\t'},
	{name: "RET", b: '\r'},
	{name: "ESC", b: 0x1b},
	{name: "SPC", b: ' '},
}

// parseRawKey parses an Emacs style key such as "C-a", "M-f", or "M-[1;5C"
// into a key sequence.
func parseRawKey(s string) ([]byte, error) {
	if len(s) == 0 {
		return nil, fmt.Errorf("invalid key, a key can not be empty")
	}
	var seq []byte
	name := s
	// C-M- is the same as M-C-.
	s = strings.ReplaceAll(s, "C-M-", "M-C-")
top:
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case strings.HasPrefix(s[i:], "M-"):
			if len(s) <= i+2 {
				return nil, fmt.Errorf("invalid key %q, M- must be followed by a key", name)
			}
			seq = append(seq, 0x1b)
			i++
			continue
		case strings.HasPrefix(s[i:], "C-"):
			if len(s) <= i+2 {
				return nil, fmt.Errorf("invalid key %q, C- must be followed by a key", name)
			}
			if strings.HasPrefix(s[i+2:], "C-") || strings.HasPrefix(s[i+2:], "M-") {
				return nil, fmt.Errorf("invalid key %q, C- can not be followed by C- or M- other than C-M-", name)
			}
			i += 2
			c = s[i]
			switch {
			case 'a' <= c && c <= 'z':
				seq = append(seq, c-'a'+1)
			case 'A' <= c && c <= 'Z':
				seq = append(seq, c-'A'+1)
			case c == '@':
				seq = append(seq, 0)
			case '[' <= c && c <= '_':
				seq = append(seq, c-'['+0x1b)
			case c == '/':
				seq = append(seq, 0x1f)
			default:
				return nil, fmt.Errorf("invalid key %q, C-%c is not supported", name, c)
			}
			continue
		}
		for _, nk := range namedKeys {
			if strings.HasPrefix(s[i:], nk.name) {
				seq = append(seq, nk.b)
				i += len(nk.name) - 1
				continue top
			}
		}
		if c <= ' ' || 0x7f <= c {
			return nil, fmt.Errorf("invalid key %q, byte 0x%02x must be written as a key name", name, c)
		}
		seq = append(seq, c)
	}
	return seq, nil
}

// formatRawKey returns the Emacs style notation for a key sequence without
// using terminal key names.
func formatRawKey(seq []byte) string {
	var b []byte
	for i, c := range seq {
		switch {
		case c == 0x1b && i == len(seq)-1:
			b = append(b, "ESC"...)
		case c == 0x1b:
			b = append(b, 'M', '-')
		case c == '\t':
			b = append(b, "TAB"...)
		case c == '\r':
			b = append(b, "RET"...)
		case c == ' ':
			b = append(b, "SPC"...)
		case c == 0x7f:
			b = append(b, "DEL"...)
		case c < 0x20:
			b = append(b, 'C', '-', "@abcdefghijklmnopqrstuvwxyz[\\]^_"[c])
		case 0x80 <= c:
			b = append(b, '\\', 'u', '0', '0', hexMap[c>>4], hexMap[c&0x0f])
		default:
			b = append(b, c)
		}
	}
	return string(b)
}

// termKeyName describes a named terminal key. Keys with a final byte are
// modified as esc [ 1 ; <modifier> <final> while the others are modified as
// esc [ <num> ; <modifier> ~.
type termKeyName struct {
	name  string
	final byte
	num   string
	plain []string // unmodified encodings, esc [ <num> ~ if empty
	shift string   // the only encoding for keys that must have a shift
}

var (
	termKeyNames = []*termKeyName{
		{name: "up", final: 'A', plain: []string{"\x1b[A", "\x1bOA"}},
		{name: "down", final: 'B', plain: []string{"\x1b[B", "\x1bOB"}},
		{name: "right", final: 'C', plain: []string{"\x1b[C", "\x1bOC"}},
		{name: "left", final: 'D', plain: []string{"\x1b[D", "\x1bOD"}},
		{name: "home", final: 'H', plain: []string{"\x1b[H", "\x1b[1~", "\x1bOH"}},
		{name: "end", final: 'F', plain: []string{"\x1b[F", "\x1b[4~", "\x1bOF"}},
		{name: "insert", num: "2"},
		{name: "delete", num: "3"},
		{name: "prior", num: "5"},
		{name: "next", num: "6"},
		{name: "f1", final: 'P', plain: []string{"\x1bOP"}},
		{name: "f2", final: 'Q', plain: []string{"\x1bOQ"}},
		{name: "f3", final: 'R', plain: []string{"\x1bOR"}},
		{name: "f4", final: 'S', plain: []string{"\x1bOS"}},
		{name: "f5", num: "15"},
		{name: "f6", num: "17"},
		{name: "f7", num: "18"},
		{name: "f8", num: "19"},
		{name: "f9", num: "20"},
		{name: "f10", num: "21"},
		{name: "f11", num: "23"},
		{name: "f12", num: "24"},
		{name: "tab", shift: "\x1b[Z"},
	}
	// termKeyAliases maps an alias to the key name and the modifiers the
	// alias includes.
	termKeyAliases = map[string]struct {
		name string
		mods int
	}{
		"pageup":   {name: "prior"},
		"pagedown": {name: "next"},
		"backtab":  {name: "tab", mods: modShift},
	}
	termKeyRx = regexp.MustCompile(`^((?:[CMS]-)*)<([A-Za-z0-9]+)>$`)
	// seqNames maps each encoding of a named key to the canonical name.
	seqNames = map[string]string{}
)

// Modifier bits of the xterm modifier parameter which is one more than the
// bits.
const (
	modShift = 1
	modMeta  = 2
	modCtrl  = 4
)

func init() {
	for _, tk := range termKeyNames {
		for mods := 0; mods < 8; mods++ {
			if seqs, err := tk.encodings(mods); err == nil {
				for _, seq := range seqs {
					seqNames[seq] = modNames(mods) + "<" + tk.name + ">"
				}
			}
		}
	}
}

// modNames returns the modifier prefix in canonical order.
func modNames(mods int) (s string) {
	if mods&modCtrl != 0 {
		s += "C-"
	}
	if mods&modMeta != 0 {
		s += "M-"
	}
	if mods&modShift != 0 {
		s += "S-"
	}
	return
}

// encodings returns the key sequences for the key with the modifiers.
func (tk *termKeyName) encodings(mods int) ([]string, error) {
	switch {
	case 0 < len(tk.shift):
		if mods != modShift {
			return nil, fmt.Errorf("<%s> must be used with only S-", tk.name)
		}
		return []string{tk.shift}, nil
	case mods == 0 && 0 < len(tk.plain):
		return tk.plain, nil
	case mods == 0:
		return []string{"\x1b[" + tk.num + "~"}, nil
	case tk.final != 0:
		return []string{fmt.Sprintf("\x1b[1;%d%c", mods+1, tk.final)}, nil
	}
	return []string{fmt.Sprintf("\x1b[%s;%d~", tk.num, mods+1)}, nil
}

// parseKey parses a key into its canonical name and all the key sequences
// for the key. A key is either a terminal key name with optional modifiers
// such as "C-<right>" or an Emacs style key such as "C-a" or "M-[1;5C".
func parseKey(s string) (name string, seqs []string, err error) {
	m := termKeyRx.FindStringSubmatch(s)
	if m == nil {
		var seq []byte
		if seq, err = parseRawKey(s); err != nil {
			return
		}
		return formatRawKey(seq), []string{string(seq)}, nil
	}
	var mods int
	for i := 0; i < len(m[1]); i += 2 {
		bit := map[byte]int{'C': modCtrl, 'M': modMeta, 'S': modShift}[m[1][i]]
		if mods&bit != 0 {
			return "", nil, fmt.Errorf("invalid key %q, %c- is repeated", s, m[1][i])
		}
		mods |= bit
	}
	kn := strings.ToLower(m[2])
	if alias, has := termKeyAliases[kn]; has {
		kn = alias.name
		mods |= alias.mods
	}
	for _, tk := range termKeyNames {
		if tk.name == kn {
			if seqs, err = tk.encodings(mods); err != nil {
				return "", nil, fmt.Errorf("invalid key %q, %w", s, err)
			}
			return modNames(mods) + "<" + kn + ">", seqs, nil
		}
	}
	return "", nil, fmt.Errorf("invalid key %q, <%s> is not a known key name", s, m[2])
}

// formatKey returns the name of a key sequence. The terminal key name is
// used if the sequence is one of the encodings of a named key.
func formatKey(seq []byte) string {
	if name, has := seqNames[string(seq)]; has {
		return name
	}
	return formatRawKey(seq)
}
