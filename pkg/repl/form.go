// Copyright (c) 2023, Peter Ohler, All rights reserved.

package repl

import "strings"

// Form is used to represent slip forms as a sequence of lines which are
// []rune.
type Form [][]rune

// Dup creates a deep duplicate of the form.
func (f Form) Dup() Form {
	d := make(Form, len(f))
	for i, line := range f {
		l2 := make([]rune, len(line))
		copy(l2, line)
		d[i] = l2
	}
	return d
}

// Contains returns true if the form contains the target string.
func (f Form) Contains(target string) bool {
	for _, line := range f {
		if strings.Contains(string(line), target) {
			return true
		}
	}
	return false
}

// String returns a string representation of the form.
func (f Form) String() string {
	var b []byte
	for _, line := range f {
		b = append(b, string(line)...)
		b = append(b, '\n')
	}
	return string(b)
}

// Append the form to a byte slice using tabs as separators between lines.
func (f Form) Append(b []byte) []byte {
	if 0 < len(f) {
		for _, line := range f {
			b = append(b, string(line)...)
			b = append(b, '\t')
		}
		b[len(b)-1] = '\n'
	}
	return b
}

// Empty return true if there are not non-space characters in the form.
func (f Form) Empty() bool {
	for _, line := range f {
		for _, r := range line {
			if r != ' ' {
				return false
			}
		}
	}
	return true
}

// Equal returns true if the two forms are equal.
func (f Form) Equal(f2 Form) bool {
	if len(f) != len(f2) {
		return false
	}
	for i, line := range f {
		line2 := f2[i]
		if len(line) != len(line2) || string(line) != string(line2) {
			return false
		}
	}
	return true
}
