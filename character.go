// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "unicode/utf8"

// CharacterCharacter is the character with a value of "character".
const CharacterSymbol = Symbol("character")

func init() {
	DefConstant(CharacterSymbol, CharacterSymbol,
		`A _character_ is a Unicode character that can be represented by a golang Rune.`)
}

var (
	specialCharacters = map[rune]string{
		rune(' '):    `#\Space`,
		rune('\b'):   `#\Backspace`,
		rune('\f'):   `#\Page`,
		rune('\n'):   `#\Newline`,
		rune('\r'):   `#\Return`,
		rune('\t'):   `#\Tab`,
		rune('\x7f'): `#\Rubout`,
	}
	hexChars = "0123456789abcdef"
)

// Character is a character Object.
type Character rune

// String representation of the Object.
func (obj Character) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Character) Append(b []byte) []byte {
	if s := specialCharacters[rune(obj)]; 0 < len(s) {
		return append(b, s...)
	}
	b = append(b, `#\`...)
	if obj < 0x20 {
		b = append(b, "u00"...)
		b = append(b, hexChars[obj>>4])
		return append(b, hexChars[obj&0x000f])
	}
	encoded := make([]byte, 4)
	n := utf8.EncodeRune(encoded, rune(obj))

	return append(b, encoded[:n]...)
}

// Simplify the Object into a string.
func (obj Character) Simplify() interface{} {
	return string([]rune{rune(obj)})
}

// Equal returns true if this Object and the other are equal in value.
func (obj Character) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as characters for the instance.
func (obj Character) Hierarchy() []Symbol {
	return []Symbol{CharacterSymbol, TrueSymbol}
}

// Eval returns self.
func (obj Character) Eval(s *Scope, depth int) Object {
	return obj
}
