// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"unicode"
	"unicode/utf8"
)

// CharacterSymbol is the Symbol with a value of "character".
const CharacterSymbol = Symbol("character")

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
	runeMap = map[string]Character{
		"backspace": Character('\b'),
		"newline":   Character('\n'),
		"page":      Character('\f'),
		"return":    Character('\r'),
		"rubout":    Character('\x7f'),
		"space":     Character(' '),
		"tab":       Character('\t'),
	}

	hexChars = "0123456789abcdef"
)

// Character is a character Object.
type Character rune

// ReadCharacter read a character from bytes that would follow #\.
func ReadCharacter(src []byte) (c Character) {
	switch len(src) {
	case 0:
		PanicParse(`'#\' is not a valid character`)
	case 1:
		c = Character(src[0])
	default:
		var ok bool
		if c, ok = runeMap[string(bytes.ToLower(src))]; ok {
			break
		}
		if src[0] == 'u' || src[0] == 'U' {
			if 7 < len(src) {
				break
			}
			var rn rune
			for _, b := range src[1:] {
				rn = rn<<4 + rune(hexByteValues[b])
			}
			if rn <= unicode.MaxRune {
				c = Character(rn)
			}
			break
		}
		if rn, n := utf8.DecodeRune(src); 0 < n {
			c = Character(rn)
		}
	}
	if c == 0 {
		PanicParse(`'#\%s' is not a valid character`, src)
	}
	return
}

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
func (obj Character) Simplify() any {
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

// LoadForm returns a form that can be evaluated to create the object.
func (obj Character) LoadForm() Object {
	return obj
}
