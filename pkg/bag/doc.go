// Copyright (c) 2022, Peter Ohler, All rights reserved.

// Package bag contains the bag functions. A bag is flexible a container for
// data composed of t, nil, integer, float, string, time, list, as well as the
// special bag::map and bag::false. It can be parsed from JSON or SEN (ojg
// package) and be encoded in the same way. It can also be converted to and
// from native LISP with bag::map becoming an assoc list. The bag::false value
// is the only non-native value that is retained since LSIP does not
// differentiate between nil and boolean false.
package bag
