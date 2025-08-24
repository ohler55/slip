// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"strings"

	"github.com/ohler55/slip"
)

type seqFunVars struct {
	item    slip.Object
	start   int
	end     int
	count   int
	test    slip.Caller
	key     slip.Caller
	fromEnd bool
	noCount bool
	noItem  bool
}

func (sfv *seqFunVars) setKeysItem(f slip.Object, s *slip.Scope, args slip.List, depth int) {
	pos := 2
	min := 2
	max := 14
	if sfv.noCount {
		max -= 2
	}
	if sfv.noItem {
		max--
		min--
		pos--
	}
	slip.CheckArgCount(s, depth, f, args, min, max)
	sfv.item = args[0] // if noItem it doesn't matter so no harm is setting
	sfv.end = -1
	sfv.count = math.MaxInt
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			sfv.key = ResolveToCaller(s, args[pos+1], depth)
		case ":test":
			sfv.test = ResolveToCaller(s, args[pos+1], depth)
		case ":start":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sfv.start = int(num)
			} else {
				slip.TypePanic(s, depth, "start", args[pos+1], "fixnum")
			}
		case ":end":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sfv.end = int(num)
			} else if args[pos+1] == nil {
				sfv.end = -1
			} else {
				slip.TypePanic(s, depth, "end", args[pos+1], "fixnum")
			}
		case ":count":
			if sfv.noCount {
				slip.TypePanic(s, depth, "keyword", sym, ":key", ":test", ":start", ":end", ":from-end")
			}
			if num, ok := args[pos+1].(slip.Fixnum); ok {
				sfv.count = int(num)
			} else {
				slip.TypePanic(s, depth, "count", args[pos+1], "fixnum")
			}
		case ":from-end":
			sfv.fromEnd = args[pos+1] != nil
		default:
			if sfv.noCount {
				slip.TypePanic(s, depth, "keyword", sym, ":key", ":test", ":start", ":end", ":from-end")
			}
			slip.TypePanic(s, depth, "keyword", sym, ":key", ":test", ":start", ":end", ":from-end", ":count")
		}
	}
	if pos < len(args) {
		slip.NewPanic("extra arguments that are not keyword and value pairs")
	}
}

func (sfv *seqFunVars) setKeysIf(f slip.Object, s *slip.Scope, args slip.List, depth int) {
	if sfv.noCount {
		slip.CheckArgCount(s, depth, f, args, 2, 10)
	} else {
		slip.CheckArgCount(s, depth, f, args, 2, 12)
	}
	sfv.test = ResolveToCaller(s, args[0], depth)
	sfv.end = -1
	sfv.count = math.MaxInt
	pos := 2
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			sfv.key = ResolveToCaller(s, args[pos+1], depth)
		case ":start":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sfv.start = int(num)
			} else {
				slip.TypePanic(s, depth, "start", args[pos+1], "fixnum")
			}
		case ":end":
			if num, ok := args[pos+1].(slip.Fixnum); ok && 0 <= num {
				sfv.end = int(num)
			} else if args[pos+1] == nil {
				sfv.end = -1
			} else {
				slip.TypePanic(s, depth, "end", args[pos+1], "fixnum")
			}
		case ":count":
			if sfv.noCount {
				slip.TypePanic(s, depth, "keyword", sym, ":key", ":start", ":end", ":from-end")
			}
			if num, ok := args[pos+1].(slip.Fixnum); ok {
				sfv.count = int(num)
			} else {
				slip.TypePanic(s, depth, "count", args[pos+1], "fixnum")
			}
		case ":from-end":
			sfv.fromEnd = args[pos+1] != nil
		default:
			if sfv.noCount {
				slip.TypePanic(s, depth, "keyword", sym, ":key", ":start", ":end", ":from-end")
			}
			slip.TypePanic(s, depth, "keyword", sym, ":key", ":start", ":end", ":from-end", ":count")
		}
	}
	if pos < len(args) {
		slip.NewPanic("extra arguments that are not keyword and value pairs")
	}
}
