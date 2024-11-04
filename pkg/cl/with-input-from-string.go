// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithInputFromString{
				Open: Open{
					Function: slip.Function{Name: "with-input-from-string", Args: args, SkipEval: []bool{true}},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "with-input-from-string",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: `A list of (var string &key index start end).`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__with-input-from-string__ evaluates the _forms_ after creating a stream from the
_string_ subset defined by the _start_ and _end_ if provided. If provided, _index_ must be a placer
which is set when the function returns.`,
			Examples: []string{
				`(let (i) (list (with-input-from-string (s "abc def" :index i) (read s)) i)) => abc 4`,
			},
		}, &slip.CLPkg)
}

// WithInputFromString represents the with-input-from-string function.
type WithInputFromString struct {
	Open
}

// Call the function with the arguments provided.
func (f *WithInputFromString) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	subArgs, ok := args[0].(slip.List)
	if !ok || len(subArgs) < 2 {
		slip.PanicType("args", args[0], "list of (var string &key index start end)")
	}
	var sym slip.Symbol
	if sym, ok = subArgs[0].(slip.Symbol); !ok {
		slip.PanicType("var", subArgs[0], "symbol")
	}
	var (
		ra    []rune
		start int
		end   int
		place slip.Object
	)
	if ss, ok2 := slip.EvalArg(s, subArgs, 1, depth).(slip.String); ok2 {
		ra = []rune(ss)
		end = len(ra)
	} else {
		slip.PanicType("string", subArgs[1], "string")
	}
	d2 := depth + 1
	subArgs = subArgs[2:]
	if v, has := slip.GetArgsKeyValue(subArgs, slip.Symbol(":start")); has {
		if num, ok2 := s.Eval(v, depth).(slip.Fixnum); ok2 && 0 <= num && int(num) < len(ra) {
			start = int(num)
		} else {
			slip.PanicType(":start", v, (fmt.Sprintf("fixnum between 0 and %d", len(ra))))
		}
	}
	if v, has := slip.GetArgsKeyValue(subArgs, slip.Symbol(":end")); has {
		if num, ok2 := s.Eval(v, depth).(slip.Fixnum); ok2 && 0 <= num && int(num) < len(ra) && start <= int(num) {
			end = int(num)
		} else {
			slip.PanicType(":end", v, (fmt.Sprintf("fixnum between %d and %d", start, len(ra))))
		}
	}
	if v, has := slip.GetArgsKeyValue(subArgs, slip.Symbol(":index")); has {
		place = v
	}

	reader := strings.NewReader(string(ra[start:end]))
	s2 := s.NewScope()
	s2.Let(sym, &slip.InputStream{Reader: reader})
	args = args[1:]
	for i := range args {
		result = slip.EvalArg(s2, args, i, d2)
	}
	if place != nil {
		str := string(ra[start:end])
		used := []rune(str[:reader.Len()])
		placeExprValue(s, place, slip.Fixnum(start+len(used)), d2)
	}
	return
}
