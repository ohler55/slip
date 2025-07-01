// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/ohler55/slip"
)

func getStrArg(arg slip.Object, use string) string {
	ss, ok := arg.(slip.String)
	if !ok {
		slip.PanicType(use, arg, "string")
	}
	return string(ss)
}

func getDurationArg(arg slip.Object, use string) time.Duration {
	sr, ok := arg.(slip.Real)
	if !ok {
		slip.PanicType(use, arg, "real")
	}
	return time.Duration(sr.RealValue() * float64(time.Second))
}

func getIntArg(arg slip.Object, use string) int64 {
	si, ok := arg.(slip.Integer)
	if !ok {
		slip.PanicType(use, arg, "integer")
	}
	return si.Int64()
}

func assocToHeader(value slip.Object, field string) http.Header {
	alist, ok := value.(slip.List)
	if !ok {
		slip.PanicType(field, value, "assoc")
	}
	header := http.Header{}
	for _, element := range alist {
		elist, ok2 := element.(slip.List)
		if !ok2 || len(elist) < 2 {
			slip.PanicType("assoc element", element, "list")
		}
		var (
			key    slip.String
			values []string
		)
		if key, ok = elist[0].(slip.String); !ok {
			slip.PanicType("header key", elist[0], "string")
		}
		for _, v := range elist[1:] {
			var ss slip.String
			if ss, ok = v.(slip.String); !ok {
				slip.PanicType("header value", v, "string")
			}
			values = append(values, string(ss))
		}
		header[string(key)] = values
	}
	return header
}

func methodDocFromFunc(method, funcName string, p *slip.Package) *slip.FuncDoc {
	fd := slip.DescribeFunction(slip.Symbol(funcName), p)
	md := slip.FuncDoc{Name: method}
	if fd != nil {
		md.Text = fmt.Sprintf("%s\n\n\nSee also: __%s__\n", strings.Replace(fd.Text, funcName, method, 1), funcName)
		if 1 < len(fd.Args) {
			md.Args = fd.Args[1:]
		}
		md.Return = fd.Return
		if 0 < len(fd.Examples) {
			// Make a copy to be modified later.
			md.Examples = make([]string, len(fd.Examples))
			copy(md.Examples, fd.Examples)
		}
	}
	return &md
}
