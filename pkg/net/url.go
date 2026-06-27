// Copyright (c) 2026, Peter Ohler, All rights reserved.

package net

import (
	"net/url"
	"strconv"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	urlFlavor *flavors.Flavor
)

func defURL() *flavors.Flavor {
	urlFlavor = flavors.DefFlavor("url-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":url"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A url-flavor instances represents a URL.`),
			},
		},
		&Pkg,
	)
	urlFlavor.Final = true
	urlFlavor.DefMethod(":init", "", urlInitCaller{})
	urlFlavor.DefMethod(":scheme", "", urlSchemeCaller{})
	urlFlavor.DefMethod(":host", "", urlHostCaller{})
	urlFlavor.DefMethod(":port", "", urlPortCaller{})
	urlFlavor.DefMethod(":path", "", urlPathCaller{})
	urlFlavor.DefMethod(":query", "", urlQueryCaller{})
	urlFlavor.DefMethod(":user", "", urlUserCaller{})
	urlFlavor.DefMethod(":password", "", urlPasswordCaller{})
	urlFlavor.DefMethod(":string", "", urlStringCaller{})

	return urlFlavor
}

type urlInitCaller struct{}

func (caller urlInitCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	if val, has := slip.GetArgsKeyValue(args, slip.Symbol(":url")); has {
		var err error
		if obj.Any, err = url.Parse(slip.MustBeString(val, ":url")); err != nil {
			slip.ErrorPanic(s, depth, "URL parse error. %s", err)
		}
	} else {
		slip.ErrorPanic(s, depth, "missing :url argument.")
	}
	return nil
}

func (caller urlInitCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Sets the initial value when _make-instance_ is called. The _:url_ argument is required.",
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":url",
				Type: "string",
				Text: `The string to parse as a URL.`,
			},
		},
	}
}

type urlSchemeCaller struct{}

func (caller urlSchemeCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":scheme", len(args), 0, 0)

	return slip.String((self.Any.(*url.URL)).Scheme)
}

func (caller urlSchemeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":scheme",
		Text:   `Returns the scheme of the URL.`,
		Return: "string",
	}
}

type urlHostCaller struct{}

func (caller urlHostCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":host", len(args), 0, 0)

	return slip.String((self.Any.(*url.URL)).Hostname())
}

func (caller urlHostCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":host",
		Text:   `Returns the host of the URL.`,
		Return: "string",
	}
}

type urlPortCaller struct{}

func (caller urlPortCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":port", len(args), 0, 0)

	// Initial parse will fail if an invalid port is used so no need to check
	// for an error here. Worse case is zero is returned.
	port, _ := strconv.Atoi((self.Any.(*url.URL)).Port())

	return slip.Fixnum(port)
}

func (caller urlPortCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":port",
		Text:   `Returns the port of the URL.`,
		Return: "fixnum",
	}
}

type urlPathCaller struct{}

func (caller urlPathCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":path", len(args), 0, 0)

	return slip.String((self.Any.(*url.URL)).Path)
}

func (caller urlPathCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":path",
		Text:   `Returns the path of the URL.`,
		Return: "string",
	}
}

type urlQueryCaller struct{}

func (caller urlQueryCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":query", len(args), 0, 0)
	var query slip.List
	for k, v := range (self.Any.(*url.URL)).Query() {
		qv := make(slip.List, len(v)+1)
		qv[0] = slip.String(k)
		for i, sv := range v {
			qv[i+1] = slip.String(sv)
		}
		query = append(query, qv)
	}
	return query
}

func (caller urlQueryCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":query",
		Text:   `Returns the query of the URL as an association list with the cdr of each being a list of values.`,
		Return: "list",
	}
}

type urlUserCaller struct{}

func (caller urlUserCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":user", len(args), 0, 0)
	if ui := (self.Any.(*url.URL)).User; ui != nil {
		result = slip.String(ui.Username())
	}
	return
}

func (caller urlUserCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":user",
		Text:   `Returns the user of the URL.`,
		Return: "string",
	}
}

type urlPasswordCaller struct{}

func (caller urlPasswordCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":password", len(args), 0, 0)
	if ui := (self.Any.(*url.URL)).User; ui != nil {
		password, _ := ui.Password()
		result = slip.String(password)
	}
	return
}

func (caller urlPasswordCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":password",
		Text:   `Returns the password of the URL.`,
		Return: "string",
	}
}

type urlStringCaller struct{}

func (caller urlStringCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":string", len(args), 0, 0)

	return slip.String((self.Any.(*url.URL)).String())
}

func (caller urlStringCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":string",
		Text:   `Returns a string representation of the URL.`,
		Return: "string",
	}
}

// MakeURL makes a new url.
func MakeURL(u *url.URL) (inst *flavors.Instance) {
	inst = urlFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = u
	return
}
