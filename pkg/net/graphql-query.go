// Copyright (c) 2025, Peter Ohler, All rights reserved.

package net

import (
	"bytes"
	"net/http"
	"time"

	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GraphqlQuery{Function: slip.Function{Name: "graphql-query", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "graphql-query",
			Args: []*slip.DocArg{
				{
					Name: "url",
					Type: "string",
					Text: "URL of the GraphQL server.",
				},
				{
					Name: "template",
					Type: "string",
					Text: "Template for the GraphQL query.",
				},
				{Name: "&optional"},
				{
					Name: "header",
					Type: "list",
					Text: `A list of lists to be used as the headers in the request.
(e.g., ((Content-Type" "application/graphql")))`,
				},
				{Name: "&key"},
				{
					Name: "timeout",
					Type: "real",
					Text: "The request timeout in seconds.",
				},
				{
					Name: "template-args",
					Type: "list",
					Text: "A list of arguments for the template.",
				},
				{
					Name: "allow-other-keys",
					Type: "object",
					Text: "Additional key value bindings for the template expansion.",
				},
			},
			Return: "bag",
			Text: `__graphql-query__ sends a GraphQL POST request to _url_. Responses
are returned as a __bag__ instance. The template provided is used as the _control_
argument to a call to the __format__ function. The &key arguments can be used with
the ~= directive which is a __slip__ extension to the __format__ directives.


The Content-Type header value of anything but application/json will be treated
as if application/graphql was set and not modification to the templated
content is made. If the Content-Type is application/json then the content is
wrapped in a JSON document. If no Content-Type is specified then the
Content-Type is set to application/graphql.
`,
			Examples: []string{
				`(graphql-query`,
				`  "http://example.com/graphql"`,
				`  "user(id:~S group:\"~=group~=\") {name}"`,
				`  :template-args '("id-123")`,
				`  :group "one") =>`,
				`{"data":{"user":{"name":"Fred"}}}`,
			},
		}, &Pkg)
}

// GraphqlQuery represents the graphql-query function.
type GraphqlQuery struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GraphqlQuery) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, -1)
	u := slip.MustBeString(args[0], "url")
	template := slip.MustBeString(args[1], "template")
	args = args[2:]
	var header http.Header
	if 0 < len(args) {
		if list, ok := args[0].(slip.List); ok {
			header = headerFromAssoc(s, list, depth)
			args = args[1:]
		} else if args[0] == nil {
			args = args[1:]
		}
	}
	targs := slip.List{slip.String(template)}
	ns := s.NewScope()
	client := &http.Client{}
	for i := 0; i < len(args); i += 2 {
		if key, ok := args[i].(slip.Symbol); ok && 1 < len(key) && key[0] == ':' {
			ns.Let(key[1:], args[i+1])
			switch key {
			case slip.Symbol(":template-args"):
				if list, ok := args[i+1].(slip.List); ok {
					targs = append(targs, list...)
				} else {
					slip.TypePanic(s, depth, ":template-args", args[i+1], "list")
				}
			case slip.Symbol(":timeout"):
				if r, ok := args[i+1].(slip.Real); ok && 0.0 <= r.RealValue() {
					client.Timeout = time.Duration(r.RealValue() * float64(time.Second))
				} else {
					slip.TypePanic(s, depth, ":timeout", args[i+1], "non-negative real")
				}
			}
		} else {
			slip.TypePanic(s, depth, ":key", args[i], "keyword")
		}
	}
	var ct string
	if header != nil {
		ct = header.Get("Content-Type")
	} else {
		header = http.Header{}
	}
	if len(ct) == 0 {
		ct = "application/graphql"
		header.Set("Content-Type", ct)
	}
	content := cl.FormatArgs(ns, targs, depth)
	if ct == "application/json" {
		content = []byte(oj.JSON(map[string]any{"query": string(content)}))
	}
	req, err := http.NewRequest("POST", u, bytes.NewReader(content))
	if err != nil {
		panic(err)
	}
	if header != nil {
		req.Header = header
	}
	var res *http.Response
	if res, err = client.Do(req); err != nil {
		panic(err)
	}
	defer func() { _ = res.Body.Close() }()
	bi := bag.Flavor().MakeInstance().(*flavors.Instance)
	bi.Init(s, slip.List{}, depth)
	bi.Any = oj.MustLoad(res.Body)

	return bi
}
