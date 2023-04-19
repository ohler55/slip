// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"io"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

var logger *flavors.Flavor

func init() {
	logger = flavors.DefFlavor(
		"logger",
		map[string]slip.Object{ // instance variables
			"level":      slip.Fixnum(1),
			"with-time":  nil, // boolean
			"with-level": slip.True,
			"colorize":   nil,
			"out":        nil,
			"json":       nil,
			"sen":        nil,
		},
		nil, // inherit
		slip.List{ // options
			slip.Symbol(":gettable-instance-variables"),
			slip.Symbol(":settable-instance-variables"),
			slip.Symbol(":inittable-instance-variables"),
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`Logs messages to *standard-output* with various options.`),
			},
		},
	)
	logger.DefMethod(":init", "", initCaller(true))
	logger.DefMethod(":error", "", errorCaller(true))
	logger.DefMethod(":warn", "", warnCaller(true))
	logger.DefMethod(":info", "", infoCaller(true))
	logger.DefMethod(":debug", "", debugCaller(true))
	logger.DefMethod(":write", "", writeCaller(true))
	logger.DefMethod(":shutdown", "", shutdownCaller(true))
}

// Logger returns the bag-flavor.
func Logger() *flavors.Flavor {
	return logger
}

type initCaller bool

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	queue := make(chan []byte, 100)
	obj.Any = queue
	go func() {
		wargs := slip.List{nil}
		for {
			message := <-queue
			if len(message) == 0 {
				break
			}
			wargs[0] = slip.String(message)
			obj.Receive(":write", wargs, 0)
		}
	}()
	return nil
}

func (caller initCaller) Docs() string {
	return `__:init__`
}

func logFormat(s *slip.Scope, level int, args slip.List) {
	limit, _ := s.Get("level").(slip.Fixnum)
	if int(limit) < level {
		return
	}
	if level < 0 {
		level = 0
	} else if 3 < level {
		level = 3
	}
	asJSON := s.Get("json") != nil
	asSEN := s.Get("sen") != nil
	message := cl.FormatArgs(s, args)
	var buf []byte
	if asJSON || asSEN {
		// TBD setup map
		// encode according to flags
		//
	} else {
		if s.Get("with-time") != nil {
			buf = time.Now().UTC().AppendFormat(buf, time.RFC3339Nano)
			buf = append(buf, ' ')
		}
		// TBD color
		if s.Get("with-level") != nil {
			buf = append(buf, "EWID"[level])
			buf = append(buf, ' ')
		}
		buf = append(buf, message...)
	}
	obj := s.Get("self").(*flavors.Instance)
	obj.Any.(chan []byte) <- buf
}

type errorCaller bool

func (caller errorCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 0, args)
	return nil
}

func (caller errorCaller) Docs() string {
	return `__:error__ _format_ &rest _args_
Log an error message if the logger _level_ is at or above 0.

`
}

type warnCaller bool

func (caller warnCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 1, args)
	return nil
}

func (caller warnCaller) Docs() string {
	return `__:warn__ _format_ &rest _args_
Log a warn message if the logger _level_ is at or above 1.

`
}

type infoCaller bool

func (caller infoCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 2, args)
	return nil
}

func (caller infoCaller) Docs() string {
	return `__:info__ _format_ &rest _args_
Log an info message if the logger _level_ is at or above 2.

`
}

type debugCaller bool

func (caller debugCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 3, args)
	return nil
}

func (caller debugCaller) Docs() string {
	return `__:debug__ _format_ &rest _args_
Log a debug message if the logger _level_ is at or above 3.

`
}

type writeCaller bool

func (caller writeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	out := s.Get("out")
	if out == nil {
		out = slip.StandardOutput
	}
	w, ok := out.(io.Writer)
	if !ok {
		slip.PanicType("stream", out, "output-stream", "nil")
	}
	var message slip.String
	if message, ok = args[0].(slip.String); !ok {
		slip.PanicType("message", args[0], "string")
	}
	_, _ = w.Write(append([]byte(message), '\n'))

	return nil
}

func (caller writeCaller) Docs() string {
	return `__:write__ _message_ => _nil_
  _message_ The message to write to the stream.
Write _message_ to _stream_.

`
}

type shutdownCaller bool

func (caller shutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	obj.Any.(chan []byte) <- nil
	return nil
}

func (caller shutdownCaller) Docs() string {
	return `__:shutdown__
Shutdown the logger.

`
}
