// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"time"

	"github.com/ohler55/slip"
)

var lineColRe = regexp.MustCompile(` at (\d+):(\d+)$`)

func init() {
	RegisterHandler("swank:ping", handlePing)
	RegisterHandler("swank:compile-string-for-emacs", handleCompileString)
	RegisterHandler("swank:compile-file-for-emacs", handleCompileFile)
	RegisterHandler("swank:load-file", handleLoadFile)
	RegisterHandler("swank:update-indentation-information", handleUpdateIndentation)
}

// handlePing echoes back the tag for keepalive.
func handlePing(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}
	return args[0]
}

// handleCompileString compiles a code string (C-c C-c in SLIME).
// Args: (string buffer position filename policy)
func handleCompileString(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return compilationResult(nil, false, 0)
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return compilationResult(nil, false, 0)
	}

	// Extract buffer name and position for error locations.
	var bufferName string
	var bufferOffset int
	if len(args) > 1 {
		if s, ok := args[1].(slip.String); ok {
			bufferName = string(s)
		}
	}
	if len(args) > 2 {
		bufferOffset = extractOffset(args[2])
	}

	start := time.Now()
	notes, success := compileSource(c, string(source), bufferName, bufferOffset)
	duration := time.Since(start).Seconds()

	return compilationResult(notes, success, duration)
}

// handleCompileFile compiles a file (C-c C-k in SLIME).
// Args: (filename load-p &rest options)
func handleCompileFile(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return compilationResult(nil, false, 0)
	}

	filename, ok := args[0].(slip.String)
	if !ok {
		return compilationResult(nil, false, 0)
	}

	loadP := len(args) > 1 && args[1] != nil

	buf, err := os.ReadFile(string(filename))
	if err != nil {
		note := makeNote(":error", fmt.Sprintf("%s", err), string(filename), 0)
		return compilationResult(slip.List{note}, false, 0)
	}

	start := time.Now()
	notes, success := compileSource(c, string(buf), string(filename), 0)
	duration := time.Since(start).Seconds()

	if success && loadP {
		// Evaluate after successful compilation.
		evalNotes, evalOK := evalSource(c, string(buf))
		if !evalOK {
			notes = append(notes, evalNotes...)
			success = false
		}
	}

	return compilationResult(notes, success, duration)
}

// handleLoadFile loads a file (C-c C-l in SLIME).
// Args: (filename)
func handleLoadFile(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.String("nil")
	}

	filename, ok := args[0].(slip.String)
	if !ok {
		return slip.String("nil")
	}

	result := evalWithCapture(c, fmt.Sprintf("(load %q)", string(filename)), false)
	return slip.String(slip.ObjectString(result))
}

// handleUpdateIndentation returns nil; SLIP has no custom indentation rules.
func handleUpdateIndentation(c *Connection, args slip.List) slip.Object {
	return nil
}

// compileSource parses and compiles source code, returning notes and success.
func compileSource(c *Connection, source, locationName string, baseOffset int) (notes slip.List, success bool) {
	success = true

	// Parse phase.
	var code slip.Code
	func() {
		defer func() {
			if r := recover(); r != nil {
				success = false
				notes = append(notes, recoverToNote(r, locationName, baseOffset))
			}
		}()
		code = slip.Read([]byte(source), c.scope)
	}()
	if !success {
		return
	}

	// Compile phase.
	func() {
		defer func() {
			if r := recover(); r != nil {
				success = false
				notes = append(notes, recoverToNote(r, locationName, baseOffset))
			}
		}()
		code.Compile()
	}()

	return
}

// evalSource evaluates already-compiled source, returning notes on failure.
func evalSource(c *Connection, source string) (notes slip.List, success bool) {
	success = true
	func() {
		defer func() {
			if r := recover(); r != nil {
				success = false
				notes = append(notes, recoverToNote(r, "", 0))
			}
		}()

		// See rpc_eval.go: primitives like princ read slip.StandardOutput
		// as a Go global, so the swap must be serialized.
		withStandardOutput(c.outputStream, func() {
			code := slip.Read([]byte(source), c.scope)
			code.Compile()
			code.Eval(c.scope, nil)
		})
	}()
	return
}

// recoverToNote converts a recovered panic to a SLIME compiler note.
func recoverToNote(r any, locationName string, baseOffset int) slip.Object {
	msg := fmt.Sprintf("%v", r)
	if p, ok := r.(*slip.Panic); ok {
		msg = p.Message
	}

	// Try to extract line:col from "... at LINE:COL" parse errors.
	var position int
	if m := lineColRe.FindStringSubmatch(msg); m != nil {
		line, _ := strconv.Atoi(m[1])
		col, _ := strconv.Atoi(m[2])
		// Approximate byte offset: assume ~80 chars per line.
		position = baseOffset + (line-1)*80 + col
	}

	return makeNote(":error", msg, locationName, position)
}

// makeNote builds a SLIME compiler note plist.
func makeNote(severity, message, locationName string, position int) slip.Object {
	var location slip.Object
	switch {
	case locationName != "" && position > 0:
		location = slip.List{
			slip.Symbol(":location"),
			slip.List{slip.Symbol(":buffer"), slip.String(locationName)},
			slip.List{slip.Symbol(":offset"), slip.Fixnum(position)},
			nil,
		}
	case locationName != "":
		location = slip.List{
			slip.Symbol(":location"),
			slip.List{slip.Symbol(":buffer"), slip.String(locationName)},
			slip.List{slip.Symbol(":offset"), slip.Fixnum(1)},
			nil,
		}
	default:
		location = slip.List{slip.Symbol(":error"), slip.String("no location")}
	}

	return slip.List{
		slip.Symbol(":message"), slip.String(message),
		slip.Symbol(":severity"), slip.Symbol(severity),
		slip.Symbol(":location"), location,
		slip.Symbol(":references"), nil,
		slip.Symbol(":source-context"), nil,
	}
}

// compilationResult builds the standard SLIME compilation result plist.
func compilationResult(notes slip.List, success bool, duration float64) slip.Object {
	var successVal slip.Object
	if success {
		successVal = slip.True
	}

	if notes == nil {
		notes = slip.List{}
	}

	return slip.List{
		slip.Symbol(":notes"), notes,
		slip.Symbol(":successp"), successVal,
		slip.Symbol(":duration"), slip.DoubleFloat(duration),
		slip.Symbol(":loadp"), successVal,
		slip.Symbol(":faslfile"), nil,
	}
}

// extractOffset extracts a byte offset from the position argument.
// SLIME sends position as (:position N) or just N.
func extractOffset(arg slip.Object) int {
	if n, ok := arg.(slip.Fixnum); ok {
		return int(n)
	}
	if list, ok := arg.(slip.List); ok {
		for i := 0; i < len(list)-1; i += 2 {
			if sym, ok := list[i].(slip.Symbol); ok && sym == ":position" {
				if n, ok := list[i+1].(slip.Fixnum); ok {
					return int(n)
				}
			}
		}
	}
	return 0
}
