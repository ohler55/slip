// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("$/alive/eval", handleEval)
}

// handleEval handles the $/alive/eval request.
// Params: {"text": "<lisp code>", "package": "<optional package name>"}
// Returns: {"text": "<result>"}
func handleEval(c *Connection, params interface{}) (interface{}, error) {
	p, ok := params.(map[string]interface{})
	if !ok {
		return nil, fmt.Errorf("invalid params")
	}

	text, ok := p["text"].(string)
	if !ok || text == "" {
		return nil, fmt.Errorf("missing text parameter")
	}

	// Optional package parameter
	if pkgName, ok := p["package"].(string); ok && pkgName != "" {
		if pkg := slip.FindPackage(pkgName); pkg != nil {
			c.SetPackage(pkg)
		}
	}

	result, output := evalWithCapture(c, text)

	response := map[string]interface{}{
		"text": slip.ObjectString(result),
	}

	if output != "" {
		response["output"] = output
	}

	return response, nil
}

// evalWithCapture evaluates code and captures output.
func evalWithCapture(c *Connection, source string) (result slip.Object, output string) {
	defer func() {
		if r := recover(); r != nil {
			if p, ok := r.(*slip.Panic); ok {
				result = slip.String(fmt.Sprintf("Error: %s", p.Message))
			} else {
				result = slip.String(fmt.Sprintf("Error: %v", r))
			}
		}
		// Capture any output
		output = c.outputStream.Flush()
	}()

	code := slip.Read([]byte(source), c.scope)
	if len(code) == 0 {
		return nil, ""
	}

	code.Compile()

	var form slip.Object
	if len(code) > 0 {
		form = code[0]
	}

	result = code.Eval(c.scope, nil)

	// Update history
	c.UpdateHistory(result, form)

	return result, ""
}
