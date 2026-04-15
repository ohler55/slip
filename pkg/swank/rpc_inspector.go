// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:init-inspector", handleInitInspector)
	RegisterHandler("swank:inspect-in-emacs", handleInspectInEmacs)
	RegisterHandler("swank:inspector-nth-part", handleInspectorNthPart)
	RegisterHandler("swank:inspect-nth-part", handleInspectNthPart)
	RegisterHandler("swank:inspector-range", handleInspectorRange)
	RegisterHandler("swank:inspector-pop", handleInspectorPop)
	RegisterHandler("swank:inspector-next", handleInspectorNext)
	RegisterHandler("swank:inspector-reinspect", handleInspectorReinspect)
	RegisterHandler("swank:quit-inspector", handleQuitInspector)
	RegisterHandler("swank:inspector-call-nth-action", handleInspectorCallNthAction)
}

// inspectorAction is a callable action registered by inspector content builders.
type inspectorAction struct {
	fn      func() slip.Object
	refresh bool
}

// Inspector holds the state for object inspection.
type Inspector struct {
	current slip.Object       // currently inspected object
	parts   []slip.Object     // inspectable parts of current object
	actions []inspectorAction // callable actions (buttons in SLIME)
	history []slip.Object     // navigation history (back)
	forward []slip.Object     // navigation history (forward)
}

// NewInspector creates a new inspector instance.
func NewInspector() *Inspector {
	return &Inspector{
		parts:   make([]slip.Object, 0),
		history: make([]slip.Object, 0),
		forward: make([]slip.Object, 0),
	}
}

// getInspector returns the connection's inspector, creating one if needed.
func getInspector(c *Connection) *Inspector {
	if c.inspector == nil {
		c.inspector = NewInspector()
	}
	return c.inspector
}

// handleInitInspector initializes the inspector.
func handleInitInspector(c *Connection, args slip.List) slip.Object {
	c.inspector = NewInspector()
	return nil
}

// handleInspectInEmacs inspects an object.
// Args: (value) or (form)
// Response: (:title :id :content :length)
func handleInspectInEmacs(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	insp := getInspector(c)

	// If there's a current object, push it to history
	if insp.current != nil {
		insp.history = append(insp.history, insp.current)
		insp.forward = nil // clear forward history
	}

	// The argument might be a string to evaluate or an object
	var obj slip.Object
	if s, ok := args[0].(slip.String); ok {
		// Evaluate the string
		code := slip.Read([]byte(s), c.scope)
		if len(code) > 0 {
			code.Compile()
			obj = code.Eval(c.scope, nil)
		}
	} else {
		obj = args[0]
	}

	insp.current = obj
	return buildInspectorContent(c, insp)
}

// handleInspectorNthPart returns the nth inspectable part.
func handleInspectorNthPart(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	n, ok := args[0].(slip.Fixnum)
	if !ok {
		return nil
	}

	insp := getInspector(c)
	idx := int(n)
	if idx < 0 || idx >= len(insp.parts) {
		return nil
	}

	return insp.parts[idx]
}

// handleInspectNthPart inspects the nth part (drills down).
func handleInspectNthPart(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	n, ok := args[0].(slip.Fixnum)
	if !ok {
		return nil
	}

	insp := getInspector(c)
	idx := int(n)
	if idx < 0 || idx >= len(insp.parts) {
		return nil
	}

	// Push current to history
	if insp.current != nil {
		insp.history = append(insp.history, insp.current)
		insp.forward = nil
	}

	insp.current = insp.parts[idx]
	return buildInspectorContent(c, insp)
}

// handleInspectorRange returns a range of content.
// Args: (start end)
func handleInspectorRange(c *Connection, args slip.List) slip.Object {
	// For now, return full content - SLIME will handle display
	insp := getInspector(c)
	return buildInspectorContent(c, insp)
}

// handleInspectorPop goes back in history.
func handleInspectorPop(c *Connection, args slip.List) slip.Object {
	insp := getInspector(c)

	if len(insp.history) == 0 {
		return nil
	}

	// Push current to forward
	if insp.current != nil {
		insp.forward = append(insp.forward, insp.current)
	}

	// Pop from history
	last := len(insp.history) - 1
	insp.current = insp.history[last]
	insp.history = insp.history[:last]

	return buildInspectorContent(c, insp)
}

// handleInspectorNext goes forward in history.
func handleInspectorNext(c *Connection, args slip.List) slip.Object {
	insp := getInspector(c)

	if len(insp.forward) == 0 {
		return nil
	}

	// Push current to history
	if insp.current != nil {
		insp.history = append(insp.history, insp.current)
	}

	// Pop from forward
	last := len(insp.forward) - 1
	insp.current = insp.forward[last]
	insp.forward = insp.forward[:last]

	return buildInspectorContent(c, insp)
}

// handleInspectorReinspect refreshes the current inspection.
func handleInspectorReinspect(c *Connection, args slip.List) slip.Object {
	insp := getInspector(c)
	return buildInspectorContent(c, insp)
}

// handleQuitInspector closes the inspector.
func handleQuitInspector(c *Connection, args slip.List) slip.Object {
	c.inspector = nil
	return nil
}

// handleInspectorCallNthAction invokes an inspector action by index.
// Args: (index)
func handleInspectorCallNthAction(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	n, ok := args[0].(slip.Fixnum)
	if !ok {
		return nil
	}

	insp := getInspector(c)
	idx := int(n)
	if idx < 0 || idx >= len(insp.actions) {
		return nil
	}

	action := insp.actions[idx]
	action.fn()

	if action.refresh {
		return buildInspectorContent(c, insp)
	}
	return nil
}

// buildInspectorContent builds the inspector response for the current object.
// Response format: (:title "..." :id N :content (...) :length N)
func buildInspectorContent(c *Connection, insp *Inspector) slip.Object {
	if insp.current == nil {
		return slip.List{
			slip.Symbol(":title"), slip.String("nil"),
			slip.Symbol(":id"), slip.Fixnum(0),
			slip.Symbol(":content"), slip.List{slip.String("nil")},
			slip.Symbol(":length"), slip.Fixnum(1),
		}
	}

	// Clear parts and actions lists
	insp.parts = make([]slip.Object, 0)
	insp.actions = nil

	// Get type information
	title := getObjectTitle(insp.current)
	typeStr := getObjectType(insp.current)

	// Build content
	content := buildContent(c, insp, insp.current)

	return slip.List{
		slip.Symbol(":title"), slip.String(title),
		slip.Symbol(":type"), slip.String(typeStr),
		slip.Symbol(":id"), slip.Fixnum(0),
		slip.Symbol(":content"), content,
		slip.Symbol(":length"), slip.Fixnum(len(insp.parts)),
	}
}

// getObjectTitle returns a title string for the object.
func getObjectTitle(obj slip.Object) string {
	if obj == nil {
		return "nil"
	}
	// For strings, use the raw value to avoid double-quoting when the
	// title is wrapped in slip.String for the wire response.
	if str, ok := obj.(slip.String); ok {
		s := string(str)
		if len(s) > 50 {
			s = s[:47] + "..."
		}
		return s
	}
	// Use short representation
	s := slip.ObjectString(obj)
	if len(s) > 50 {
		s = s[:47] + "..."
	}
	return s
}

// getObjectType returns the type string for the object.
func getObjectType(obj slip.Object) string {
	if obj == nil {
		return "null"
	}
	h := obj.Hierarchy()
	if len(h) > 0 {
		return string(h[0])
	}
	return "object"
}

// buildContent builds the inspector content list.
// Format: ("label" (:value obj idx) (:newline) ...)
func buildContent(c *Connection, insp *Inspector, obj slip.Object) slip.List {
	var content slip.List

	// Add type line
	content = append(content, slip.String("Type: "))
	content = append(content, slip.String(getObjectType(obj)))
	content = append(content, slip.Symbol(":newline"))

	// Handle different object types
	switch v := obj.(type) {
	case slip.List:
		content = appendListContent(content, insp, v)
	case slip.Fixnum, slip.Float, *slip.Ratio:
		content = appendNumberContent(content, obj)
	case slip.String:
		content = appendStringContent(content, v)
	case slip.Symbol:
		content = appendSymbolContent(content, c, v)
	case *slip.Lambda:
		content = appendLambdaContent(content, insp, v)
	case *slip.FuncInfo:
		content = appendFuncInfoContent(content, insp, v)
	case slip.Describer:
		content = appendDescriberContent(content, insp, v)
	default:
		// Generic inspection using Simplify if available
		if s, ok := obj.(interface{ Simplify() any }); ok {
			content = appendSimplifiedContent(content, insp, s.Simplify())
		} else {
			content = append(content, slip.Symbol(":newline"))
			content = append(content, slip.String("Value: "))
			content = append(content, slip.String(slip.ObjectString(obj)))
		}
	}

	return content
}

// appendListContent adds list inspection content.
func appendListContent(content slip.List, insp *Inspector, list slip.List) slip.List {
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String(fmt.Sprintf("Length: %d", len(list))))
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String("Elements:"))
	content = append(content, slip.Symbol(":newline"))

	for i, elem := range list {
		idx := len(insp.parts)
		insp.parts = append(insp.parts, elem)

		content = append(content, slip.String(fmt.Sprintf("  [%d]: ", i)))
		content = append(content, slip.List{
			slip.Symbol(":value"),
			slip.String(truncateString(slip.ObjectString(elem), 60)),
			slip.Fixnum(idx),
		})
		content = append(content, slip.Symbol(":newline"))
	}

	return content
}

// appendNumberContent adds number inspection content.
func appendNumberContent(content slip.List, obj slip.Object) slip.List {
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String("Value: "))
	content = append(content, slip.String(slip.ObjectString(obj)))

	// Add additional representations for fixnum
	if n, ok := obj.(slip.Fixnum); ok {
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String(fmt.Sprintf("Hex: #x%X", int64(n))))
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String(fmt.Sprintf("Octal: #o%o", int64(n))))
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String(fmt.Sprintf("Binary: #b%b", int64(n))))
	}

	return content
}

// appendStringContent adds string inspection content.
func appendStringContent(content slip.List, s slip.String) slip.List {
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String(fmt.Sprintf("Length: %d", len(s))))
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String("Contents:"))
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String(string(s)))

	return content
}

// appendSymbolContent adds symbol inspection content.
func appendSymbolContent(content slip.List, c *Connection, sym slip.Symbol) slip.List {
	name := string(sym)
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String("Name: "))
	content = append(content, slip.String(name))

	// Check for function binding
	if fi := c.currentPkg.GetFunc(strings.ToLower(name)); fi != nil {
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String("Function: bound"))
	}

	// Check for value binding
	if vv := c.currentPkg.GetVarVal(strings.ToLower(name)); vv != nil {
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String("Value: "))
		content = append(content, slip.String(slip.ObjectString(vv.Value())))
	}

	return content
}

// appendLambdaContent adds lambda inspection content.
func appendLambdaContent(content slip.List, insp *Inspector, lam *slip.Lambda) slip.List {
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String("Lambda-List: ("))
	if lam.Doc != nil {
		for i, arg := range lam.Doc.Args {
			if i > 0 {
				content = append(content, slip.String(" "))
			}
			content = append(content, slip.String(arg.Name))
		}
	}
	content = append(content, slip.String(")"))
	content = append(content, slip.Symbol(":newline"))

	if len(lam.Forms) > 0 {
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String("Body:"))
		content = append(content, slip.Symbol(":newline"))

		for i, form := range lam.Forms {
			idx := len(insp.parts)
			insp.parts = append(insp.parts, form)

			content = append(content, slip.String(fmt.Sprintf("  [%d]: ", i)))
			content = append(content, slip.List{
				slip.Symbol(":value"),
				slip.String(truncateString(slip.ObjectString(form), 60)),
				slip.Fixnum(idx),
			})
			content = append(content, slip.Symbol(":newline"))
		}
	}

	return content
}

// appendFuncInfoContent adds function info inspection content.
func appendFuncInfoContent(content slip.List, insp *Inspector, fi *slip.FuncInfo) slip.List {
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String("Name: "))
	content = append(content, slip.String(fi.Name))
	content = append(content, slip.Symbol(":newline"))

	content = append(content, slip.String("Kind: "))
	content = append(content, slip.String(string(fi.Kind)))
	content = append(content, slip.Symbol(":newline"))

	if fi.Pkg != nil {
		content = append(content, slip.String("Package: "))
		content = append(content, slip.String(fi.Pkg.Name))
		content = append(content, slip.Symbol(":newline"))
	}

	if fi.Doc != nil {
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String("Lambda-List: ("))
		for i, arg := range fi.Doc.Args {
			if i > 0 {
				content = append(content, slip.String(" "))
			}
			content = append(content, slip.String(arg.Name))
		}
		content = append(content, slip.String(")"))
		content = append(content, slip.Symbol(":newline"))

		if fi.Doc.Return != "" {
			content = append(content, slip.String("Return: "))
			content = append(content, slip.String(fi.Doc.Return))
			content = append(content, slip.Symbol(":newline"))
		}

		if fi.Doc.Text != "" {
			content = append(content, slip.Symbol(":newline"))
			content = append(content, slip.String("Documentation:"))
			content = append(content, slip.Symbol(":newline"))
			// Strip markdown and add
			docText := string(slip.AppendDoc(nil, fi.Doc.Text, 2, 80, false))
			content = append(content, slip.String(docText))
		}
	}

	return content
}

// appendDescriberContent adds content for objects implementing Describer.
func appendDescriberContent(content slip.List, insp *Inspector, d slip.Describer) slip.List {
	// Use the object's Describe method
	desc := string(d.Describe(nil, 0, 80, false))
	content = append(content, slip.Symbol(":newline"))
	content = append(content, slip.String(desc))
	return content
}

// appendSimplifiedContent adds content from a simplified object.
func appendSimplifiedContent(content slip.List, insp *Inspector, simplified any) slip.List {
	switch v := simplified.(type) {
	case map[string]any:
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String("Slots:"))
		content = append(content, slip.Symbol(":newline"))

		for key, val := range v {
			content = append(content, slip.String(fmt.Sprintf("  %s: ", key)))
			content = append(content, slip.String(fmt.Sprintf("%v", val)))
			content = append(content, slip.Symbol(":newline"))
		}
	default:
		content = append(content, slip.Symbol(":newline"))
		content = append(content, slip.String(fmt.Sprintf("Simplified: %v", v)))
	}

	return content
}

// truncateString truncates a string to maxLen, adding "..." if truncated.
func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
