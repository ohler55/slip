// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// Combination is the combined method functions for a class.
type Combination struct {
	From    Class
	Primary Caller
	Before  Caller
	After   Caller
	Wrap    Caller
}

// Empty return true if there are no daemons in the combination.
func (c *Combination) Empty() bool {
	return c.Primary == nil && c.Wrap == nil && c.Before == nil && c.After == nil
}

// Simplify by returning a representation of the combination.
func (c *Combination) Simplify() any {
	simple := map[string]any{
		"from": c.From.Name(),
	}
	if c.Wrap != nil {
		simple["whopper"] = true
	}
	if c.Before != nil {
		simple["before"] = true
	}
	if c.Primary != nil {
		simple["primary"] = true
	}
	if c.After != nil {
		simple["after"] = true
	}
	return simple
}
