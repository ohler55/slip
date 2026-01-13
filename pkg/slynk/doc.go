// Copyright (c) 2025, Peter Ohler, All rights reserved.

// Package slynk implements a Slynk server for SLY integration.
//
// Slynk is the protocol used by SLY (Sylvester the Cat's Common Lisp IDE)
// to communicate with Lisp implementations. SLY is a fork of SLIME with
// enhanced features including multiple REPLs, stickers, and improved completion.
//
// # Starting the Server
//
// From SLIP:
//
//	(slynk-server :port 4005)
//
// Or from Go:
//
//	server := slynk.NewServer(scope)
//	server.Start(":4005")
//
// # Connecting from Emacs
//
// Once the server is running, connect from Emacs with:
//
//	M-x sly-connect RET localhost RET 4005 RET
//
// # Protocol
//
// The Slynk wire protocol is identical to Swank:
//   - 6 ASCII hex characters encoding the payload length
//   - UTF-8 encoded S-expression payload
//
// # Channels and Multiple REPLs
//
// Unlike Swank, Slynk supports channels - bidirectional streams that enable
// multiple concurrent REPLs. Each channel has its own evaluation scope,
// package context, and history.
//
// # Flex Completion
//
// Slynk provides enhanced completion with scoring and classification:
//   - Fuzzy matching with configurable scoring
//   - Symbol classification (function, variable, macro, class)
//   - Match chunk highlighting for UI display
package slynk
