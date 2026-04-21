// Copyright (c) 2025, Peter Ohler, All rights reserved.

// Package swank implements a Swank server for SLIME integration.
//
// Swank is the protocol used by SLIME (Superior Lisp Interaction Mode for Emacs)
// to communicate with Lisp implementations. This package provides a server that
// allows SLIP to be used interactively from Emacs.
//
// # Starting the Server
//
// From SLIP:
//
//	(swank-server :port 4005)
//
// Or from Go:
//
//	server := swank.NewServer(scope)
//	server.Start(":4005")
//
// # Connecting from Emacs
//
// Once the server is running, connect from Emacs with:
//
//	M-x slime-connect RET localhost RET 4005 RET
//
// # Protocol
//
// The Swank wire protocol uses length-prefixed S-expressions:
//   - 6 ASCII hex characters encoding the payload length
//   - UTF-8 encoded S-expression payload
//
// Messages are either RPC requests (:emacs-rex) from SLIME or responses/events
// from the server (:return, :write-string, :new-package, etc.).
package swank
