// Copyright (c) 2025, Peter Ohler, All rights reserved.

// Package alive implements an LSP server for VSCode's Alive extension.
//
// The Alive extension provides Common Lisp development support in VSCode.
// This package implements the Language Server Protocol (LSP) to enable
// SLIP development with features like evaluation, completion, hover
// documentation, and macro expansion.
//
// # Usage
//
// Start the server from SLIP:
//
//	(alive:alive-server :port 4006)
//
// Then configure VSCode's Alive extension to connect to localhost:4006.
//
// # Protocol
//
// The server uses LSP over TCP with JSON-RPC 2.0 messages:
//
//	Content-Length: <length>\r\n
//	\r\n
//	{"jsonrpc":"2.0","id":1,"method":"...","params":{...}}
//
// # Supported Methods
//
// Standard LSP methods:
//   - initialize / initialized / shutdown / exit
//   - textDocument/completion
//   - textDocument/hover
//   - textDocument/didOpen, didClose, didChange
//
// Custom Alive methods:
//   - $/alive/eval - Evaluate Lisp expressions
//   - $/alive/listPackages - List available packages
//   - $/alive/macroexpand - Expand macros
package alive
