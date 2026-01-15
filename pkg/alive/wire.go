// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// JSONRPCMessage represents a JSON-RPC 2.0 message.
type JSONRPCMessage struct {
	JSONRPC string      `json:"jsonrpc"`
	ID      interface{} `json:"id,omitempty"`
	Method  string      `json:"method,omitempty"`
	Params  interface{} `json:"params,omitempty"`
	Result  interface{} `json:"result,omitempty"`
	Error   *RPCError   `json:"error,omitempty"`
}

// RPCError represents a JSON-RPC 2.0 error.
type RPCError struct {
	Code    int         `json:"code"`
	Message string      `json:"message"`
	Data    interface{} `json:"data,omitempty"`
}

// Standard JSON-RPC error codes.
const (
	ParseError           = -32700
	InvalidRequest       = -32600
	MethodNotFound       = -32601
	InvalidParams        = -32602
	InternalError        = -32603
	ServerNotInitialized = -32002
	RequestCancelled     = -32800
)

// ReadMessage reads an LSP message with Content-Length framing.
// LSP format: "Content-Length: N\r\n\r\n{JSON payload}"
func ReadMessage(r io.Reader) (*JSONRPCMessage, error) {
	br, ok := r.(*bufio.Reader)
	if !ok {
		br = bufio.NewReader(r)
	}

	// Read headers until blank line
	var contentLength int64
	for {
		line, err := br.ReadString('\n')
		if err != nil {
			return nil, err
		}

		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			break // End of headers
		}

		// Parse header
		if strings.HasPrefix(line, "Content-Length:") {
			lengthStr := strings.TrimSpace(strings.TrimPrefix(line, "Content-Length:"))
			contentLength, err = strconv.ParseInt(lengthStr, 10, 64)
			if err != nil {
				return nil, fmt.Errorf("invalid Content-Length: %s", lengthStr)
			}
		}
		// Ignore other headers (Content-Type, etc.)
	}

	if contentLength == 0 {
		return nil, fmt.Errorf("missing Content-Length header")
	}

	// Read payload
	payload := make([]byte, contentLength)
	if _, err := io.ReadFull(br, payload); err != nil {
		return nil, err
	}

	// Parse JSON
	var msg JSONRPCMessage
	if err := json.Unmarshal(payload, &msg); err != nil {
		return nil, fmt.Errorf("invalid JSON: %w", err)
	}

	return &msg, nil
}

// WriteMessage writes an LSP message with Content-Length framing.
func WriteMessage(w io.Writer, msg *JSONRPCMessage) error {
	// Ensure jsonrpc field is set
	if msg.JSONRPC == "" {
		msg.JSONRPC = "2.0"
	}

	payload, err := json.Marshal(msg)
	if err != nil {
		return err
	}

	// Write header
	header := fmt.Sprintf("Content-Length: %d\r\n\r\n", len(payload))
	if _, err := w.Write([]byte(header)); err != nil {
		return err
	}

	// Write payload
	_, err = w.Write(payload)
	return err
}

// NewResponse creates a successful response message.
func NewResponse(id interface{}, result interface{}) *JSONRPCMessage {
	return &JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      id,
		Result:  result,
	}
}

// NewErrorResponse creates an error response message.
func NewErrorResponse(id interface{}, code int, message string, data interface{}) *JSONRPCMessage {
	return &JSONRPCMessage{
		JSONRPC: "2.0",
		ID:      id,
		Error: &RPCError{
			Code:    code,
			Message: message,
			Data:    data,
		},
	}
}

// NewNotification creates a notification message (no ID, no response expected).
func NewNotification(method string, params interface{}) *JSONRPCMessage {
	return &JSONRPCMessage{
		JSONRPC: "2.0",
		Method:  method,
		Params:  params,
	}
}
