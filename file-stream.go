// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"os"
	"strconv"
)

// FileStreamSymbol is the symbol with a value of "file-stream".
const FileStreamSymbol = Symbol("file-stream")

func init() {
	DefConstant(&CLPkg, string(FileStreamSymbol), FileStreamSymbol, `A _file-stream_ stream backed by a *os.File.`)
}

// FileStream is a *os.File.
type FileStream os.File

// String representation of the Object.
func (obj *FileStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *FileStream) Append(b []byte) []byte {
	b = append(b, "#<FILE-STREAM "...)
	b = append(b, (*os.File)(obj).Name()...)
	b = append(b, " {"...)
	b = strconv.AppendInt(b, int64((*os.File)(obj).Fd()), 10)

	return append(b, "}>"...)
}

// Simplify the Object into an int64.
func (obj *FileStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *FileStream) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *FileStream) Hierarchy() []Symbol {
	return []Symbol{FileStreamSymbol, StreamSymbol, TrueSymbol}
}

// StreamType returns 'fileStream.
func (obj *FileStream) StreamType() Symbol {
	return FileStreamSymbol
}

// Eval returns self.
func (obj *FileStream) Eval(s *Scope, depth int) Object {
	return obj
}

// Write made visible since os.File functions are not automatically visible.
func (obj *FileStream) Write(b []byte) (int, error) {
	return (*os.File)(obj).Write(b)
}

// Read made visible since os.File functions are not automatically visible.
func (obj *FileStream) Read(b []byte) (int, error) {
	return (*os.File)(obj).Read(b)
}

// Close made visible since os.File functions are not automatically visible.
func (obj *FileStream) Close() error {
	return (*os.File)(obj).Close()
}

// IsOpen return true if the stream is open or false if not.
func (obj *FileStream) IsOpen() bool {
	_, err := (*os.File)(obj).Write([]byte{})
	return err == nil
}

// LastByte returns the last byte written or zero if nothing has been written.
func (obj *FileStream) LastByte() byte {
	b := []byte{0}
	if pos, err := (*os.File)(obj).Seek(0, 1); err == nil && 0 < pos { // relative to current
		_, _ = (*os.File)(obj).ReadAt(b, pos-1)
	}
	return b[0]
}

// FileLength return the length of a file.
func (obj *FileStream) FileLength() (length Object) {
	if fi, err := (*os.File)(obj).Stat(); err == nil {
		length = Fixnum(fi.Size())
	}
	return
}

// Seek moves the pos in buf. This is part of the io.Seeker interface.
func (obj *FileStream) Seek(offset int64, whence int) (n int64, err error) {
	return (*os.File)(obj).Seek(offset, whence)

}
