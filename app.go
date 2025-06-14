// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"crypto/sha3"
	"embed"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"plugin"
	"strings"
)

// App has multiple roles related to developing, generating, and running an
// application. During development of a standalone SLIP application the App
// can execute LISP code from source files. Once development is complete then
// App can be used to generate a standalone application project. When that
// application is run the App functions assist in setting up and evaluating
// the LISP code that implements the application behavior.
type App struct {
	// Title is the name of the application.
	Title string

	// Usage for help documentation.
	Usage func()

	// Plugins is a list of paths to the plugin packages to be included in the
	// go.mod file. (e.g., src/message.so)
	Plugins []string

	// Options identify the command line options for the application.
	Options []*AppArg

	// LispCode are the filepaths to the LISP source code. This use used
	// during development and to form an encrypted and embedded file when the
	// application is generated.
	LispCode []string

	// Source for the application. This is expected to be a single LISP file
	// that may be encryped with a key specified as a command line argument or
	// in a file.
	Source *embed.FS

	// KeyFlag is the command line flag for specifying a decryption key if the
	// source is encrypted.
	KeyFlag string

	// KeyFile is the path to the key file containing a decryption key if one
	// is needed.
	KeyFile string

	// EntryFunction is the name of a function or a lambda that takes no
	// arguments. This is called to run the application. It is started within
	// a scope that includes the command line argument variables already
	// bound.
	EntryFunction string

	// OnPanic is called if a panic is raised. It should return the exit code
	// for the application.
	OnPanic func(f any) int
}

// Run the application with the optional arguments. The args provide a means
// of testing during development.
func (app *App) Run(args ...string) (exitCode int) {
	defer func() {
		if r := recover(); r != nil {
			exitCode = 1
			if app.OnPanic != nil {
				exitCode = app.OnPanic(r)
			}
		}
	}()
	fs := flag.CommandLine
	if 0 < len(args) {
		fs = flag.NewFlagSet(app.Title, flag.ExitOnError)
	} else {
		args = os.Args[1:]
	}
	if app.Usage != nil {
		fs.Usage = app.Usage
	}
	scope := NewScope()

	var (
		key     string
		prepare bool
		genDir  string
		cleanup bool
	)
	if strings.Contains(os.Args[0], "go-build") { // using go run
		// Could also check suffix for /exe/main.
		fs.BoolVar(&prepare, "slipapp.prepare", false,
			"prepare the build by encrypting lisp sources and copying plugins to the src directory")
		fs.StringVar(&genDir, "slipapp.generate", "",
			"generate an application directory, prepare, and build the application")
		fs.BoolVar(&prepare, "slipapp.cleanup", false,
			"if generate is specified and cleanup is true all but the final application are removed")
	}
	if 0 < len(app.KeyFile) {
		if _, err := os.Stat(string(app.KeyFile)); err == nil {
			content, err := os.ReadFile(app.KeyFile)
			if err != nil {
				panic(err)
			}
			key = strings.TrimSpace(string(content))
		}
	}
	if 0 < len(app.KeyFlag) {
		fs.StringVar(&key, app.KeyFlag, "", "source code decryption key")
	}

	for _, opt := range app.Options {
		opt.SetFlag(fs, scope)
	}
	if err := fs.Parse(args); err != nil {
		panic(err)
	}
	for _, opt := range app.Options {
		opt.UpdateScope(scope)
	}
	if 0 < len(genDir) {
		app.Generate(genDir, key, cleanup)
		fmt.Printf("The go application %q has been created and contains then standalone application name %s.\n",
			app.Title, app.Title)
		return
	}
	if prepare {
		app.BuildEmbed("src", []byte(key))
		enc := "encrypted"
		if len(key) == 0 {
			enc = "not encrypted"
		}
		fmt.Printf("LISP files were %s and copied to the src directory. Plugin files also copied to src.\n", enc)
		return
	}
	app.load(scope, []byte(key))
	_ = ReadString(fmt.Sprintf("(%s)", app.EntryFunction), scope).Eval(scope, nil)

	return
}

// BuildEmbed sources and plugin libraries to the specified directory. If a
// non empty key is provided then lisp sources are encrypted.
func (app *App) BuildEmbed(dir string, key []byte) {
	if len(dir) == 0 {
		dir = "src"
	}
	var code []byte
	for _, path := range app.LispCode {
		content, err := os.ReadFile(path)
		if err != nil {
			panic(err)
		}
		code = append(code, content...)
		code = append(code, '\n')
	}
	if len(key) != 32 {
		key = sha3.SumSHAKE256(key, 32)
	}
	block, _ := aes.NewCipher(key)
	bsize := aes.BlockSize

	if len(code)%bsize != 0 {
		code = append(code, bytes.Repeat([]byte{'\n'}, bsize-len(code)%bsize)...)
	}
	buf := make([]byte, bsize+len(code))
	nonce := buf[:bsize]
	_, _ = io.ReadFull(rand.Reader, nonce)
	mode := cipher.NewCBCEncrypter(block, nonce)
	mode.CryptBlocks(buf[bsize:], code)

	if err := os.WriteFile(filepath.Join(dir, "app.lisp.enc"), buf, os.FileMode(0666)); err != nil {
		panic(err)
	}
	for _, path := range app.Plugins {
		src, err := os.Open(path)
		if err != nil {
			NewPanic("open %s reading failed. %s", path, err)
		}
		var dest *os.File
		destPath := filepath.Join(dir, filepath.Base(path))
		if dest, err = os.Create(destPath); err != nil {
			NewPanic("open %s for writing failed. %s", destPath, err)
		}
		if _, err = io.Copy(dest, src); err != nil {
			NewPanic("failed to copy %s to %s. %s", path, destPath, err)
		}
	}
}

// Generate project directory with a main.go, go.mod, and lisp code directory.
func (app *App) Generate(dir, key string, cleanup bool) {
	// TBD create the dir if needed
	// generate a app.lisp file, encrypt if a key is provided
	// write a main.go file
	// mkdir src
	// go mod init
	// go mod tidy
	// copy .so files to src
	// go build (verify it builds)
	// if cleanup
	//   remove main.go, src, go.mod, and go.sum
}

func (app *App) load(scope *Scope, key []byte) {
	var (
		pluginsFound bool
		lispFound    bool
	)
	if app.Source != nil {
		if len(key) != 32 {
			key = sha3.SumSHAKE256(key, 32)
		}
		entries, err := app.Source.ReadDir("src")
		if err != nil {
			panic(err)
		}
		var temp *os.File
		defer func() {
			if temp != nil {
				_ = os.Remove(temp.Name())
			}
		}()
		for _, entry := range entries {
			path := filepath.Join("src", entry.Name())
			switch {
			case strings.HasSuffix(path, ".lisp"):
				content, err := app.Source.ReadFile(path)
				if err != nil {
					NewPanic("%s open failed. %s", path, err)
				}
				_ = Compile(content, scope)
			case strings.HasSuffix(path, ".so"):
				if temp, err = os.CreateTemp("/tmp", "*.so"); err != nil {
					panic(err)
				}
				src, err := app.Source.Open(path)
				if err != nil {
					NewPanic("open %s reading failed. %s", path, err)
				}
				if _, err = io.Copy(temp, src); err != nil {
					NewPanic("failed to copy %s to %s. %s", path, temp.Name(), err)
				}
				if _, err := plugin.Open(temp.Name()); err != nil {
					NewPanic("plugin %s open failed. %s", temp.Name(), err)
				}
				_ = temp.Close()
				_ = os.Remove(temp.Name())
				// For development leave in place so it picks up the original
				// file.
				pluginsFound = true
			case strings.HasSuffix(path, ".enc"):
				data, err := os.ReadFile(path)
				if err != nil {
					panic(err)
				}
				block, _ := aes.NewCipher(key)
				bsize := aes.BlockSize
				non := data[:bsize]
				data = data[bsize:]
				mode := cipher.NewCBCDecrypter(block, non)
				mode.CryptBlocks(data, data)
				if data[len(data)-1] != '\n' {
					panic("Invalid key")
				}
				if result := Compile(data, scope); result == nil {
					panic("No objects loaded. Possibly an invalid key.")
				}
				lispFound = true
			}
		}
	}
	if !pluginsFound {
		for _, path := range app.Plugins {
			if _, err := plugin.Open(path); err != nil {
				NewPanic("plugin %s open failed. %s", path, err)
			}
		}
	}
	if !lispFound {
		for _, path := range app.LispCode {
			content, err := os.ReadFile(path)
			if err != nil {
				panic(err)
			}
			_ = Compile(content, scope)
		}
	}
}
