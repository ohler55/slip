// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"crypto/sha3"
	"embed"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
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
		prepare string
		genDir  string
		replace string
		cleanup bool
		verbose bool
	)
	if strings.Contains(os.Args[0], "go-build") { // using go run
		// Could also check suffix for /exe/main.
		fs.StringVar(&prepare, "slipapp.prepare", "",
			"prepare the build by encrypting lisp sources and copying plugins to the src directory")
		fs.StringVar(&genDir, "slipapp.generate", "",
			"generate an application directory, prepare, and build the application")
		fs.BoolVar(&cleanup, "slipapp.cleanup", false,
			"if generate is specified and cleanup is true all but the final application are removed")
		fs.StringVar(&replace, "slipapp.replace", "",
			"add a replace for the slip package at the end of the go.mod file")
	}
	fs.BoolVar(&verbose, "v", false, "verbose")

	if 0 < len(app.KeyFile) {
		if _, err := os.Stat(app.KeyFile); err == nil {
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
		app.Generate(genDir, []byte(key), replace, cleanup)
		if verbose {
			fmt.Printf(
				"The go application directory %q has been created and contains the %s application.\n",
				app.Title, app.Title,
			)
		}
		return
	}
	if 0 < len(prepare) {
		app.BuildEmbed(prepare, []byte(key))
		if verbose {
			enc := "encrypted"
			if len(key) == 0 {
				enc = "not encrypted"
			}
			fmt.Printf("LISP files were %s and copied to the src directory. Plugin files were copied to src.\n", enc)
		}
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
	if err := os.MkdirAll(dir, 0755); err != nil {
		ErrorPanic(NewScope(), 0, "failed to created the %s directory", dir)
	}
	app.makeOneLisp(dir, key)
	app.copyPlugins(dir)
}

// Generate project directory with a main.go, go.mod, and lisp code directory.
func (app *App) Generate(dir string, key []byte, replace string, cleanup bool) {
	path := filepath.Join(dir, "src")
	if err := os.MkdirAll(path, 0755); err != nil {
		ErrorPanic(NewScope(), 0, "failed to created the %s directory", path)
	}
	app.makeOneLisp(path, key)
	app.copyPlugins(path)
	app.writeMain(dir)

	app.runCommand(dir, "go", "mod", "init", "main")
	if 0 < len(replace) {
		if f, err := os.OpenFile(filepath.Join(dir, "go.mod"), os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644); err == nil {
			_, _ = fmt.Fprintf(f, "\nreplace github.com/ohler55/slip => %s\n", replace)
			_ = f.Close()
		}
	}
	app.runCommand(dir, "go", "mod", "tidy")
	app.runCommand(dir, "go", "build", "-C", dir, "-o", app.Title)
	if cleanup {
		_ = os.RemoveAll(path)
		_ = os.RemoveAll(filepath.Join(dir, "go.mod"))
		_ = os.RemoveAll(filepath.Join(dir, "go.sum"))
		_ = os.RemoveAll(filepath.Join(dir, "main.go"))
	}
}

func (app *App) runCommand(dir, name string, args ...string) string {
	cmd := exec.Command(name, args...)
	if 0 < len(dir) {
		cmd.Dir = dir
	}
	out, err := cmd.Output()
	if err != nil {
		var ee *exec.ExitError
		if errors.As(err, &ee) {
			out = append(out, ee.Stderr...)
		}
		ErrorPanic(NewScope(), 0, "%s %s failed: %s\n%s", name, args, err, out)
	}
	return string(out)
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
					ErrorPanic(scope, 0, "%s open failed. %s", path, err)
				}
				_ = Compile(content, scope)
			case strings.HasSuffix(path, ".so"):
				if temp, err = os.CreateTemp("/tmp", "*.so"); err != nil {
					panic(err)
				}
				src, err := app.Source.Open(path)
				if err != nil {
					ErrorPanic(scope, 0, "open %s reading failed. %s", path, err)
				}
				if _, err = io.Copy(temp, src); err != nil {
					ErrorPanic(scope, 0, "failed to copy %s to %s. %s", path, temp.Name(), err)
				}
				if _, err := plugin.Open(temp.Name()); err != nil {
					ErrorPanic(scope, 0, "plugin %s open failed. %s", temp.Name(), err)
				}
				_ = temp.Close()
				_ = os.Remove(temp.Name())
				// For development leave in place so it picks up the original
				// file.
				pluginsFound = true
			case strings.HasSuffix(path, ".enc"):
				data, err := app.Source.ReadFile(path)
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
				ErrorPanic(scope, 0, "plugin %s open failed. %s", path, err)
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

func (app *App) makeOneLisp(dir string, key []byte) {
	var code []byte
	filename := "app.lisp"
	for _, path := range app.LispCode {
		content, err := os.ReadFile(path)
		if err != nil {
			panic(err)
		}
		code = append(code, content...)
		code = append(code, '\n')
	}
	if 0 < len(key) {
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
		code = buf
		filename = "app.lisp.enc"
	}
	if err := os.WriteFile(filepath.Join(dir, filename), code, os.FileMode(0666)); err != nil {
		ErrorPanic(NewScope(), 0, "writing %s/%s failed. %s", dir, filename, err)
	}
}

func (app *App) copyPlugins(dir string) {
	for _, path := range app.Plugins {
		copyFile(path, filepath.Join(dir, filepath.Base(path)))
	}
}

func copyFile(srcPath, destPath string) {
	src, err := os.Open(srcPath)
	if err != nil {
		ErrorPanic(NewScope(), 0, "open %s reading failed. %s", srcPath, err)
	}
	defer func() { _ = src.Close() }()

	var dest *os.File
	if dest, err = os.Create(destPath); err != nil {
		ErrorPanic(NewScope(), 0, "open %s for writing failed. %s", destPath, err)
	}
	defer func() { _ = dest.Close() }()
	if _, err = io.Copy(dest, src); err != nil {
		ErrorPanic(NewScope(), 0, "failed to copy %s to %s. %s", srcPath, destPath, err)
	}
}

func (app *App) writeMain(dir string) {
	b := fmt.Appendf(nil, `package main

import (
	"embed"
	"fmt"

	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg"
)

//go:embed src
var srcFS embed.FS

func main() {
	app := slip.App{
		Title: %q,
`, app.Title)
	b = append(b, "\t\tLispCode: []string{\n"...)
	for _, lc := range app.LispCode {
		b = append(b, "\t\t\t\""...)
		b = append(b, lc...)
		b = append(b, '"', ',', '\n')
	}
	b = append(b, "\t\t},\n"...)
	if 0 < len(app.Plugins) {
		b = append(b, "\t\tPlugins: []string{\n"...)
		for _, pi := range app.Plugins {
			b = append(b, "\t\t\t\""...)
			b = append(b, pi...)
			b = append(b, '"', ',', '\n')
		}
		b = append(b, "\t\t},\n"...)
	}
	if 0 < len(app.Options) {
		b = append(b, "\t\tOptions: []*slip.AppArg{\n"...)
		for _, aa := range app.Options {
			b = fmt.Appendf(b, "\t\t\t{Flag: %q, Doc: %q, Type: %q, Var: %q, Default: %s},\n",
				aa.Flag, aa.Doc, aa.Type, aa.Var, aa.DefaultReadable())
		}
		b = append(b, "\t\t},\n"...)
	}
	b = append(b, "\t\tSource:        &srcFS,\n"...)
	if 0 < len(app.KeyFlag) {
		b = fmt.Appendf(b, "\t\tKeyFlag:       %q,\n", app.KeyFlag)
	}
	if 0 < len(app.KeyFile) {
		b = fmt.Appendf(b, "\t\tKeyFile:       %q,\n", app.KeyFile)
	}
	b = fmt.Appendf(b, "\t\tEntryFunction: %q,\n", app.EntryFunction)
	b = append(b, `		OnPanic: func(r any) int {
			fmt.Printf("*-*-* %s\n", r)
			return 1
		},
	}
	app.Run()
}
`...)
	if err := os.WriteFile(filepath.Join(dir, "main.go"), b, os.FileMode(0666)); err != nil {
		ErrorPanic(NewScope(), 0, "writing %s/main.go failed. %s", dir, err)
	}
}
