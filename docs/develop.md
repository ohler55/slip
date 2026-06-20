# Slip Development

Slip is more than just a language. It is a development environment
with support for writing, testing, and deploying software.

Once a set of requirements have been established, even if they are
simply a concept floating around in a developer's head, the fun
starts, development. Leaving out the management of requirement and focussing on the
software itselt leads us to three basic steps; editing, testing, and
deployment.

## Editing

For many the editor of choice for writing Lisp code is Emacs. Slip
includes a swank package so Slip evaluator can be run inside Emacs. On
a personal note I tend to edit in Emacs and make use of the Slip REPL
but that's just me. I rely heavily on the REPL for function and type
documentation.

The REPL is also a great place to try out code snippets or for
informal testing. The multi-line editing feature allows for more
complex function editing as well. Bouncing between Emacs and the Slip
REPL is a solid approach to writing Lisp code. The stash feature makes
it easy to stash a function once it works as expected and then pick up
the function from the stash while in the editor.

The REPL includes several options for exploring Lisp. The `apropos`
function helps in searching for functions and variables with a simple
name match. The `apropos` function is included in Common Lisp (CL) as
well. It's not unique to Slip. After finding a function or variable
the `describe` function can be used to find out the details. Slip adds
three more functions not found in CL similar to `describe`. The
`describe-flavor` function can be used to see the details of a
specific flavor. Closely related is `describe-type` which prints out a
description of any type, class, or flavor. Finally then
`describe-method` function can be used to describe a method on a
flavor.

The Slip REPL has tab completion which reduces typing but can also be
used to search for a function. Popup help is also available. When the
cursor is at the end of a function name hitting M-/ will pop up help
for that function. The same help displayed with the `describe`
function.

## Testing

Ages ago when Lisp was young testing was typically informal or done
manually. Today best practices call for repeatable tests and some
assurances that edges cases have to tested. Slip includes a test
package which has test suites along with assert and refute functions
for building tests. Subsets of test can be run or all can be run and
results viewed in formatted report.

It's nice to be able to test but without knowing if all functions have
been reached it is hard to claim a collection of code is covered
completely. Slip includes an option for collecting coverage and for
displaying a summary of code coverage. Individual files can be
displayed colorized to indicated what functions are covered and which
are not.

Coverage can be turned on in Lisp code with `(setq *coverage* t)` and
a report generated with `(coverage-report "cov.lisp")`. Coverage can
also be set with the `-cover` command line argument of the slip
application.

Once a coverage file has been generated it can be viewed with the
lisp/cover.lisp script. Like the slip and slipr applications the
cover.lisp script can be copied to location in the `$PATH` such as
`~/bin` and invoked from any location. This is made possible with the
`#!/usr/bin/env slipr` header of the file.

## Deployment

It is convenient to be able to just type the name of a script or Lisp
application with having to type `slip my-app.lisp`. Slip has an
interpreter that takes no command line options to support using the
`#!/usr/bin/env slipr` header of the file.

A companion application to Slip is Slap. Slap is a customized Slip
interpreter that pulls in plugins when the slap application is build
so all coded needed is included in one executable. Similar to slipr,
Slap includes a slapr application that can be used with the shebang
approach just like slipr.

When building and providing code or a system that is intended for
loading by other applications, the Slip system class is provided. Once
a system.asd file is created it can be loaded with `(load-system
'my-system)` to simplify package reuse.
