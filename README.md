## cl-cffi-gtk4

### General information

The `cl-cffi-gtk4` library is a Lisp binding to GTK 4 which is a library for
creating graphical user interfaces. The `cl-cffi-gtk4` library replaces the
deprecated `cl-cffi-gtk3` library.

The `cl-cffi-gtk4` library is developped using SBCL on UBUNTU. The library runs
successfully on WINDOWS.

### License

The `cl-cffi-gtk4` library is licensed under the MIT license.

### Installation

The `cl-cffi-gtk4` library is ASDF installable:
```
(asdf:load-system :cl-cffi-gtk4)
```
After loading the library information about the installed version is
available:
```
* (gtk:cl-cffi-gtk-build-info)
cl-cffi-gtk version: 0.9.0
cl-cffi-gtk build date: 16:14 4/1/2025
GTK version: 4.16.3
GLIB version: 2.82.1
GDK-Pixbuf version: 2.42.12
Pango version: 1.54.0
Cairo version: 1.18.2
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-5200U CPU @ 2.20GHz
Software type: Linux
Software version: 6.11.0-21-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.2.9.debian
```

The `cl-cffi-gtk4` library depends on the following Lisp libraries:

*  `cl-cffi-glib`
*  `cl-cffi-gdk-pixbuf`
*  `cl-cffi-pango`
*  `cl-cffi-cairo`
*  `cl-cffi-graphene`

These libraries are available from [github](https://github.com/crategus).
The `cl-cffi-gtk4` API documentation is available at
[crategus.com](https://crategus.com/books/cl-cffi-gtk4/).

### Documentation on Wiki

Visit the [https://github.com/crategus/cl-cffi-gtk4/wiki](Wiki page) to learn
more about using the library.

## GTK demo and examples

The `cl-cffi-gtk4` library comes with examples and a GTK demo. Use
```
(asdf:load-system :gtk4-example)
```
or
```
(asdf:load-system :gtk4-demo)
```
to load the examples or the GTK demo. Start the GTK demo with
```
(gtk4-demo:gtk4-demo)
```
on the Lisp prompt. When loading the examples, switch to the package and start
an example with the `run-example` function:
```
* (asdf:load-system :gtk4-example)
T
* (in-package :gtk4-example)
#<PACKAGE "GTK4-EXAMPLE">
* (run-example #'do-window-simple)
```
The examples and the GTK demo need the resource file
```
example/gtk4-example.gresource
```
This resource file is automatically created by the
`glib-sys:check-and-create-resources` function, that is part of the
`cl-cffi-glib` library.

## TESTSUITE

The `cl-cffi-gtk4` library comes with a testsuite. The testsuite can be
performed with
```
(asdf:test-system :cl-cffi-gtk4)
```
or loaded with
```
(asdf:load-system :cl-cffi-gtk4/test)
```
Unfortunately, many of the tests depend on the hardware or local environment
you are using and will therefore fail. This may be fixed in the future.
