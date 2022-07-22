# linear-programming-glpk

[![Github Actions Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fneil-lindquist%2Flinear-programming-glpk%2Fbadge&style=flat&label=build)](https://actions-badge.atrox.dev/neil-lindquist/linear-programming-glpk/goto)
![GPL License](https://img.shields.io/github/license/neil-lindquist/linear-programming-glpk.svg?color=informational)

This is a backend for the [linear-programming](https://github.com/neil-lindquist/linear-programming) Common Lisp library using the GNU Linear Programming Kit (GLPK) library.
To use this backend, simply evaluate `(setf linear-programming:*solver* glpk:glpk-solver)` before calling `linear-programming:solve-problem`.  Then, the linear-programming library will automatically use GLPK to solve problems.

## Installation
First, obtain a shared library for [GLPK](https://www.gnu.org/software/glpk/) and place the dynamic library somewhere that CFFI can find it, such as in the current working directory.
Precompiled binaries are available for many platforms, otherwise it can be compiled from source.
First, the [Julia GLPK wrapper](https://github.com/jump-dev/GLPK.jl/) provides [precompiled binaries for a variety of platforms](https://github.com/JuliaBinaryWrappers/GLPK_jll.jl/releases).
Alternatively, your operating system's package manager may provide binaries for your platform.
Additionally, there is a set of prebuild binaries [specifically for Windows](http://winglpk.sourceforge.net/).

This library is available through Quicklisp and can be loaded as `(ql:quickload :linear-programming-glpk)`.
It is also available through the Ultralisp distribution.
Alternatively, the library can be installed manually.
First, install [linear-programming](https://github.com/neil-lindquist/linear-programming), [CFFI](https://www.common-lisp.net/project/cffi/), and their dependencies.
Next, download this repository somewhere ASDF can find them.
Then, it can be loaded with `(asdf:load-system :linear-programming)`.

The library's tests can be used to verify that the library is installed correctly and GLPK can be found.
Evaluate `(asdf:test-system :linear-programming-glpk)` to test the system.
