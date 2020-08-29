# linear-programming-glpk

![GPL License](https://img.shields.io/github/license/neil-lindquist/linear-programming-glpk.svg?color=informational)

This is a backend for the [linear-programming](https://github.com/neil-lindquist/linear-programming) Common Lisp library using the GNU Linear Programming Kit library (GLPK).
To use this backend, simply evaluate `(setf linear-programming:*solver* glpk:glpk-solver)` before calling `linear-programming:solve-problem`.  Then, the linear-programming library will automatically use GLPK to solve problems.

## Installation
This library is not yet in the main Quicklisp distribution, so it needs to be installed via [Ultralisp's Quicklisp distribution](ultralisp.org) or installed manually.
To install it via Ultralisp, make sure the Ultralisp distribution is registered with Quicklisp, evaluating `(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)` if needed.
Then, simply evaluate `(ql:quickload :linear-programming-glpk)`.
To install it manually, install [linear-programming](https://github.com/neil-lindquist/linear-programming), [CFFI](https://www.common-lisp.net/project/cffi/), and this repository somewhere ASDF can find them.  (The first two are provided in the main Quicklisp distribution.)
Then, it can be loaded with `(asdf:load-system :linear-programming)`.

Finally, download and compile [GLPK](https://www.gnu.org/software/glpk/) and place the dynamic library somewhere that CFFI can find it, such as in the current working directory.
To check that it is working and that GLPK can be found, the tests can be run with `(asdf:test-system :linear-programming-glpk)`.

Instead of compiling GLPK by hand, there may be precompiled binaries for your platform.
First, the [Julia GLPK wrapper](https://github.com/jump-dev/GLPK.jl/) provides [precompiled binaries for a variety of platforms](https://github.com/JuliaBinaryWrappers/GLPK_jll.jl/releases).
Additionally, there is a set of prebuild binaries [specifically for Windows](http://winglpk.sourceforge.net/).
Finally, your operating system's package manager may provide binaries for your platform.
