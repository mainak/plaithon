A barebones python interpreter in written in racket

Attribution:
=============
This was a course project and the bootstrap code and testcases are those provided by the TAs of cs173 course taught in Fall 2012 at Brown http://cs.brown.edu/courses/cs173/2012 . The original harness code can be found at https://github.com/brownplt/cs173-python

File Layout:
=============

It is exactly the same as in the initial project framework except for a file called python-bootstrap.rkt. This file is used to load all the builtin symbols and associated objects

Design choices:
================

The basic idea is the same as python, which is that everything is an object. Notably there are two special objects called type and object(same as in python) which are responsible for the creation and proper functioning of all the classes and objects.

The scoping rules are also exactly same as python. There is a builtin scope which is read only. There is a global scope(module level) and there is the dynamic lexical scope. All lookups first happen in the lexical scope, then global scope and finally builtin scope. One exception is when identifiers are tagged with the global keyword in nexted scopes in which case they bypass the lexical scopes.

Scoping is handled by the desugarer by variable hoisting. Also, all identifiers accessing global variables use modified core commands to enable bypassing of the lexical scope.

The core is realtively modest with shortcut commands which can be used to create the basic data types. The basic data types can also be created by calling their respective class constructors. One special command which can be considered cheating is RunCoreC. this is used to run bypass the interpreter, so that particularly cumbersome algos, like computation of mro can be done in plai-typed and not in the core language.

The builtin environment and initial objects store are hardcoded so that the builtin functions get a proper environment with all the builtin names defined in the respective closures.

The store and the global and buiiltin envs are global variables to save the hassle of threading it all the time. This is not a very good implementation choice if we want to take advantage of multi-threaded environments.

Et Cetera:
================
code fails with timeout of 3 secs. I have increased it to 10 secs. Please do
the same to achieve similar results
