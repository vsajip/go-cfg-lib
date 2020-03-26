The CFG configuration format is a text format for configuration files which is similar to, and a superset of, the JSON format. It dates from [2008](https://wiki.python.org/moin/HierConfig) and has the following aims:

* Allow a hierarchical configuration scheme with support for key-value mappings and lists.
* Support cross-references between one part of the configuration and another.
* Provide the ability to compose configurations (using include and merge facilities).
* Provide the ability to access real application objects safely.

It overcomes a number of drawbacks of JSON when used as a configuration format:

* JSON is more verbose than necessary.
* JSON doesn’t allow comments.
* JSON doesn’t allow trailing commas in lists and mappings.

Installation
============
You can use this package using ``go get github.com/vsajip/go-cfg-lib/config`` and then importing ``github.com/vsajip/go-cfg-lib/config`` in your code.

Exploration
============
To explore CFG functionality for Go, we use the `gore` Read-Eval-Print-Loop (REPL), which is available from [here](https://github.com/motemen/gore). Once installed, you can invoke a shell using
```
$ gore
```
Note that `gore` requires a version of Go that supports Go modules. We use a slightly modified version for this documentation, which prints the type of the objects explored in addition to printing their values.

Getting Started with CFG in Go
==============================
A configuration is represented by an instance of the `Config` struct. A reference to one can be obtained using either the `NewConfig()` or `FromFile()` functions. The former creates an instance with no configuration loaded, while the latter initialises using a configuration in a specified file: the text is read in, parsed and converted to an object that you can then query. A simple example:

```
a: 'Hello, '
b: 'world!'
c: {
  d: 'e'
}
'f.g': 'h'
christmas_morning: `2019-12-25 08:39:49`
home: `$HOME`
foo: `$FOO|bar`
```

Loading a configuration
=======================
The configuration above can be loaded as shown below. In the REPL shell:

```
   gore> :import os
   gore> :import config
   gore> os.Chdir(os.Getenv("PWD"))
   <nil>(<nil>)
   gore> cfg, err := config.FromFile("test0.cfg")
   *config.Config(Config("test0.cfg" [7 items]))
   <nil>(<nil>)
```
The ``os.Chdir(os.Getenv("PWD"))`` dance is needed because of the way ``gore`` works, and it isn't relevant to this example. The ``<nil>(<nil>)`` printed is just the error returned from the function.

Access elements with keys
=========================
Accessing elements of the configuration with a simple key is not much harder than using a ``map[string]Any``:

```
   gore> cfg.Get("a")
   string(Hello, )
   <nil>(<nil>)
   gore> cfg.Get("b")
   string(world!)
   <nil>(<nil>)
```
You can see the types and values of the returned objects are as expected.

Access elements with paths
==========================
As well as simple keys, elements  can also be accessed using `path` strings:
```
   gore> cfg.Get("c.d")
   string(e)
   <nil>(<nil>)
```
Here, the desired value is obtained in a single step, by (under the hood) walking the path `c.d` – first getting the mapping at key `c`, and then the value at `d` in the resulting mapping.

Note that you can have simple keys which look like paths:
```
   gore> cfg.Get("f.g")
   string(h)
   <nil>(<nil>)
```
If a key is given that exists in the configuration, it is used as such, and if it is not present in the configuration, an attempt is made to interpret it as a path. Thus, `f.g` is present and accessed via key, whereas `c.d` is not an existing key, so is interpreted as a path.

Access to date/time objects
===========================
You can also get native Go date/time objects from a configuration, by using an ISO date/time pattern in a `backtick-string`:
```
   gore> cfg.Get("christmas_morning")
   time.Time(2019-12-25 08:39:49 +0000 UTC)
   <nil>(<nil>)
```
Access to environment variables
===============================

To access an environment variable, use a `backtick-string` of the form `$VARNAME`:
```
   gore> cfg.Get("home")
   string(/home/vinay)
   <nil>(<nil>)
```
You can specify a default value to be used if an environment variable isn’t present using the `$VARNAME|default-value` form. Whatever string follows the pipe character (including the empty string) is returned if `VARNAME` is not a variable in the environment.
```
   gore> cfg.Get("foo")
   string(bar)
   <nil>(<nil>)
```
For more information, see [the documentation](https://docs.red-dove.com/cfg/index.html).
