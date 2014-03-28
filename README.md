# Pochopedia

Pochopedia is a music database for a german brass choires (Posaunenchöre).
Its primary use is to track in which books you can find a certain piece of music,
but its also a good example of a web application built with common lisp.

## Dependencies

Pochopedia has some dependencies not yet on Quicklisp, namely:

* [site-compiler](https://github.com/chfin/site-compiler)
* [ev-liturgical-colors](https://github.com/chfin/ev-liturgical-colors)

You can clone them to your Quicklisp installation's `local-projects/` directory
or put them somewhere in your asdf load path.

## Installation

Clone the [repository](https://github.com/chfin/pochopedia)
([HTTPS clone URL](https://github.com/chfin/pochopedia.git)) to an ASDF-loadable location.

## Running

### From the REPL (development)

You can load the system "pochopedia" using either Quicklisp (recommended) or ASDF.

```common-lisp
CL-USER> (ql:quickload "pochopedia")
```

To start the server run

```common-lisp
CL-USER> (pochopedia:compile-db)
CL-USER> (pochopedia:start-server)
```

You can stop the server with

```common-lisp
CL-USER> (pochopedia:stop-server)
```

or restart it (e.g. after reloading the ASDF system) with

```common-lisp
CL-USER> (pochopedia:restart-server)
```

### Using the build script (production, currently requires quicklisp)

Run the file `build.lisp` e.g.

```
$ sbcl --load build.lisp
```

This will create a binary called `pochopedia` which you can use to run the server.
Executing the binary will call start the web server as well as a swank server,
so that you can `slime-connect` to it.

Note that relative paths used in pochopedia are relative to the ASDF system
and will be compiled into the binary based on the location of the system at compile time.
This means, you should be able to put the binary anywhere you like
(e.g. your web server's document tree)
and it will still use the directory containing the ASDF system at compile time

## Configuration

Pochopedia uses [envy](https://github.com/fukamachi/envy) to select its configuration.
The configurations are currently stored in `config.lisp`
but support for configuration files is planned.

### Environment Variables

* `POCHO_ENV` determines the configuration (`"local"` or `"production"`)
* `POCHO_SWANK_PORT` (production only) can override the port number swank listens on
  when using the binary produced by `build.lisp`

## Hosting

Currently pochopedia uses Hunchentoot as a web server but it might be necessary to
integrate it into another web server, e.g. for shared hosting.
There are several possibilities to achive this.

### Proxy Rewrite Rule

You can use a `.htacces` file to redirect all requests to a running pochopedia instance:

```htaccess
RewriteEngine On
RewriteRule (.*) http://localhost:661819/$1 [P]
```

If you us a different port, please change it accordingly.

### FastCGI

To be done.
Pochopedia uses [ningle](http://8arrow.org/ningle/)
which uses [clack](http://clacklisp.org/) which supports FastCGI.
Help with this is welcome.