# Guile Hoot Game Jam Template

This repository is the quickest way to get started building games in
Scheme that run in web browsers with Guile Hoot!

It has everything you need:

* A simple Breakout clone to use as a starting point.

* HTML and JavaScript boilerplate for running the game in a web page.

* DOM bindings for events, images, audio, and more.

* HTML5 canvas bindings for rendering.

* Some simple but useful game math modules.

* A `Makefile` for compiling the game to WebAssembly, running a
  development web server, and generating zip bundles for publishing to
  itch.io.

* A Guix `manifest.scm` file for creating a development environment
  with `guix shell`.

## Tutorial

The fastest way to get everything you need is to use [GNU
Guix](https://guix.gnu.org), a wonderful package manager written in
Scheme.

Once you have Guix, the development environment with all necessary
dependencies can be created:

```
guix shell
```

To build the game, run:

```
make
```

To launch a development web server, run:

```
make serve
```

To check if the program works, visit https://localhost:8088 in your
web browser.  We recommend using Mozilla Firefox or Google Chrome.
Hoot is not supported on Safari at this time.

When it's time to publish the game to itch.io, run:

```
make bundle
```

Upload the resulting zip file to your itch.io game page and share your
game with others!  Have fun!

### Guix on Mac

It is possible to set up a virtual machine in which Guix is available. However, you'd still need to transfer the code of this repository to that box in order to benefit from it. Alternatively, read the instructions for a manual installation of dependencies in the following section.

If you want to give it a try, follow the instructions here to install msg, a Guix environment for Mac: https://superkamiguru.org/projects/msg.html

Then run the following commands in order to set up and connect to the virtual machine in which Guix is available:

```
msg machine init
msg machine start
msg shell
```

### Manual installation of dependencies on Mac

Follow the instructions here to install Hoot: https://gitlab.com/spritely/guile-hoot

Also add the following path exports to your ~/.bashrc or similar:

```
export GUILE_LOAD_PATH="/opt/homebrew/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/opt/homebrew/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/opt/homebrew/lib/guile/3.0/extensions"
```

## Getting help

If you have questions or need some help, visit the [Spritely
Institute's forum](https://community.spritely.institute/) or connect
to the `#spritely` channel on the Libera.Chat IRC network.
