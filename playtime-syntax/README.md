# playtime README

## Install current version

In VS Code, run "Extensions: Install from VSIX" and select the file `playtime-dsl-0.0.1.vsix` from the playtime-syntax folder.

## Development

### Preconditions

Make sure you've got vsce installed:

```
npm install -g vsce
```

### Create package

To compile, run:

```
vsce package
```

This creates a file named `playtime-dsl-0.0.1.vsix`.


## Development setup

* Open the playtime-syntax folder in VS Code.
* Press F5 (Start Debugging)

This opens a second VS Code window with the playime language extension loaded.

Use the VS Code command "Developer: Reload Window" to reload the extension.

If you don't see your changes reflected, there may be a conflict with an installed extension, perhaps an older version of the playtime language extension that takes precedence. Maybe there is a tidy solution to that, but for now, what works is to uninstall any existing versions and try again.
