# Waxeye Parser Generator JavaScript Runtime

This is the JavaScript runtime for [Waxeye](http://waxeye.org).

## Installation

The runtime is available in several formats.

### TypeScript: `waxeye.ts`

This is the TypeScript implementation for the runtime.
The JavaScript implementation is generated from it.

### JavaScript: `waxeye.js`

This JavaScript file detects CommonJS and AMD module systems,
and falls back to exporting the module as a `waxeye` global.

## Development

Run these commands from the root waxeye directory.

### Test

```bash
make test-javascript
```

### Build

```bash
make runtime-javascript
```

### Format

Whenever the build command is run, the source code is
automatically formatted using `clang-format`.

### Lint

Whenever the build command is run, the source code is
linted using `tslint`.
