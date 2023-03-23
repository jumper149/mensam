# Server

The server executable is written in Haskell using wai and servant for the HTTP interface.
OpenAPI documentation is generated automatically.
This application uses mtl-style effects.

## Configuration

Environment variables and a configuration file change the runtime behaviour.

### Environment

Environment variables are checked early during the initialization.
The options are set [here](./source/library/Mensam/Environment.hs).

### Configuration file

The configuration file is written in JSON and is specified in [here](./source/library/Mensam/Configuration.hs).

## Development

```
# Generate static files and configuration.
nix build ..#subflakes.config.packages.x86_64-linux.default

# Use cabal to develop the application.
MENSAM_CONFIG_FILE=result cabal run mensam
```

### Formatting

Use `fourmolu` to format Haskell.

```
# Format Haskell.
fourmolu --mode inplace ./source
```

### `weeder`

Running `weeder` should be a part of linting, but requires manual execution at the moment.

```
# Prepare additional information for weeder.
cabal clean
cabal build all

# Run weeder.
weeder
```

### `graphmod`

The module dependency graph can be visualised using `graphmod`.

```
# Run graphmod.
nix build ..#subflakes.server.checks.x86_64-linux.graphmod

# View graph with xdot or your PDF viewer.
xdot result/graphmod.dot
zathura result/graphmod.pdf
```
