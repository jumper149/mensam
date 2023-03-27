# Mensam

A desk-booking web application.

View the [OpenAPI specification](https://redocly.github.io/redoc/?url=https://raw.githubusercontent.com/jumper149/mensam/main/final/openapi.json).

## Development

Mensam is built with Nix.
You can use regular flake commands such as `nix build`, `nix develop` or `nix flake check`.

### Subflakes

The source code is split up into subflakes, which also have their own development environments and checks.
These are just regular nix files, but follow the naming conventions of flakes.

* [Setup:](./setup) Nix overlays and configuration
* [Server:](./server) HTTP API, Server executable, CLI-client executable
* [Static:](./static) Static files directly visible from HTML
* [Config:](./config) Runtime configuration with static files
* [Final:](./final) Wrapper and NixOS module

### Binary Cache

GitHub Actions pushes Nix results to Cachix.
Use this binary cache to speed up your local builds.

Configure your NixOS configuration to trust the binary cache.

```nix
{
  nix.settings = {
    substituters = [
      "https://jumper149-mensam.cachix.org"
    ];
    trusted-public-keys = [
      "jumper149-mensam.cachix.org-1:9502wAOm00GdLxZM8uTE4goaBGCpHb+d1jUt3dhR8ZM="
    ];
  };
}
```
