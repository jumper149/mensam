# Mensam

A desk-booking web application.

View the [OpenAPI specification](https://redocly.github.io/redoc/?url=https://raw.githubusercontent.com/jumper149/mensam/main/final/openapi.json).

## Usage

__This section describes the goal of the MVP.__

Are you hosting a **coworking space**, a **hot-desk office** or a community space like a **hacker space**?

Desks are a limited resource.
Your members will be disappointed when they arrive at your location and realize there aren't any desks available.
Sometimes there are desks available, but a user has some special requirement like a power outlet or the user just wants to sit close to a window.

In these cases you probably want to set up a desk-booking solution like __Mensam__.

### Setup

As the operator you simply visit an instance of Mensam, such as [mensam.felixspringer.xyz](https://mensam.felixspringer.xyz).

First you will have to create a __User__ account for yourself.
Now you create a new __Space__ for your location.
Within this __Space__ you can now add __Desks__.

### Join a Space

After creating a __User__ account you can simply join a __Space__ with a single click.
Depending on the setup of the __Space__, it is listed publically or only accessible with a specific link.

### Book a desk

After joining a __Space__ you can create a __Reservation__ for a __Desk__.
You will be able to see other __Reservations__, so you can plan around them.
Mensam will deny overlapping reservations.

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
