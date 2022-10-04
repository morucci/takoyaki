# Takoyaki

Takoyaki is an experiment of a simple reactive Web framework
built over HTMX.

## Start

```
nix run
firefox http://127.0.0.1:8092
```

## Hack

```Shell
nix develop
ghcid -W -c 'cabal repl' -r="Demo.Counter.run"
firefox http://127.0.0.1:8092
```

