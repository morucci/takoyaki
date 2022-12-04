# Takoyaki

Takoyaki is an experiment of a simple reactive Web framework
built over HTMX.

It provides some Demo applications:

- MineSweeper
- Todolist

## Start

For instance to run the MineSweeper app from the nix flake:

```
nix run .# -- MineSweeper
firefox http://127.0.0.1:8092
```

Also, the container can be built this way;

```
nix build .#container
podman load < ./result
# Then
mkdir ~/takoyaki-home
podman run -it --rm --network host -v ~/takoyaki-home:/var/lib/takoyaki:Z docker.io/morucci/takoyaki:latest takoyaki MineSweeper
firefox http://127.0.0.1:8092
```

## Hack

```Shell
nix develop
ghcid -W -c 'cabal repl' -r="Demo.Todo.run 8092"
ghcid -W -c 'cabal repl' -r="Demo.MineSweeper.run 8092"
firefox http://127.0.0.1:8092
# Run your code editor in the nix shell
code .
```

