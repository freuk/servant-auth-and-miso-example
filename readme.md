[miso](https://github.com/dmjio/miso) version of:
- [mchaver/servant-auth-and-elm-example](https://github.com/mchaver/servant-auth-and-elm-example)
- [mchaver/servant-auth-and-purescript-pux-example](https://github.com/mchaver/servant-auth-and-purescript-pux-example)

**building/serving locally**  

As file `default.nix` shows, this repository uses both nixpkgs
20.03 pinned through a github release tarball and the rolling master miso
package set tarball. The former is only used for tooling additions (see
`shell.nix`), and is expected to be stable. The latter can be expected to
change, thus breaking the build in the future.

```
# optional use of cache
$ nix-env -iA cachix -f https://cachix.org/api/v1/install
# optional use of cache
$ cachix use miso-haskell

$ nix-build -A app
$ cd result
$ bin/server
```

**interactive development**  

This implementation doesn't support jsaddle. `ghcid` usage is as follows.

```
$ nix-shell
nix-shell$ ghcid -c "cabal v2-repl client"
nix-shell$ ghcid -c "cabal v2-repl serverlib"
```
