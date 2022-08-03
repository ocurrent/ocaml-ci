This package is a web-ui with a [dream](https://aantron.github.io/dream/)
based implementation. It was written to coincide with a redesign of the front-end of `ocaml-ci.`

## Principles

These are some principles for `web-ui` that we will hold to:

- The layers of this package are:
  - Static assets (e.g. css) appear in `web-ui/static` and are served using `ocaml-crunch`
  - Templates are in `view` -- there should be minimal logic here
  - Controllers are in `controller` -- these modules are responsible for getting data
    from the service (or backend) and calling the appropriate view with the data.
    There should be no template or markup related code here.
  - Routes in `web-ui/main.ml` -- these call controllers

3. Testing -- TBD

## Development

See also the development docs in [doc/dev.md](doc/dev.md) -- you will need to follow these to
start up `ocaml-ci-service`

Using the capability file written out by the service:

```shell
dune exec -- web-ui/main.exe --backend ./capnp-secrets/ocaml-ci-admin.cap
```

This should result in a client site on `localhost:8090`
