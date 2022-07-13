This package is intended to replace web-ui with a [dream](https://aantron.github.io/dream/) based implementation. It coincides with a redesign of the front-end of `ocaml-ci.`

## Principles

These are some principles that we will hold to while implementing `dream-web:`

1. Do not modify `ocaml-ci-service` (improvements or changes that are needed should be documented)
2. Use dream's [templates](https://aantron.github.io/dream/#templates) instead of `Tyxml.html` -- the rationale for this principle is that it is easier for a designer to contribute to the project if they can stay within the realm of HTML, CSS and JS. Once progress is under way, we will revisit this principle to test the effectiveness of this strategy.
3. The layers of this package are:
   - Static assets (e.g. css) appear in `dream-web/static`
   - Templates are in `view` -- there should be minimal logic here
   - Controllers are in `dream-web/controller` -- these modules are responsible for getting data from the service (or backend) and calling the appropriate view with the data. There should be no template or markup related code here.
   - Routes in `dream-web/main.ml` -- these call controllers
4. Testing -- TBD


## Development

See also the development docs in [doc/dev.md](doc/dev.md) -- you will need to follow these to start up `ocaml-ci-service`

Using the capability file written out by the service:

``` shell
dune exec -- dream-web/main.exe --backend ./capnp-secrets/ocaml-ci-admin.cap
```

This should result in a client site on `localhost:8090`
