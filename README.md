# OCaml-CI

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Focaml.ci.dev%2Fbadge%2Focurrent%2Focaml-ci%2Fmaster&logo=ocaml)](https://ocaml.ci.dev/github/ocurrent/ocaml-ci)

This is an [OCurrent][] pipeline that provides CI for OCaml projects hosted on GitHub.
It uses metadata from the project’s opam and dune files to work out what to build,
and uses caching to make builds fast. It automatically tests projects against
multiple OCaml versions and OS platforms.

The pipeline is defined in [pipeline.ml][]. It:

1. Gets the list of installations of its GitHub app.
2. For each installation, gets the list of repositories to check.
3. For each repository, gets the branches and PRs to check.
4. For each target, it fetches the head commit, generates a Dockerfile and builds it.

The generated Dockerfile first adds all the `*.opam` files found in the project,
then uses `opam` to install all the dependencies, then adds the rest of the source
files. This means that rebuilds are often very fast, because Docker will reuse the
previously cached build step as long as the opam files don't change.

To add the CI to your own project:

1. Go to https://github.com/apps/ocaml-ci and install the app for your GitHub user.
2. Configure just the repositories you want to test (start with one!). If you select `All Repositories` we won't build anything.
3. Ask us to add you to the alpha-testers list by submitting a PR against this
   repository adding yourself to `--github-account-allowlist` in `stack.yml`. eg https://github.com/ocurrent/ocaml-ci/pull/346. Additionally, please add yourself to `deploy-data/github-organisations.txt`.
4. Add a status badge from the OCaml-CI endpoint with:
   ```
   [![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ocaml.ci.dev/badge/<user>/<repo>/<branch>&logo=ocaml)](https://ocaml.ci.dev/github/<user>/<repo>)
   ```
5. Report bugs :-)

## Installation

Get the code with:

```sh
git clone --recursive https://github.com/ocurrent/ocaml-ci.git
cd ocaml-ci
opam install --deps-only ./ocaml-dockerfile ./ocluster ./ocurrent ./solver-service .
```

Note: you need to clone with `--recursive` because this project uses submodules
(it depends on some packages that aren't released yet).
If you forget, `git submodule update --init --recursive` will fetch them.

To test the CI on a local Git clone, you need to first install the
dependencies and then use `dune exec` as shown below:

```sh
opam update
opam install . --deps-only --with-test --yes
dune exec -- ocaml-ci-local /path/to/project
```

This will build the project as the real CI would,
but it only monitors the default branch and does not push the results anywhere.
It runs a web interface at <http://localhost:8080>.
This is useful if you want to try out changes to the pipeline.

If you want to build the whole system, the easiest way is using Docker:

```sh
docker build -f Dockerfile -t ocaml-ci-service .
docker build -f Dockerfile.gitlab -t ocaml-ci-gitlab .
docker build -f Dockerfile.web -t ocaml-ci-web .
```

The `stack.yml` contains the configuration used on the live system.
You'll have to register your own GitHub app to be able to test the services locally.

If you want it to update to changes in opam-repository automatically you'll also need
to register a webhook there sending push events to the CI's `/webhooks/github` path.

[This document](doc/docker-deployment.md) takes you through the steps
necessary to deploy the Docker images.

## Remote API

The service provides a [Cap'n Proto endpoint][capnp-api] and a command-line client that uses it.
You will need to be given the `ocaml-ci.cap` file, which grants access to the API.
The client can be built and run using `dune exec -- ocaml-ci --ci-cap=ocaml-ci.cap ...`, or
installed as `ocaml-ci`.

To see the branches and PRs that ocaml-ci is monitoring in a repository:

```bash
$ ocaml-ci mirage/irmin
615364620f4233cb82a96144824eb6ad5d1104f0 refs/heads/1.4 (passed)
e0fcf0d336544650ca5237b356cfce4a48378245 refs/heads/master (passed)
6c46d1de5e67a3f504fc55af1d644d852c946533 refs/heads/mirage-dev (passed)
28421a152e8e19b3fb5048670629e7e01d0fbea6 refs/pull/523/head (passed)
acfbee7e82fcaaa5a0dad900068dc67f22021f2e refs/pull/678/head (passed)
3fc04e9f6e7574c0f61eacb3187b412b3bababe4 refs/pull/728/head (passed)
32f6c9f303616880994998881ee75c8d1fe0df91 refs/pull/771/head (passed)
b2d4b06f94d13384ae08eb06439ce9c6066419cd refs/pull/815/head (failed)
d8161e6cbf06c3005a080d4df209f7de67d6fa5c refs/pull/851/head (passed)
5e36237d7ce6279878578cf48d8b63937c499e5a refs/pull/858/head (failed)
04a368ecd52ea436bfcd252ed94772f55b5159d5 refs/pull/866/head (passed)
2e838b491a4c0b21750f7a2e6dee88eee1c7d94e refs/pull/867/head (passed)
```

You can pass either the reference (e.g. `refs/heads/master`) or the commit hash to choose one of them.

```bash
$ ocaml-ci mirage/irmin refs/heads/main
alpine-3.10-ocaml-4.08
```

To view the log (following it if incomplete):

```bash
$ ocaml-ci mirage/irmin refs/heads/master alpine-3.10-ocaml-4.08 log
[...]
- Test Successful in 17.643s. 99 tests run.
-> compiled  irmin-unix.dev
-> installed irmin-unix.dev
Done.
# Run eval $(opam env) to update the current shell environment
Removing intermediate container 4c85cdc76ddc
 ---> c8e34c3b5eee
Successfully built c8e34c3b5eee
2019-09-25 14:55.57: Job succeeded
```

Instead of `log`, you can also use `cancel`, `rebuild` or `status`.

For convenience, you can omit the leading `refs/` when specifying a reference,
and for PRs you can omit the trailing `/head`. For commits, you must give at
least the first 6 characters. e.g.

```bash
$ ocaml-ci mirage/irmin pull/867 alpine-3.10-ocaml-4.08 cancel
```

## Deployment

`ocaml-ci` is deployed as three docker images built from `Dockerfile`, `Dockerfile.gitlab` and `Dockerfile.web`, with
the live service following `live-engine` for the backend and `live-www` for the frontend.
An ocurrent-deployer [pipeline](deploy.ci.dev) watches these branches, performing a docker build
and deploy whenever it sees a new commit. The live branches should typically contain commits from `master` plus potentially
short lived commits for testing changes that are later merged into `master`.

To deploy code changes either from `master` or a branch:
 * check that you've rebased the changes onto master
 * git push -u upstream HEAD:live-engine or
 * git push -u upstream HEAD:live-www

To deploy changes to `stack.yml` run (assuming a docker context with sufficient access):

``` bash
docker -c ocaml.ci.dev stack deploy -c stack.yml ocaml-ci
```

## Opam repository updates

When it is updated [`opam-repository`](https://github.com/ocaml/opam-repository) sends a webhook to `Ocaml-ci` triggering its pipelines.
This mechanism allows builds to remain up to date with changes in the opam package ecosystem. For further details of this webhook,
please contact a maintainer of `opam-repository`.


## Local development

See [this document](doc/dev.md) for set up and running the server and web components locally.

[OCurrent]: https://github.com/ocurrent/ocurrent
[pipeline.ml]: https://github.com/ocurrent/ocaml-ci/blob/master/service/pipeline.ml
[capnp-api]: https://github.com/ocurrent/ocaml-ci/blob/master/api/schema.capnp

