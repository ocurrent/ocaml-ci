# OCaml-CI

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focurrent%2Focaml-ci%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/ocurrent/ocaml-ci)

Status: **experimental**

This is an [OCurrent][] pipeline that provides CI for OCaml projects hosted on GitHub.

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
2. Configure just the repositories you want to test (start with one!).
3. Ask us to add you to the alpha-testers list by submitting a PR against this
   repository adding yourself to `--github-account-whitelist` in `stack.yml`.
4. Report bugs :-)

## Installation

Get the code with:

```sh
git clone --recursive https://github.com/ocurrent/ocaml-ci.git
```

Note: you need to clone with `--recursive` because this project uses submodules
(it depends on some packages that aren't released yet).
If you forget, `git submodule update --init` will fetch them.

To test the CI on a local Git clone, use:

```sh
dune exec -- ocaml-ci-local /path/to/project
```

This will build the project as the real CI would,
but it only monitors the default branch and does not push the results anywhere.
It runs a web interface at <http://localhost:8080>.
This is useful if you want to try out changes to the pipeline.

If you want to build the whole system, the easiest way is using Docker:

```sh
docker build -t ocaml-ci-service .
docker build -f Dockerfile.web -t ocaml-ci-web .
```

The `stack.yml` contains the configuration used on the live system.
You'll have to register your own GitHub app to be able to test the services locally.

## Remote API

The service provides a [Cap'n Proto endpoint][capnp-api] and a command-line client that uses it.
You will need to be given the `ocaml-ci.cap` file, which grants access to the API.
The client can be built and run using `dune exec -- ocaml-ci --ci-cap=ocaml-ci.cap ...`, or
installed as `ocaml-ci`.

To see the branches and PRs that ocaml-ci is monitoring in a repository:

```bash
$ ocaml-ci mirage/irmin
615364620f4233cb82a96144824eb6ad5d1104f0 refs/heads/1.4
e0fcf0d336544650ca5237b356cfce4a48378245 refs/heads/master
6c46d1de5e67a3f504fc55af1d644d852c946533 refs/heads/mirage-dev
28421a152e8e19b3fb5048670629e7e01d0fbea6 refs/pull/523/head
acfbee7e82fcaaa5a0dad900068dc67f22021f2e refs/pull/678/head
3fc04e9f6e7574c0f61eacb3187b412b3bababe4 refs/pull/728/head
32f6c9f303616880994998881ee75c8d1fe0df91 refs/pull/771/head
b2d4b06f94d13384ae08eb06439ce9c6066419cd refs/pull/815/head
d8161e6cbf06c3005a080d4df209f7de67d6fa5c refs/pull/851/head
5e36237d7ce6279878578cf48d8b63937c499e5a refs/pull/858/head
04a368ecd52ea436bfcd252ed94772f55b5159d5 refs/pull/866/head
2e838b491a4c0b21750f7a2e6dee88eee1c7d94e refs/pull/867/head
```

You can pass either the reference (e.g. `refs/heads/master`) or the commit hash to choose one of them.

```bash
$ ocaml-ci mirage/irmin refs/heads/master
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

[OCurrent]: https://github.com/ocurrent/ocurrent
[pipeline.ml]: https://github.com/ocurrent/ocaml-ci/blob/master/service/pipeline.ml
[capnp-api]: https://github.com/ocurrent/ocaml-ci/blob/master/api/schema.capnp
