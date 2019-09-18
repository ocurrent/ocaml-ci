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

[OCurrent]: https://github.com/talex5/ocurrent
[pipeline.ml]: https://github.com/talex5/ocaml-ci/blob/master/src/pipeline.ml
