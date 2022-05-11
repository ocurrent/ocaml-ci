# Local development

This document sets you up to use locally running instances of `ocaml-ci-server` and `ocaml-ci-web` to build an OCaml project that is in a repository owned by your GitHub user.

### Setting up a GitHub App

Since `ocaml-ci` is a GitHub App, create a GitHub App under your own user and point it to localhost via a webhook payload delivery service like [smee.io](https://smee.io).

To do this, follow the instructions in [Setting up your development environment to create a GitHub App](https://docs.github.com/en/developers/apps/getting-started-with-apps/setting-up-your-development-environment-to-create-a-github-app) but when it comes to setting permissions for your app, set the following:

```
Metadata: Read
Checks: Read and write
Commit statuses: Read and write
```

### Running the server locally

You will need the following:

1. The GitHub App ID of the app you created
2. The `pem` file containing the private key associated to the app
3. A capability file that has been generated for you by an ocaml-ci admin. This gives you the ability to submit jobs to the ci cluster
4. The app secret that is used to authenticate to the app

```
dune exec -- ocaml-ci-service \
  --github-app-id <your-github-app-id> \
  --github-private-key-file <path-to/private-key.pem> \
  --github-account-allowlist <your-github-username> \
  --submission-service <path-to-the-submission-capability-file> \
  --github-webhook-secret-file <path-to-the-app-secret> \
  --capnp-address tcp:127.0.0.1:9000
```

This will generate a capability file. See the logs for `Wrote capability reference to "./capnp-secrets/ocaml-ci-admin.cap"`

You should see the admin site on `localhost:8080`

### Running the web client locally

Using the capability file written out by the service, run the web client as follows:

```
dune exec -- ocaml-ci-web \
  --backend ./capnp-secrets/ocaml-ci-admin.cap
```

You should see the client site on `localhost:8090`

