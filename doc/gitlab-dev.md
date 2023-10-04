## Running the GitLab pipeline locally

You will need the following:

1. The GitLab API token with permissions to the repositories to build
2. GitLab secret associated with webhooks
3. A capability file for submitting jobs to a cluster, in this case the main ocaml-ci cluster as documented in https://github.com/ocurrent/ocluster#admin

``` shell
dune exec -- ocaml-ci-gitlab \
  --gitlab-token-file <your-gitlab-token> \
  --gitlab-webhook-secret-file <your-gitlab-secret> \
  --submission-service <path-to-the-submission-capability-file> \
  --capnp-listen-address tcp:127.0.0.1:9800
  --migration-path "$PWD/migrations"
```

This will generate a capability file. See the logs for `Wrote capability reference to "./capnp-secrets/ocaml-ci-gitlab-admin.cap"`
