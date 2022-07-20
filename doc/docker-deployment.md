
This document gives a step-by-step guide to deploying OCaml-CI from the
Docker images and how to integrate them into a GitHub organisation to
monitor PRs.

# Build the Docker images

```shell=
git clone --recursive https://github.com/ocurrent/ocaml-ci.git
cd ocaml-ci
docker build -t ocaml-ci-service .
docker build -f Dockerfile.web -t ocaml-ci-web .
```

# Create a GitHub App

Sign in to your [GitHub](github.com) account

1. Navigate to your account/organizations settings. ...
2. In the left sidebar, click Developer settings.
3. In the left sidebar, click GitHub Apps.
4. Click New GitHub App.
5. In "GitHub App name", type the name of your app. ...
6. Optionally, in "Description", type a description of your app that users will see.

## Complete the form

App Name: example-ci (must be unique across GitHub)
Homepage URL: https://ci.example.org/
Callback URL: https://ci.example.org:8100/login
Webhook URL: https://ci.example.org:8100/webhooks/github
Webhook Secret: Create a secure webhook secret - perhaps like this `ruby -rsecurerandom -e 'print SecureRandom.hex(20)'`

Give repository permissions:

* Checks: Read/Write
* Commit statuses: Read/Write
* Contents: Read-only
* Metadata: Read-only (default)
* Pull requests: Read-only

Subscribe to events:

* Create
* Pull request
* Push

Where can this GitHub App be installed? Any account

Once the app has been created record the App ID: 215418 (unique App ID)

# Docker Secrets

In GitHub, generate and download a private key for the new
app. i.e. `example-ci.2022-06-28.private-key.pem`

On the machine which will host the Docker containers, ensure it is in
swarm mode to allow the use of secrets:

```shell=
docker swarm init --advertise-addr 127.0.0.1:2377 --listen-addr 127.0.0.1:2377
```

Create the Docker secrets required for the app.

> Be careful not to have a trailing return character on the webhook secret.
> Also be mindful of your shell history when creating secrets.

This may be a good way to create a random secret in a file without a
return character:

```shell=
ruby -rsecurerandom -e 'print SecureRandom.hex(20)' > example-ci-webhook-secret
```

You will need a submission cap for a [OCurrent cluster](https://github.com/ocurrent/ocluster.git).

```shell=
docker secret create example-ci-github-key example-ci.2022-06-28.private-key.pem
docker secret create example-ci-webhook-secret example-ci-webhook-secret
docker secret create ocaml-ci-submission.cap cluster.cap
```

# Caddy Proxy

Setup Caddy to act as a proxy for the Docker services.  Create `/etc/caddy/Caddyfile` as follows

```
ci.example.org:8100 {
        reverse_proxy ci:8080
}               

ci.example.org {
        reverse_proxy web:8090
}
```

# Deploy the Docker Stack

Update [example-stack.yml](example-stack.yml):
- match the secret names created above
- update the application id

```shell=
docker stack deploy example --compose-file example-stack.yml
```

Check the webhook delivery status under your app's advanced settings:

https://github.com/organizations/example/settings/apps/example-ci/advanced

# Testing

Install the GitHub App for a user and select which repository you want to test

Check https://ci.example.org and https://ci.example.org:8100

