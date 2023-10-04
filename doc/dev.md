# Local development

This document sets you up to use local instances of `ocaml-ci-service` and `ocaml-ci-web` running in Docker containers to build an OCaml project that is in a repository owned by your GitHub user.

## Setting up a GitHub App

`ocaml-ci` uses the functionality of a GitHub App to interact with GitHub. To use `ocaml-ci` yourself you must create your own. You can create a GitHub App ([settings/apps](https://github.com/settings/apps)) under your own user and point it to localhost via a webhook payload delivery service like [smee.io](https://smee.io).

To do this, follow the instructions in [Setting up your development environment to create a GitHub App](https://docs.github.com/en/developers/apps/getting-started-with-apps/setting-up-your-development-environment-to-create-a-github-app) but when it comes to setting permissions for your app, set the following as the "Repository permissions":

```
Checks: Read and write
Commit statuses: Read and write
Contents: Read-only
Metadata: Read-only
Pull requests: Read-only
```

Also, subscribe to the following events:

```
Create
Pull request
Push
```

## Running the GitHub pipeline locally

In `docker-compose.yml` change the following:

1. Under `services`, then `service`, change the argument to `--github-app-id` to the GitHub App ID of the app you created
2. Under the same `service` tag, change the argument to `--github-account-allowlist` to a comma-separated list of GitHub accounts to allow—this could start out as just your GitHub account
3. Under `secrets`, set `ocaml-ci-github-key` to the `pem` file containing the private key associated to the app, and `ocaml-ci-webhook-secret` to a file containing the webhook secret that is used to authenticate to the app

Create a file `/etc/caddy/Caddyfile` containing:
```
{
	log default {
		level WARN
	}
}

http://localhost:8100 {
	reverse_proxy service:8080
}

http://localhost {
	reverse_proxy web:8090
}
```

You can then start the services with:

```
docker compose up
```

You should see the admin site on [`http://localhost:8100`](http://localhost:8100) and the user site on [`http://localhost`](http://localhost).

If you want webhooks to be redirected to your application, start `smee` in another terminal, replacing the argument to `--url` with the URL you generated before on [smee.io](https://smee.io):

```
smee --url https://smee.io/xxxxxxxxxxxxxxxx --path /webhooks/github --port 8100
```

## Migrations

Migrations are managed using `omigrate.` If you are using an opam switch for ocaml-ci then `omigrate` should be installed and you can create a new migration by doing this from the project root:

```
omigrate create --dir migrations <migration-name>
```

This will create timestamped files in the `migrations` directory that you can then populate with the sql necessary to introduce and remove the migration (in the `up` and `down` files respectively).

Migrations will not run unless the `--migration-path` flag is present when invoking `ocaml-ci-service.`
