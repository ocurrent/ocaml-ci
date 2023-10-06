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

You will need the following:

1. The GitHub App ID of the app you created
2. A comma-separated list of GitHub accounts to allow—this could start out as just your GitHub account
3. A file `private-key.pem` containing the private key associated with the app
4. A file `webhook-secret` containing the webhook secret that is used to authenticate with the app

`private-key.pem` and `webhook-secret` must be stored in the same directory on the host. The directory path will be needed.

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
APP_ID=... ALLOW_LIST=... SECRETS_DIR=... docker compose up
```

You should see the admin site on [`http://localhost:8100`](http://localhost:8100) and the user site on [`http://localhost`](http://localhost).

Alternatively, you can store the environment variables in a `.env` file at the root of the project. For example:
```
APP_ID=359343
ALLOW_LIST="myusername,ocurrent"
SECRETS_DIR=$HOME/ci_secrets/
```

You can then run the compose as simply:
```
docker compose up
```

If you want webhooks to be directed to your application, start `smee` in another terminal, replacing the argument to `--url` with the URL you generated before on [smee.io](https://smee.io):
```
smee --url https://smee.io/xxxxxxxxxxxxxxxx --path /webhooks/github --port 8100
```

Make sure that the GitHub App is sending webhooks to the URL, specified in its settings.

## Migrations

Migrations are managed using `omigrate.` If you are using an opam switch for ocaml-ci then `omigrate` should be installed and you can create a new migration by doing this from the project root:

```
omigrate create --dir migrations <migration-name>
```

This will create timestamped files in the `migrations` directory that you can then populate with the sql necessary to introduce and remove the migration (in the `up` and `down` files respectively).

Migrations will not run unless the `--migration-path` flag is present when invoking `ocaml-ci-service.`
