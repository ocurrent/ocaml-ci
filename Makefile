CONTEXT := ocaml.ci.dev

all:
	dune build ./service/main.exe ./client/main.exe ./web-ui/main.exe ./service/local.exe @runtest

deploy-backend:
	env DOCKER_BUILDKIT=1 docker --context $(CONTEXT) build -t ocaml-ci-service .

deploy-web:
	env DOCKER_BUILDKIT=1 docker --context $(CONTEXT) build -f Dockerfile.web -t ocaml-ci-web .

orgs := $(shell cat deploy-data/github-organisations.txt | tr '\n' ',')

stack.yml: stack.yml.in deploy-data/github-organisations.txt
	sed 's/GITHUB_ORGANISATIONS/${orgs:,=}/' stack.yml.in > stack.yml

deploy-stack: stack.yml
	docker --context $(CONTEXT) stack deploy --prune -c stack.yml ocaml-ci
