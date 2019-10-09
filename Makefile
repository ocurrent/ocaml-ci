CONTEXT := ci.ocamllabs.io

all:
	dune build ./service/main.exe ./client/main.exe ./web-ui/main.exe ./service/local.exe

deploy-backend:
	docker --context $(CONTEXT) build -t ocaml-ci-service .

deploy-web:
	docker --context $(CONTEXT) build -f Dockerfile.web -t ocaml-ci-web .

deploy-stack:
	docker --context $(CONTEXT) stack deploy --prune -c stack.yml ocaml-ci
