CONTEXT := ci.ocamllabs.io

all:
	dune build ./service/main.exe ./client/main.exe

deploy:
	docker --context $(CONTEXT) build -t ocaml-ci-service .

deploy-stack:
	docker --context $(CONTEXT) stack deploy --prune -c stack.yml ocaml-ci
