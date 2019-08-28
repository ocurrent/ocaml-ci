CONTEXT := ci.ocamllabs.io

all:
	dune build ./src/main.exe

deploy:
	docker --context $(CONTEXT) build -t ocaml-ci .

deploy-stack:
	docker --context $(CONTEXT) stack deploy --prune -c stack.yml ocaml-ci
