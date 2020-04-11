#!/bin/bash -ex

docker build -t ocaml-ci-web -f Dockerfile.web .
docker build -t ocaml-ci-service -f Dockerfile .
docker stack rm opam-ci
sleep 15
docker stack deploy -c stack.yml opam-ci
