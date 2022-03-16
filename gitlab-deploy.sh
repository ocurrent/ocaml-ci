#!/bin/bash -ex

docker -c tezos.ci.dev build -t ocaml-ci-gitlab -f Dockerfile.gitlab .
docker -c tezos.ci.dev build -t ocaml-ci-github-web -f Dockerfile.web .
docker -c tezos.ci.dev stack rm ocaml-ci-gitlab
sleep 15
docker -c tezos.ci.dev stack deploy -c gitlab-stack.yml ocaml-ci-gitlab
