# syntax=docker/dockerfile:1
FROM ocaml/opam:debian-13-ocaml-4.14 AS build
RUN sudo ln -sf /usr/bin/opam-2.5 /usr/bin/opam && opam init --reinit -ni
RUN sudo rm -f /etc/apt/apt.conf.d/docker-clean; echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' | sudo tee /etc/apt/apt.conf.d/keep-cache
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    sudo apt update && sudo apt-get --no-install-recommends install -y \
    capnproto \
    graphviz \
    libev-dev \
    libffi-dev \
    libgmp-dev \
    libsqlite3-dev \
    m4 \
    pkg-config \
    libcapnp-dev
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 544825e77ff664abd7de5f51d723fbf53bec964d && opam update
RUN opam option --global solver=builtin-0install
COPY --chown=opam --link ocaml-ci.opam ocaml-ci-service.opam ocaml-ci-api.opam /src/
WORKDIR /src
# Pin current_* to the ocurrent branch carrying the proof-of-work challenge
# (Current_web.Challenge). All current_* must come from one commit.
RUN opam pin add -yn current.dev         "https://github.com/mtelvers/ocurrent.git#ca5cfef6fcd10aed438f096c448dc2f3747db1f3" && \
    opam pin add -yn current_web.dev     "https://github.com/mtelvers/ocurrent.git#ca5cfef6fcd10aed438f096c448dc2f3747db1f3" && \
    opam pin add -yn current_git.dev     "https://github.com/mtelvers/ocurrent.git#ca5cfef6fcd10aed438f096c448dc2f3747db1f3" && \
    opam pin add -yn current_github.dev  "https://github.com/mtelvers/ocurrent.git#ca5cfef6fcd10aed438f096c448dc2f3747db1f3" && \
    opam pin add -yn current_docker.dev  "https://github.com/mtelvers/ocurrent.git#ca5cfef6fcd10aed438f096c448dc2f3747db1f3" && \
    opam pin add -yn current_slack.dev   "https://github.com/mtelvers/ocurrent.git#ca5cfef6fcd10aed438f096c448dc2f3747db1f3" && \
    opam pin add -yn current_rpc.dev     "https://github.com/mtelvers/ocurrent.git#ca5cfef6fcd10aed438f096c448dc2f3747db1f3"
RUN --mount=type=cache,target=/home/opam/.opam/download-cache,sharing=locked,uid=1000,gid=1000 \
    opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune build ./_build/install/default/bin/ocaml-ci-service

FROM debian:13
RUN rm -f /etc/apt/apt.conf.d/docker-clean; echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' > /etc/apt/apt.conf.d/keep-cache
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt update && apt-get --no-install-recommends install -y \
    ca-certificates \
    curl \
    docker-cli \
    dumb-init \
    git \
    graphviz \
    libev4 \
    libsqlite3-dev \
    netbase \
    openssh-client
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/ocaml-ci-service"]
ENV OCAMLRUNPARAM=a=2
# Enable experimental for docker manifest support
ENV DOCKER_CLI_EXPERIMENTAL=enabled
COPY --from=build --link /src/_build/install/default/bin/ocaml-ci-service /usr/local/bin/
COPY --from=build --link /src/migrations /migrations
