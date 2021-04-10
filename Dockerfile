FROM ocaml/opam:latest

ENV DEBIAN_FRONTEND=noninteractive

RUN sudo apt-get install -y gcc-multilib

RUN opam install depext
RUN opam install user-setup
RUN opam depext menhir dune ounit
RUN opam install menhir dune ounit
RUN opam user-setup install
RUN opam install ocaml-lsp-server ocamlformat

RUN sudo sh -c "echo 'eval $(opam config env)' >> /etc/bash.bashrc"