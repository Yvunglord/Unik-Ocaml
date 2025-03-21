FROM ocaml/opam:ubuntu-22.04-ocaml-5.0
WORKDIR /app
COPY . .
RUN opam install . --deps-only -y
RUN eval $(opam env)
CMD ["dune", "build"]