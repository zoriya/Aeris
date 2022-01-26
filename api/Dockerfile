FROM haskell:8

WORKDIR /opt/aeris
RUN cabal update
COPY ./aeris.cabal /opt/aeris/aeris.cabal
RUN cabal build --only-dependencies -j4
COPY . /opt/aeris
RUN cabal install
CMD aeris
