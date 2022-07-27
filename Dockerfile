FROM haskell:8.10.7 as builder

WORKDIR /bitcoin-dca

RUN apt update && apt install -y pkg-config libsecp256k1-0 libsecp256k1-dev

RUN stack update

COPY ./stack.yaml ./stack.yaml.lock ./package.yaml ./*.cabal ./

RUN stack build --only-dependencies

ADD app app
ADD src src
COPY ./LICENSE ./README.md ./ChangeLog.md ./
RUN stack install --local-bin-path /usr/bin

ENTRYPOINT ["bitcoin-dca"]

FROM builder AS tester
COPY test ./test
RUN stack test --no-run-tests --only-dependencies
RUN stack test
