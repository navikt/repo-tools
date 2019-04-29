FROM fpco/stack-build:lts-13.16 as build

RUN mkdir /opt/build

COPY stack.yaml repo-tools.cabal /opt/build/

RUN cd /opt/build && stack build

COPY . /opt/build

RUN cd /opt/build && stack clean && stack build --system-ghc

FROM ubuntu:18.04

# RUN apk --update add ca-certificates

RUN mkdir -p /opt/app

ARG BINARY_PATH

WORKDIR /opt/app

COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.16/8.6.4/bin .

COPY frontend/build /opt/app/frontend/build

COPY config /opt/app/config

EXPOSE 3000

CMD ["/opt/app/repo-tools"]
