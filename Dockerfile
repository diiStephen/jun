FROM alpine:edge

RUN apk --no-cache add binutils-gold curl gcc g++ git gmp-dev ncurses-dev ncurses-static libffi-dev make xz tar perl zlib-dev zlib-static

RUN mkdir -p ~/.ghcup/bin && curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > ~/.ghcup/bin/ghcup && chmod +x ~/.ghcup/bin/ghcup
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"

RUN ghcup install ghc 8.10.7
RUN ghcup set ghc 8.10.7
RUN ghcup install cabal

RUN cabal update

RUN mkdir -p /usr/jun

ADD ./release/jun-0.1.0.0.tar.gz /usr/jun
WORKDIR /usr/jun/jun-0.1.0.0

RUN cabal build jun-release
RUN cabal install exe:jun-release --install-method=copy --installdir=./

ENTRYPOINT ["/bin/sh"]