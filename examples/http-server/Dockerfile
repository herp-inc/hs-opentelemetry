FROM nixos/nix:latest as build
WORKDIR /workdir
ENV LANG=C.UTF-8
ENV SYSTEM_CERTIFICATE_PATH=/root/.nix-profile/etc/ssl/certs
RUN nix-env -iA nixpkgs.stack nixpkgs.gnumake
COPY . /workdir
RUN make -C examples/http-server bin/http-server
CMD [ "/workdir/examples/http-server/bin/http-server" ]
