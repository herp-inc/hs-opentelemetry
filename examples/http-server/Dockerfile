# syntax=docker/dockerfile:1
FROM nixos/nix:latest as build
WORKDIR /workdir
ENV LANG=C.UTF-8
ENV SYSTEM_CERTIFICATE_PATH=/root/.nix-profile/etc/ssl/certs
RUN mkdir -p /root/.config/nix
RUN cat <<EOT > /root/.config/nix/nix.config
max-jobs = auto
cores = 0
substituters = https://cache.nixos.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
EOT
RUN ["nix-env", "-iA", "nixpkgs.gnumake", "nixpkgs.stack"]
COPY . /workdir
RUN make -C examples/http-server bin/http-server
CMD [ "/workdir/examples/http-server/bin/http-server" ]
