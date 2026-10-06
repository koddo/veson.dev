FROM docker.io/debian:13-slim

# Defaults. Override with --build-arg UID=$(id -u) --build-arg GID=$(id -g) to match host system.
# We need this, when we have local volumes. Otherwise we'll have problems with permissions.
ARG UID=1000
ARG GID=1000

RUN groupadd -g $GID appuser && \
    useradd -u $UID \
        --gid $GID \
        --create-home \
        --no-log-init \
        appuser

RUN apt-get update && \
    apt-get install -y \
                    curl \
                    # xz is for the nix installer \
                    xz-utils

# We have to create this dir, otherwise the nix installer would try and fail this in sudo.
RUN mkdir -m 0755 /nix && chown appuser:appuser /nix
USER appuser
WORKDIR /home/appuser
ENV USER=appuser
# $USER has to be set for the nix installer.

# Single-user installation, see https://nixos.org/download/
RUN curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install | sh -s -- --no-daemon

# Cache dependencies for shell.nix in the image, otherwise nix-shell is going to download them all the time.
COPY shell.nix /tmp
COPY npins /tmp/npins
RUN ls -al /tmp
RUN . $HOME/.nix-profile/etc/profile.d/nix.sh && \
    nix-shell /tmp/shell.nix \
              --run true    # a no-op, just to make it fetch dependencies

# By default run bash in the shell.nix environment.
# The command for running the actual app is configured in docker-compose.yml.
ENTRYPOINT ["sh", "-c", ". $HOME/.nix-profile/etc/profile.d/nix.sh && nix-shell --quiet --command \"$@\"", "--"]
CMD ["bash"]

RUN echo "npm ci && npx @11ty/eleventy --serve --output=./_site" >> ~/.bash_history
# to ../_site because /home/appuser/workspace is :ro
