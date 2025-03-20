FROM node:23-bookworm

# defaults, which are used when there's no --build-arg UID=$(id -u) --build-arg GID=$(id -g)
# The base image creates a user for us, but the uid and gid don't match the host system and we can't use --userns=keep-id.
# So we re-create the user.
ARG UID=1000
ARG GID=1000

RUN deluser --remove-home node
RUN groupadd -g $GID node && \
    useradd -u $UID \
        --gid $GID \
        --create-home \
        --no-log-init \
        node

RUN apt update && \
    apt install -y pandoc

USER node

RUN mkdir /home/node/blog
WORKDIR /home/node/blog

