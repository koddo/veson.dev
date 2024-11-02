FROM node:23-bookworm

ARG UID=1000
ARG GID=1000
RUN groupadd -g $GID theuser && \
    useradd -u $UID \
        --gid $GID \
        --create-home \
        --no-log-init \
        theuser && \
    mkdir /home/theuser/blog

RUN apt update && \
    apt install -y pandoc

USER theuser
WORKDIR /home/theuser/blog
