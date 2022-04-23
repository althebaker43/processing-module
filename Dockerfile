
FROM ubuntu:18.04 as base

WORKDIR /root

# Install prerequisites
COPY docker/install_prereqs.sh .
RUN ./install_prereqs.sh && rm -rf /var/lib/apt/lists/*

# Install terminal emulator for UI
RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y terminator

# Install emacs as main editor
RUN apt-get update && apt-get install -y libxpm-dev libjpeg-dev libgif-dev libtiff-dev gnutls-dev
RUN wget http://mirror.us-midwest-1.nexcess.net/gnu/emacs/emacs-27.2.tar.gz && \
    tar xf emacs-27.2.tar.gz && \
    mkdir emacs-build && cd emacs-build && ../emacs-27.2/configure && \
    make && make install && \
    cd .. && rm -r emacs-27.2.tar.gz emacs-27.2 emacs-build

# Include gtkwave
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get -y install tzdata
RUN apt-get install -y gtkwave

# Add user
ARG user
RUN adduser --disabled-password $user
USER $user
WORKDIR /home/$user

# Install dependencies of Chisel project
# RUN git clone https://github.com/althebaker43/processing-module.git && \
#     cd processing-module && \
#     sbt test clean && \
#     cd .. && rm -r processing-module
COPY --chown=$user build.sbt processing-module/build.sbt
COPY --chown=$user src processing-module/src
COPY --chown=$user project/build.properties processing-module/project/build.properties
COPY --chown=$user project/plugins.sbt processing-module/project/plugins.sbt
RUN cd processing-module && \
    sbt test clean && \
    cd .. && rm -r processing-module

# Set up bloop and metals
RUN mkdir bloop && cd bloop && \
    curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes && \
    ./cs install bloop && \
    ./cs install metals
ENV PATH="$PATH:/home/$user/.local/share/coursier/bin:/home/$user/.local/share/metals/bin"

# Copy emacs config to home
COPY --chown=$user docker/.emacs .
COPY --chown=$user docker/init.el ./.emacs.d/
COPY --chown=$user docker/elpa/ ./.emacs.d/elpa/

COPY --chown=$user docker/terminator_config.txt ./.config/terminator/config

CMD ["terminator", "-u"]
