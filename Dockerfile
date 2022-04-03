
FROM ubuntu:18.04 as base

WORKDIR /root

# Install prerequisites
COPY docker/install_prereqs.sh .
RUN ./install_prereqs.sh && rm -rf /var/lib/apt/lists/*

# Install dependencies of Chisel project
RUN git clone https://github.com/althebaker43/processing-module.git && \
    cd processing-module && \
    sbt test clean && \
    cd .. && rm -r processing-module

# Install xterm for UI
RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get install -y xterm

# Install emacs as main editor
RUN apt-get update && apt-get install -y libxpm-dev libjpeg-dev libgif-dev libtiff-dev gnutls-dev
RUN wget http://mirror.us-midwest-1.nexcess.net/gnu/emacs/emacs-27.2.tar.gz && \
    tar xf emacs-27.2.tar.gz && \
    mkdir emacs-build && cd emacs-build && ../emacs-27.2/configure && \
    make && make install && \
    cd .. && rm -r emacs-27.2.tar.gz emacs-27.2 emacs-build
COPY docker/.emacs .
COPY docker/init.el ./.emacs.d/
COPY docker/elpa/ ./.emacs.d/elpa/

# Set up bloop and metals
RUN mkdir bloop && cd bloop && \
    curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes && \
    ./cs install bloop && \
    ./cs install metals
ENV PATH="$PATH:/root/.local/share/coursier/bin:/root/.local/share/metals/bin"

# Include gtkwave
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get -y install tzdata
RUN apt-get install -y gtkwave

CMD ["xterm"]
