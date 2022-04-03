#!/bin/bash

set -ex

export DEBIAN_FRONTEND=noninteractive 

apt-get update
apt-get install -y build-essential bison flex software-properties-common curl
apt-get install -y libgmp-dev libmpfr-dev libmpc-dev zlib1g-dev vim default-jdk default-jre
# install sbt: https://www.scala-sbt.org/release/docs/Installing-sbt-on-Linux.html#Ubuntu+and+other+Debian-based+distributions
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
apt-get update
apt-get install -y sbt
apt-get install -y texinfo gengetopt
apt-get install -y libexpat1-dev libusb-dev libncurses5-dev cmake
# deps for poky
apt-get install -y python3.8 patch diffstat texi2html texinfo subversion chrpath wget
# deps for qemu
apt-get install -y libgtk-3-dev gettext
# deps for firemarshal
apt-get install -y python3-pip python3.8-dev rsync libguestfs-tools expat ctags
# install DTC
apt-get install -y device-tree-compiler
apt-get install -y python
# install git >= 2.17
add-apt-repository ppa:git-core/ppa -y
apt-get update
apt-get install git -y

# install verilator
apt-get install -y autoconf
git clone http://git.veripool.org/git/verilator
cd verilator
git checkout v4.034
autoconf && ./configure && make -j$(nproc) && make install && make clean
