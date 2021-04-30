FROM bitnami/minideb:buster

ARG USER_ID

ARG GROUP_ID

RUN apt-get update && \
    apt-get install -y ruby bundler python2.7 rake sudo

RUN addgroup --gid $GROUP_ID user && \
    adduser --disabled-password --gecos '' \
            --uid $USER_ID --gid $GROUP_ID user && \
    adduser user sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers && \
    ln -sf /usr/share/zoneinfo/America/New_York /etc/localtime && \
    ln -sf /usr/bin/python2.7 /usr/bin/python

USER user

WORKDIR /home/user

COPY Gemfile Gemfile[.]lock  ./

ENV LANG C.UTF-8

RUN bundle install \
    $([ -f Gemfile.lock ] && echo --frozen) \
    -j 3 --no-cache

WORKDIR /home/user/Blog
