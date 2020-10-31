FROM bitnami/minideb:buster

ARG USER_ID

ARG GROUP_ID

RUN apt-get update && apt-get install -y ruby bundler rake

RUN apt-get install -y sudo

RUN addgroup --gid $GROUP_ID user
RUN adduser --disabled-password --gecos '' --uid $USER_ID --gid $GROUP_ID user

RUN adduser user sudo

RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

RUN ln -sf /usr/share/zoneinfo/America/New_York /etc/localtime

USER user

WORKDIR /home/user

COPY Gemfile .
COPY Gemfile.lock .

ENV LANG C.UTF-8

RUN bundle install --frozen -j 3 --no-cache

WORKDIR /home/user/Blog
