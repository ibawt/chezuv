FROM debian:sid

RUN apt-get update && apt-get install nginx chezscheme build-essential libuv1 openssl libssl-dev -y

RUN mkdir /app
COPY . /app
WORKDIR /app

ENTRYPOINT ["make", "test"]