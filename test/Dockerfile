FROM debian:sid

RUN apt-get update && apt-get install nginx chezscheme build-essential libuv1 openssl libssl-dev -y
ADD https://github.com/summerwind/h2spec/releases/download/v2.2.0/h2spec_linux_amd64.tar.gz /tmp
RUN tar xf /tmp/h2spec_linux_amd64.tar.gz && cp h2spec /usr/local/bin && rm h2spec /tmp/*.tar.gz

RUN mkdir /app
COPY . /app
WORKDIR /app

ENTRYPOINT ["make", "test"]
