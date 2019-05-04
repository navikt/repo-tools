FROM ubuntu:18.04

RUN apt-get -y update && apt-get -y install \
  netbase \
  ca-certificates \
  curl

COPY webproxy.crt /usr/local/share/ca-certificates/

RUN mkdir -p /opt/app

WORKDIR /opt/app

ADD .stack-work/install/x86_64-linux/lts-13.16/8.6.4/ .

COPY frontend/build /opt/app/frontend/build

COPY config /opt/app/config

COPY scripts/run.sh /opt/app/run.sh

EXPOSE 3000

CMD ["/opt/app/run.sh"]
