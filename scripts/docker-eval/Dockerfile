FROM ubuntu

RUN apt-get update && apt-get install -y apt-transport-https

RUN apt-get install -y git make gcc flex bison libssl-dev bc libelf-dev

COPY linux /linux

COPY docker-checkout-make.sh /docker-checkout-make.sh

RUN chmod +x /docker-checkout-make.sh

ENTRYPOINT /docker-checkout-make.sh