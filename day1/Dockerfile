FROM swipl:latest
MAINTAINER Daniel Schruhl <danielschruhl@gmail.com>

ADD *.pl /app/
ADD ./resources /app/resources
WORKDIR /app
CMD ["swipl", "/app/main.pl"]
