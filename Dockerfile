FROM rocker/shiny

MAINTAINER Olivia Brode-Roger "olivia@olivia-data.com"

RUN apt-get update && apt-get install -y \
  nodejs \
  libxml2-dev \
  libssl-dev \
  unixodbc

RUN R -e  "install.packages(c( \
  'packrat',                   \
  'tidyverse',                 \
  'highcharter',               \
  'shinydashboard',            \
  'hrbrthemes'))"

RUN apt-get install -y \
  unixodbc-dev

RUN R -e "install.packages(c( \
  'odbc',                     \
  'pool'))"

COPY ui.R /srv/shiny-server/6.004/
COPY server.R /srv/shiny-server/6.004/
#COPY general.R /srv/shiny-server/6.004/
#COPY queue_to_df.R /srv/shiny-server/6.004/
#COPY queue.js /srv/shiny-server/6.004/
COPY *queue.csv /srv/shiny-server/6.004/
COPY odbc/odbc.ini /etc/odbc.ini
COPY odbc/odbcinst.ini /etc/odbcinst.ini
COPY odbc/lib/libmyodbc5a.so /usr/lib/x86_64-linux-gnu/odbc/libmyodbc5a.so

#RUN cd /srv/shiny-server/6.004/ && \
  #node queue.js > queue-log &
#RUN cd /srv/shiny-server/6.004/ && \
  #node queue.js | Rscript queue_to_df.R &

EXPOSE 3838

