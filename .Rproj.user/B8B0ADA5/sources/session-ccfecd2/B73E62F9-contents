# start from the rocker/r-ver:latest image
FROM rocker/r-ver:latest

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev \
  libmysqlclient-dev

# install plumber
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('mirtCAT')"
RUN R -e "install.packages('RMySQL')"
RUN R -e "install.packages('mirt')"
RUN R -e "install.packages('purrr')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('dplyr')"

# copy everything from the current directory into the container
COPY / /

# open port 1994 to traffic
EXPOSE 1994

# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "main.R"]