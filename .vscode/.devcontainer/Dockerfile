
# Use the official R image from Rocker
FROM --platform=$BUILDPLATFORM rocker/rstudio:4

# Install additional dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('languageserver', 'httpgd'), 
repos = 'https://cloud.r-project.org/')"



