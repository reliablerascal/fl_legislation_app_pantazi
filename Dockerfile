FROM rocker/shiny-verse:latest

# Install system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DT', 'data.table', 'plotly', 'shinyWidgets', 'jsonlite', 'lubridate', 'stringr', 'foreach', 'profvis'), repos='https://cloud.r-project.org/')"

RUN mkdir -p /srv/shiny-server/dashboard
RUN mkdir -p /srv/shiny-server/heatmap

# Copy the app to the image
COPY ./dashboard_app.R /srv/shiny-server/dashboard/app.R
COPY ./heatmap_app.r /srv/shiny-server/heatmap/app.R

# Ensure data.RData is in the container
COPY ./data.RData /srv/shiny-server/dashboard/data.RData
COPY ./data.RData /srv/shiny-server/heatmap/data.RData

# Expose the port Shiny App runs on
EXPOSE 3838

CMD ["shiny-server"]
