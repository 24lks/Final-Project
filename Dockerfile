# Start from the official plumber image
FROM rstudio/plumber

# Install Linux libraries
RUN apt-get update -qq && \
    apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev pandoc && \
    apt-get clean

# Install R packages
RUN R -e "install.packages(c('tidyverse','tidymodels','yardstick','ggplot2','GGally','leaflet','plumber'))"

# Set working directory
WORKDIR /home/plumber

# Copy files into the container
COPY plumber.R /home/plumber/
COPY diabetes_binary_5050split_health_indicators_BRFSS2015.csv /home/plumber/
COPY rf_final_model.rds /home/plumber/

# Open port
EXPOSE 8000

# Run Plumber API
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]

