#-- Build command --#
## cd to where the docker file is located.
# docker build -t kamermanpr/diabetes-and-hypertension:v1.0 .

#-- Push build to DockerHub --#
# docker push

#-- The build image can be downloaded from Docker Hub --#
# docker pull kamermanpr/diabetes-and-hypertension:v1.0

#-- Run command --#
# docker run --name diabetes -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/diabetes-and-hypertension:v1.0

#-- Login to RStudio --#
# In your browser, navigate to: localhost:8787
# Username: user
# Password: password

#-- Get the verse rocker image --#

FROM rocker/tidyverse:4.0.4

MAINTAINER Peter Kamerman <peter.kamerman@gmail.com>

#-- Install extra packages --#

RUN Rscript -e "install.packages(c('survey', 'ggthemes', 'patchwork', 'foreign'), dependencies = TRUE)"
