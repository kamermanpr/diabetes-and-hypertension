# Under-diagnosis of diabetes and hypertension

Under-diagnosis of diabetes and hypertension in South Africa. Analysis of the South African DHS 2016 data.

## Bibliometric information


## Abstract


## Reproducibility

For reproducibility we have built a docker image with the environment used to run the scripts:  
[kamermanpr/diabetes-and-hypertension](https://hub.docker.com/repository/docker/kamermanpr/diabetes-and-hypertension)

### Using Docker to run the scripts

You need to have docker installed on your computer. To do so, go to [docker.com](https://www.docker.com/community-edition#/download) and follow the instructions for installing Docker for your operating system. Once Docker has been installed, follow the steps below, noting that Docker commands are entered in a terminal window (Linux and OSX/macOS) or command prompt window (Windows). 

#### Download the latest image

Enter: `docker pull kamermanpr/diabetes-and-hypertension:v1.0`

#### Run the container

Enter: `docker run --name diabetes -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/diabetes-and-hypertension:v1.0`

#### Login to RStudio Server

- Open a web browser window and navigate to: `localhost:8787`

- Use the following login credentials: 
    - Username: _user_	
    - Password: _password_
    
#### Upload repository

- Go to the [diabetes-and-hypertension](https://github.com/kamermanpr/diabetes-and-hypertension.git) repository on GitHub and select _Code_ and then _Download ZIP_.

- In the _Files_ tab on the lower right panel of RStudio, click **Upload**, located the zip file you downloaded and the click **OK**. The zip file will be uploaded and will automatically unzip, giving you access to all the content, including the analysis scripts, for the project.

- In the _Files_ tab, double-click the **alpaca-data.Rproj** file to ensure all the working directories are in order before running any of the scripts.

- Open the individual `R` scripts and run them. Note, the `01_clean-data.R` script will not run unless you have obtained the original data files from the DHS Program and set the file paths in the script to where the original data files have been saved to on your computer.

#### Shutting down

Once done, log out of RStudio Server and enter the following into a terminal to stop the docker container: `docker stop diabetes`. If you then want to remove the container, enter: `docker rm diabetes`. If you also want to remove the docker image you downloaded, enter: `docker rmi kamermanpr/diabetes-and-hypertension:v1.0`
