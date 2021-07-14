# Under-diagnosis of diabetes and hypertension

[![DOI](https://zenodo.org/badge/378867751.svg)](https://zenodo.org/badge/latestdoi/378867751)

Under-diagnosis of diabetes and hypertension in South Africa. Analysis of the South African DHS 2016 data.

## Bibliometric information


## Abstract
**Background.** Hypertension and diabetes mellitus are major causes of natural death in South Africa. Under-diagnosis of these treatable diseases would hamper efforts to improve management and hence reduce morbidity and mortality for these conditions.

**Objective.** To assess the level of under-diagnosis of hypertension and diabetes in South Africa.

**Methods.** I used data from adult participants (≥15 years old) of the 2016 edition of the South African Demographics and Health Survey, a survey that draws a nationally representative sample of the population. Using these data, I assessed the prevalences of hypertension and diabetes mellitus at the time of the study using objective criteria (hypertension: systolic blood pressure ≥140 mmHg and/or diastolic blood pressure ≥90 mmHg; diabetes: HbA1c ≥6.5%), and disease prevalences based on participant recall of having ever received a diagnosis of hypertension or diabetes from a healthcare professional. I calculated the difference between the objectively measured prevalences of the diseases and the prevalences based on recall to get an estimate of the level of under-diagnosis of the diseases. 

**Results.** 10 336 adult participants answered the question on the recall of a diagnosis, 8 092 had their blood pressure measured, and 6 740 had HbA1c assessed. The prevalence of hypertension based on recall was 18.9% (95% CI: 17.7 to 20.1%), while the prevalence using blood pressure measurements was 37.1% (95% CI: 35.3 to 38.9%). Thus, 49% of cases of hypertension are undiagnosed (~7.1 million people). The prevalence of diabetes based on recall was 4.5% (95% CI: 3.9 to 5.1%), and 11.4% (95% CI: 10.4 to 12.4%) based on HbA1c values. Thus, 61% of cases of diabetes are undiagnosed (~2.7 million people).

**Conclusion.** There is significant under-diagnosis of hypertension and diabetes mellitus in South Africa. The under-diagnosis of these two treatable conditions, which have high morbidity and mortality, has major population health implications.  

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

- In the _Files_ tab, double-click the **diabetes-and-hypertension.Rproj** file to ensure all the working directories are in order before running any of the scripts.

- Open the individual `R` scripts and run them. Note, the `01_clean-data.R` script will not run unless you have obtained the original data files from the DHS Program and set the file paths in the script to where the original data files have been saved to on your computer.

#### Shutting down

Once done, log out of RStudio Server and enter the following into a terminal to stop the docker container: `docker stop diabetes`. If you then want to remove the container, enter: `docker rm diabetes`. If you also want to remove the docker image you downloaded, enter: `docker rmi kamermanpr/diabetes-and-hypertension:v1.0`
