# intermediate.wheatgrass_seed.ct_4.6.19

## Project information
### 1. This project demonstrates how to use R to quantify intermediate wheatgrass seeds.  
   1. Seeds from three intermediate wheatgrass plants were imaged.
   1. Intermediate wheatgrass is an alternative perennial grain crop currently under domestication.
   1. Because not all domestications traits have been bred into the breeding germplasm some seeds do not thresh like normal wheat kernels.
   1. In the images you will see there are some clean (dark brown) and some hulled (light brown and larger) seeds.
   1. Note: the more you can spread the seeds out on the imaging plat the better segmentation you will get.
   1. These images were taken with a simple DSLR camera and are saved as JPGs in the 'original images' folder.
### 2. A metadata sheet is included in the 'results' folder.  
   1. The data sheet contains important information to validate the computer output.
   1. Variables were collected manually on clean seed count, hulled seed count, and threshability.
### 3. Several training data csv files are included in the 'training data' folder.  
   1. Collectively, these mixes (csv files) make up the training palette for this project.
   1. The training palette is used to fit random forest models.
   1. The training palette can be expanded or shrunk by adding or subtracting mixes.
   1. New mixes can be made easily in ImageJ (SEE MODULE ON MAKING TRAINING DATA).
   1. Mixes can be subtracted by removing the files after downloading or using R.


## Setup
1. Start by downloading the ZIP file with all documents by clicking the "clone or download" button.
1. Save these documents in a folder anywhere on your hard drive called 'intermediate.wheatgrass_seed.ct_4.6.19'.
1. NOTE: if you do not use this file name it will take a little more work to run the R script.
1. Open the R file in R studio.
1. Go through each required package and download or update packages if needed.
1. Update R if you have not done so recently.
1. In the R file go to line 32 and change the object file path to wherever your 'intermediate.wheatgrass_seed.ct_4.6.19' folder is located.
1. The script should run and conduct the following tasks:
   1. Create new folders within 'intermediate.wheatgrass_seed.ct_4.6.19'.
   1. Each folder contains a step in the pipeline (S1-S6).
   1. Build three random forest models.
   1. Operate image analysis using the models and EBImage functions.
   1. Output and save a summary data file.
1. This script will take about 4-6 minutes to run (13.3 min with a 2014 MacBook Pro 16G RAM macOS High Sierra).
