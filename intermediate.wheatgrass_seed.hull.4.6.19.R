## Demo script for intermediate wheatgrass seed and hull counting
## This script will demonstrate:
#1) seperating foreground (seed and hull) from background
#2) count seeds
#3) count hulls
#4) measure proportion of seed to hull
## Date: 04.06.19
## Author: Garett Heineck

## Required packages - may need installation first
## PLEASE UPDATE R (if you have not done so recently)
library(tidyverse)
library(readxl)
library(dplyr)
library(jpeg)
library(EBImage) #needs to be downloaded from (https://www.bioconductor.org/packages/release/bioc/html/EBImage.html)***
library(randomForest)
library(stringr)
library(ggplot2)
library(cowplot)
##############
##############
##############


##############
##############
##############
## Required file paths
#parent directory folder named "intermediate.wheatgrass_seed.ct_4.6.19"***
#To run this you need a folder named "intermediate.wheatgrass_seed.ct_4.6.19"***
img_IWG_seed.demo.4.6.19<- "/Users/heine237/Documents/GitHub/intermediate.wheatgrass_seed.ct_4.6.19" #NOTE: change to your own file path***
#************************#
## Creating folders to store all image output
#NOTE: the folder "original_img" is where the images you want to process need to be***
folders <- c("training data_4.6.19", 
             "results", 
             "original_img", 
             "crop_img", 
             "S1_foreground_classify", 
             "S2_foreground_EBImage", 
             "S3_hull_classify",  
             "S4_hull_EBImage", 
             "S5_seed_classify",  
             "S6_seed_EBImage") #adding in the correct folder list*** 
for (i in 1:length(folders))  { 
  dir.create(paste(img_IWG_seed.demo.4.6.19,folders[i], sep="/")) 
}
#NOTE: you may see Warning messages, that is ok***
#make sure the "original_img" folder has images in it***
##############
##############
##############


##############
##############
##############
## Read in field data sheet.
## The field data contains plant information about each image captured in the originalimages folder.
## You can open the excel spreadsheet to read the column descriptions
## Data on manual seed and hull counts is available in this sheet.
## We can compare these manual data to our computer generated ratings.
#************************#
IWG_seed.dat<-  read_excel(paste(img_IWG_seed.demo.4.6.19, "results", "IWG_seed.ct_data.xlsx", sep = "/"), skip = 6, na = ".")
summary(IWG_seed.dat)
##############
##############
##############


##############
##############
##############
## This step crops the original images.
## Cropping your images is important to reduce processing time. 
## This happens to not be importat for this demo so we set dimensions to 0
short<- 0.00 #proportion taken off the short side of an image***
long<-  0.00 #proportion taken off the long side of an image***
#************************#
original_img.path<- list.files(path=paste(img_IWG_seed.demo.4.6.19, "original_img",sep = "/"), full.names = T)
original_img.name<- list.files(path=paste(img_IWG_seed.demo.4.6.19, "original_img",sep = "/"), full.names = F)
folder_crop_img<-   (paste(img_IWG_seed.demo.4.6.19,"crop_img",sep = "/"))
#************************#
for(i in 1:length(original_img.path)){
    temp1<- readImage(original_img.path[i])
    if(dim(temp1)[1]>dim(temp1)[2]){
      short1<- round(min(c(dim(temp1)[1],dim(temp1)[2])) * short)
      short2<- min(c(dim(temp1)[1],dim(temp1)[2])) - round(min(c(dim(temp1)[1],dim(temp1)[2])) * short) 
      long1<- round(max(c(dim(temp1)[1],dim(temp1)[2])) * long)
      long2<- round(max(c(dim(temp1)[1],dim(temp1)[2])) - (max(c(dim(temp1)[1],dim(temp1)[2])) * long))
      temp3<-temp1[long1:long2,short1:short2,]
    } else {
      temp2<- rotate(temp1, 90)
      short1<- round(min(c(dim(temp2)[1],dim(temp2)[2])) * short)
      short2<- min(c(dim(temp2)[1],dim(temp2)[2])) - round(min(c(dim(temp2)[1],dim(temp2)[2])) * short) 
      long1<- round(max(c(dim(temp2)[1],dim(temp2)[2])) * long)
      long2<- round(max(c(dim(temp2)[1],dim(temp2)[2])) - (max(c(dim(temp2)[1],dim(temp2)[2])) * long))
      temp3<-temp2[long1:long2,short1:short2,]
    }
    img.code<- IWG_seed.dat$image_num[i]
    img.type<- IWG_seed.dat$image_typ[i]
    writeImage(temp3, paste(folder_crop_img, "/", img.code, "_", img.type, "_", original_img.name[i], sep = ""), quality = 85)
}
##############
##############
##############


##############
##############
##############
## We now need to load the training data.
## Information on how to create training data can be found in the TRAINING DATA HELP GUIDE.
## Collectively the training mixes are called a palette in the training palette folder.
## The palette has many mixes, each help in predicting different features within the image.
#************************#
palette_directory_IWG.seed<- paste(img_IWG_seed.demo.4.6.19, "training data_4.6.19",sep = "/") #file path where mixes are saved***
#************************#
mixes_names<- list.files(path=palette_directory_IWG.seed,pattern="*.csv",full.names = FALSE) #name directory for what is in the palette folder***
mixes_path<- list.files(path=palette_directory_IWG.seed, pattern="*.csv", full.names = TRUE) #path directory for what is in the palette folder***
training.palette_IWG.seed<- data.frame()
#this for() loop will systematically re arrange and condense each mix file in the training palette folder***
#the reason I am doing this is to allow the script to update itself upon adding additional mixes***
for (i in 1:length(mixes_path)){
  temp_mix<- read.csv(mixes_path[i])
  temp_mix$band<- NA
  temp_mix$band[1:which(temp_mix$Label == "Red")] <- "Red"
  temp_mix$band[(which(temp_mix$Label == "Red")+1):which(temp_mix$Label == "Green")] <- "Green"
  temp_mix$band[(which(temp_mix$Label == "Green")+1):which(temp_mix$Label == "Blue")] <- "Blue"
  temp<- split(temp_mix, temp_mix$band)
  temp2<- do.call("cbind", split(temp_mix, temp_mix$band))
  image<- temp2$Blue.Label[1]
  mix<- mixes_names[i]
  temp3<- data.frame(mix, image, x=temp2[5]$Blue.X, y=temp2[6]$Blue.Y, red=temp2[18]$Red.Mean, green=temp2[11]$Green.Mean, blue=temp2[4]$Blue.Mean)
  training.palette_IWG.seed<- rbind(training.palette_IWG.seed, temp3) 
}
summary(training.palette_IWG.seed) #summarizing the training palette***
count(training.palette_IWG.seed, mix) %>% View #counting observations in each mix of the training palette*** 
##############
##############
##############


##############
##############
##############
## We will now make the random forest models to detect different features in the cropped images.
## A different random forest model will be needed for each feature.
## Here were are detecting three features: 
# 1) the foreground (seeds an hulls) from the background
# 2) clean seeds 
# 3) hulled seeds (they look lighter in color in the provided image)
#************************#
#model to seperate foreground (seeds and hull related pixels)***
palette_selection_seed.all<- training.palette_IWG.seed
palette_selection_seed.all$classification<- c(rep(1, len=300),rep(0, len=200),rep(1, len=400)) #selecting the mixes (1=foreground)***
palette_selection_seed.all %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_seed.all_IWG.seed.demo_4.6.19<- randomForest(classification~(red+green+blue),data=palette_selection_seed.all, ntree=60,mtry = 1,importance=TRUE)
print(rfm_seed.all_IWG.seed.demo_4.6.19)
plot(rfm_seed.all_IWG.seed.demo_4.6.19) #ntree is set to 60, that looks about right***
importance(rfm_seed.all_IWG.seed.demo_4.6.19) 
#************************#
#model for clean seed***
palette_selection_seed.clean<- filter(training.palette_IWG.seed, !mix %in% c("IWG_hull.dark.01.csv"))
palette_selection_seed.clean$classification<- c(rep(0, len=200),rep(0, len=200),rep(1, len=400)) 
palette_selection_seed.clean %>% group_by(mix) %>% summarise(avg=mean(classification))  
rfm_seed.clean_IWG.seed.demo_4.6.19<- randomForest(classification~(red+green+blue),data=palette_selection_seed.clean, ntree=70,mtry = 1,importance=TRUE)
print(rfm_seed.clean_IWG.seed.demo_4.6.19)
plot(rfm_seed.clean_IWG.seed.demo_4.6.19)
#************************#
#model for hull***
palette_selection_hull<- training.palette_IWG.seed
palette_selection_hull$classification<-  c(rep(1, len=300),rep(0, len=200),rep(0, len=400)) 
palette_selection_hull %>% group_by(mix) %>% summarise(avg=mean(classification))
rfm_hull_IWG.seed.demo_4.6.19<- randomForest(classification~(red+green+blue),data=palette_selection_hull, ntree=70,mtry = 1,importance=TRUE)
print(rfm_hull_IWG.seed.demo_4.6.19)
plot(rfm_hull_IWG.seed.demo_4.6.19)
##############
##############
##############


##############
##############
##############
## Running the image processing loop.
## This is a really large loop that is broken up into 6 sections.
#************************#
#each path is for an image***
folder_cropped_IWG_seed.demo_4.6.19<-  (paste(img_IWG_seed.demo.4.6.19,"crop_img",sep = "/"))
folder_classify_IWG_seed.demo_4.6.19<- (paste(img_IWG_seed.demo.4.6.19,"S1_foreground_classify",sep = "/"))
folder_EBImage_IWG_seed.demo_4.6.19<-  (paste(img_IWG_seed.demo.4.6.19,"S2_foreground_EBImage",sep = "/"))
folder_hull_classify_IWG_seed.demo_4.6.19<-  (paste(img_IWG_seed.demo.4.6.19,"S3_hull_classify",sep = "/"))
folder_hull_EBImage_IWG_seed.demo_4.6.19<-  (paste(img_IWG_seed.demo.4.6.19,"S4_hull_EBImage",sep = "/"))
folder_seed_classify_IWG_seed.demo_4.6.19<-  (paste(img_IWG_seed.demo.4.6.19,"S5_seed_classify",sep = "/"))
folder_seed_EBImage_IWG_seed.demo_4.6.19<-  (paste(img_IWG_seed.demo.4.6.19,"S6_seed_EBImage",sep = "/"))
#************************#
#check to make sure all the cropped image show up***
paths_cropped_IWG.seed<- list.files(path=folder_cropped_IWG_seed.demo_4.6.19,full.names = TRUE)
names_cropped_IWG.seed<- list.files(path=folder_cropped_IWG_seed.demo_4.6.19,full.names = FALSE) 
#create a data frome to collect numeric output from the analysis***
img.stats_IWG.seed.demo_4.6.19<- data.frame()

for (i in 1:length(paths_cropped_IWG.seed)) {
  img.01<- readJPEG(paths_cropped_IWG.seed[i])
  coor<- as.data.frame(as.table(img.01[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.01[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.01[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.01[,,3]))[3]
  img.dat.01<- cbind(coor, red, green, blue)
  colnames(img.dat.01)<- c("y","x","red","green","blue")
  img.dat.01$classify<- predict(rfm_seed.all_IWG.seed.demo_4.6.19, img.dat.01)
  img.dat.01$thresh<- ifelse(img.dat.01$classify>0.80, img.dat.01$classify, 0)  #Set threshold to 80%, NOTE: I am retaining the non-binary probabilities***
  img.02<- matrix(img.dat.01$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
  writeJPEG(img.02, paste(folder_classify_IWG_seed.demo_4.6.19, "/", names_cropped_IWG.seed[i], sep = ""), quality = 1)
  paths_classify_IWG.seed.01<- list.files(path=folder_classify_IWG_seed.demo_4.6.19,full.names = TRUE)
  morph_op.01<- readImage(paths_classify_IWG.seed.01[i])
  overlay.01<-  readImage(paths_cropped_IWG.seed[i])
  kernal.01<- makeBrush(3, shape='disc') #kernal sizes between 3 and 9 seem to work best***
  image_dilate.01<- opening(morph_op.01, kernal.01) #here opening is used, sometimes 'erode' works better***
  replace<-which(image_dilate.01==0)
  seedpicred<-overlay.01[,,1] # we have to operate on each matrix seperately in jpeg array. 
  seedpicblue<-overlay.01[,,2]
  seedpicgreen<-overlay.01[,,3]
  seedpicred[replace]<-0 # replace all the pixel values in the cropped, orginal image with 0 at the coordinates
  seedpicblue[replace]<-0
  seedpicgreen[replace]<-0
  img.03<- array(c(seedpicred, seedpicblue, seedpicgreen), dim=dim(overlay.01))
  writeJPEG(img.03, paste(folder_EBImage_IWG_seed.demo_4.6.19, "/", names_cropped_IWG.seed[i] ,sep=""), quality = 100)
  IWG.seed.featr<- sum(overlay.01)
  
  paths_EBImage_IWG.seed<-list.files(path=folder_EBImage_IWG_seed.demo_4.6.19,full.names = TRUE) #starting hull prediciton***
  img.04<- readJPEG(paths_EBImage_IWG.seed[i])
  coor<- as.data.frame(as.table(img.04[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.04[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.04[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.04[,,3]))[3]
  img.dat.02<- cbind(coor, red, green, blue)
  colnames(img.dat.02)<- c("y","x","red","green","blue")
  img.dat.02$order<- seq(1:length(img.dat.02$x))
  img.dat.02$exclude<- img.dat.02$red+img.dat.02$blue+img.dat.02$green
  img.dat.02_rgb<- filter(img.dat.02, exclude > 0)
  img.dat.02_black<- filter(img.dat.02, exclude == 0)
  img.dat.02_rgb$classify<- predict(rfm_hull_IWG.seed.demo_4.6.19, img.dat.02_rgb)
  img.dat.02_black$classify<- rep(0, times=length(img.dat.02_black$red))
  img_combine.hull<- rbind(img.dat.02_rgb,img.dat.02_black)
  img_combine.hull<- arrange(img_combine.hull, order)
  img_combine.hull$thresh<- ifelse(img_combine.hull$classify>0.90, 1,0) #NOTE: increasing threshold to 90% and NOT retaining variable probability***
  img.05<- matrix(img_combine.hull$thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  writeJPEG(img.05, paste(folder_hull_classify_IWG_seed.demo_4.6.19, "/", names_cropped_IWG.seed[i] ,sep=""), quality= 1)
  
  paths_classify_hull<- list.files(path=folder_hull_classify_IWG_seed.demo_4.6.19,full.names = TRUE)
  morph_op.02<- readImage(paths_classify_hull[i])
  img.06<- fillHull(morph_op.02)
  img.06<- opening(img.06, makeBrush(5, shape='disc')) #here I am using a dilating operation expanding a neighborhood of pixels with a circular structure***
  img.06 = watershed(distmap(img.06), 40) #idnetifying peaks and valleys in the greyscale image, becasue pustules are small a small radius of 1 is used***
  writeImage(colorLabels(img.06), paste(folder_hull_EBImage_IWG_seed.demo_4.6.19, "/", names_cropped_IWG.seed[i], sep=""), quality = 100)
  hull.featr<- data.frame(computeFeatures.shape(img.06, overlay.01)) %>%
    filter(s.area > 300) #filtering out anything less than 100 pixels in size***
  
  paths_EBImage_IWG.seed<-list.files(path=folder_EBImage_IWG_seed.demo_4.6.19,full.names = TRUE) #starting clean seed***
  img.07<- readJPEG(paths_EBImage_IWG.seed[i])
  coor<- as.data.frame(as.table(img.07[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.07[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.07[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.07[,,3]))[3]
  img.dat.03<- cbind(coor, red, green, blue)
  colnames(img.dat.03)<- c("y","x","red","green","blue")
  img.dat.03$classify<- predict(rfm_seed.clean_IWG.seed.demo_4.6.19, img.dat.03)
  img.dat.03$thresh<- ifelse(img.dat.03$classify>0.80, img.dat.03$classify, 0) #Set threshold to 80%***
  img.08<- matrix(img.dat.03$thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  writeJPEG(img.08, paste(folder_seed_classify_IWG_seed.demo_4.6.19, "/", names_cropped_IWG.seed[i] ,sep=""), quality= 1)
  
  paths_classify.seed<- list.files(path=folder_seed_classify_IWG_seed.demo_4.6.19,full.names = TRUE)
  morph_op.03<- readImage(paths_classify.seed[i])
  f = makeBrush(3, shape='disc', step=FALSE)
  f = f/sum(f)
  img.09<- filter2(morph_op.03, f, boundary = "replicate")
  img.09<- dilate(img.09, makeBrush(3, shape='diamond'))
  img.09<- thresh(img.09, w=12,h=12, offset = .01)
  img.09<- erode(img.09, makeBrush(3, shape='diamond'))
  display(img.09)
  img.09<- watershed(distmap(img.09), 3) 
  writeImage(colorLabels(img.09), paste(folder_seed_EBImage_IWG_seed.demo_4.6.19, "/", names_cropped_IWG.seed[i], sep=""), quality = 100)
  seed.featr<- data.frame(computeFeatures.shape(img.09, overlay.01)) %>%
    filter(s.area > 200) #filtering out anything less than 100 pixels in size***
  
  write.stats<- data.frame(img.ID=              str_sub(names_cropped_IWG.seed[i]), #unique image ID***
                           comp.sum.img=        length(img.dat.01$thresh), #total pixels***
                           comp.sum.foreground= IWG.seed.featr,
                           comp.sum.hull=       sum(hull.featr$s.area),
                           comp.count.hull=     length(hull.featr$s.area),
                           comp.sum.seed=       sum(seed.featr$s.area),
                           comp.count.seed=     length(seed.featr$s.area),
                           comp.prop.seed.clean=(length(seed.featr$s.area)/(length(seed.featr$s.area)+length(hull.featr$s.area))))
  
  img.stats_IWG.seed.demo_4.6.19<-rbind(img.stats_IWG.seed.demo_4.6.19, write.stats) 
}
#writing the output statistics to the parent directory folder***
write.csv(img.stats_IWG.seed.demo_4.6.19, paste(img_IWG_seed.demo.4.6.19, "results","img.stats_IWG.seed.demo_4.6.19.csv", sep = "/"))
##############
##############
##############
#302 seeds in image 2