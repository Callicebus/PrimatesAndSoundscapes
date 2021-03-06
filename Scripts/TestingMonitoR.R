#Automated Detection Algorithm - Template Matching using Monitor's Spectrogram Cross Correlation
#Authors: Silvy van Kuijk & Sun O'Brien


#Link to MonitoR guide: https://cran.r-project.org/web/packages/monitoR/vignettes/monitoR_QuickStart.pdf
#Link to MonitoR full package description: https://cran.r-project.org/web/packages/monitoR/monitoR.pdf

#Link to folder with audio files to create a template: https://utexas.box.com/s/94z5usiudavxiyvumtko2bvzkjdo63h6
#Link to folder with audio files to test a template on: https://utexas.box.com/s/l07hjh5cycqyps3dfrwvkyz9auxo6c7k


#-----------

#If you need to install packages, use this: (You'll need to install a lot in the initial stages, most likely)

install = FALSE

if(install) {
  install.packages("monitoR")
  install.packages("tuneR")
  install.packages("foreach")
}

#Other potentially useful/interesting acoustic packages are 'WarbleR', seewave' and 'dynaSpec'.

#Before you're able to run the code that's in a certain package, you need to call the package into R's memory:
library(monitoR)
library(tuneR)
library(foreach)

#Some first steps to take:
#1. Try and load some of the .wav files from the last link mentioned above into memory (maybe even see if you can create a spectrogram?)
#2. Follow the guide to see if you can create a basic detection template with the Spectrogram Cross Correlation method.
#3. Test your template on an audio file that is known to have titi monkey duets. 

# data(survey)
# survey
# viewSpec(survey)

# fileList <- list.files(path = "AutomatedDetection_CreatingTemplates", full.names = TRUE)
# 
# waveList <- list()
# templateList <- list()
# 
# for(value in fileList) {
#   sunwave <- readWave(value)
#   append(waveList, sunwave)
#   template <- makeCorTemplate(value, frq.lim = c(0.2, 1.8), t.lim = c(0, 10))
#   template
#   templateList <- c(templateList, template)
#   #(templateList)
# }
# 
# print(templateList)
# print(templateList[])
# 
# 
# ctemps <- combineCorTemplates(templateList[])



# now working on binary template matching instead of cross-correlation

fileList <- list.files(path = "AutomatedDetection_CreatingTemplates", full.names = TRUE)

sunwave <- readWave(fileList[1])
template1b <- makeBinTemplate(fileList[1], frq.lim = c(0.3, 1.6), t.lim = c(25, 35), name = "F1,025,-21,T", amp.cutoff = (-21))

sunwave <- readWave(fileList[2])
template2b1 <- makeBinTemplate(fileList[2], frq.lim = c(0.2, 1.8), t.lim = c(0, 10), name = "F2,000,-21,T", amp.cutoff = (-21))
template2b2 <- makeBinTemplate(fileList[2], frq.lim = c(0.2, 1.8), t.lim = c(87, 97), name = "F2,087,-21,T", amp.cutoff = (-21)) 

sunwave <- readWave(fileList[3])
template3b <- makeBinTemplate(fileList[3], frq.lim = c(0.2, 1.8), t.lim = c(0, 10), name = "F3,000,-20,T", amp.cutoff = (-20))

sunwave <- readWave(fileList[4])
template4b1 <- makeBinTemplate(fileList[4], frq.lim = c(0.2, 1.4), t.lim = c(0, 10), name = "F4,000,-25,T", amp.cutoff = (-25))
template4b2 <- makeBinTemplate(fileList[4], frq.lim = c(0.2, 1.8), t.lim = c(30, 40), name = "F4,030,-24,T", amp.cutoff = (-24)) 

sunwave <- readWave(fileList[5])
template5b <- makeBinTemplate(fileList[5], frq.lim = c(0.2, 1.4), t.lim = c(65, 75), name = "F5,065,-21,T", amp.cutoff = (-21))

sunwave <- readWave(fileList[8])
template8b1 <- makeBinTemplate(fileList[8], frq.lim = c(0.3, 1.5), t.lim = c(25, 35), name = "F8,025,-24,T", amp.cutoff = (-23)) 
template8b2 <- makeBinTemplate(fileList[8], frq.lim = c(0.3, 1.5), t.lim = c(36, 46), name = "F8,036,-24,T", amp.cutoff = (-22)) 

sunwave <- readWave(fileList[6])
template6b <- makeBinTemplate(fileList[6], frq.lim = c(0.2, 1.0), t.lim = c(15, 25), name = "F6,015,-32,H") 

sunwave <- readWave(fileList[7])
template7b <- makeBinTemplate(fileList[7], frq.lim = c(0.2, 1.4), t.lim = c(17, 27), name = "F7,017,-26,H") 

sunwave <- readWave("subset_samples/manual_recording/256.wav")
templateManual <- makeBinTemplate("subset_samples/manual_recording/256.wav", frq.lim = c(0.2, 1.8), t.lim = c(102, 112), name = "manual", amp.cutoff = (-27))

ctemps_adjusted_amp <- combineBinTemplates(template1b, template2b1, template2b2, template3b, template4b1, template4b2, template5b, template8b1, template8b2, template6b, template7b, templateManual)



# runs against all files in the 4,200 folder, from 5:45 am to 8:00 am.
  # (we manually logged detections for only those times)

folderList <- list.files(path = "AutomatedDetection_TestingTemplates/4,200", full.names = TRUE)

folderCounter = 1
maxFilesPerFolder = 10   # we need to stop at 8:00 am inclusive
sink(file = "output.txt")
for(folder in folderList) {
  fileCounter = 1
  audioFileList <- list.files(path = folderList[folderCounter], full.names = TRUE)
  print(paste("folder", folderCounter))
  while(fileCounter <= maxFilesPerFolder) {
    print(paste("    file", fileCounter))
    scores <- binMatch(audioFileList[fileCounter], ctemps_adjusted_amp, quiet = TRUE)
    print(scores) 
    fileCounter = fileCounter + 1
  }
  folderCounter = folderCounter + 1
}
sink()







fileList <- list.files(path = "subset_samples/first_howler_neg_template_test", full.names = TRUE)
for(value in fileList){
  cscores <- binMatch(value, ctemps_adjusted_amp)
  print(cscores)
  # code for showing peaks on plot. will cause loop to not run fully, as plot(cdetects) is interactive.
  # cdetects <- findPeaks(cscores)
  # plot(cdetects)
  # showPeaks(detection.obj = cdetects, which.one = "F1,025,-21,T", point = TRUE, what = "peaks", scorelim = c(5,10))
  # print(cscores)
}




# HARPIA_20130216_054500    -- NEGATIVE CASE
# HARPIA_20130220_061500  -- NEGATIVE CASE
# HARPIA_20130216_060000    -- POSITIVE CASE - TITI MONKEY
# HARPIA_20140213_060000  -- FALSE POSITIVE CASE - HOWLER MONKEY

#test manually adjusted templates
cscores <- binMatch("subset_samples/basic_test/HARPIA_20130216_054500.wav", ctemps_adjusted_amp) # neg 1
print(cscores)

cscores <- binMatch("subset_samples/basic_test/HARPIA_20130220_061500.wav", ctemps_adjusted_amp) # neg 2
print(cscores)

cscores <- binMatch("subset_samples/basic_test/HARPIA_20130216_060000.wav", ctemps_adjusted_amp) # pos 1
print(cscores)

cscores <- binMatch("subset_samples/basic_test/HARPIA_20140213_060000.wav", ctemps_adjusted_amp) # false pos 1
print(cscores)




# call distance four titi monkeys
fileList <- list.files(path = "subset_samples/call_distance_four", full.names = TRUE)
for(value in fileList) {
  cscores <- corMatch(value, ctemps)
  print(cscores)
}





# medium-sized sample -- 15 files. 14 have no detections and 1 file has titi
# monkeys (positive case file is HARPIA_20130216_060000, will run second of 15)
# line 83 of data google sheet
fileList <- list.files(path = "subset_samples/medium_sample", full.names = TRUE)
for(value in fileList){
  cscores <- binMatch(value, ctemps)
  print(cscores)
}





# howler monkey samples against titi monkey template
fileList <- list.files(path = "subset_samples/howler_sample", full.names = TRUE)
for(value in fileList) {
  cscores <- binMatch(value, ctemps)
  print(cscores)
}





# stereo/mono channel experiment. one audio sample -- only right channel is 
# functioning for these recordings (damage to left mic)
fileList <- list.files(path = "subset_samples/mic_2_sample", full.names = TRUE)
for(value in fileList) {
  stereo_channel_wave <- readWave(value)
  stereo_channel_wave
  mono_channel_wave <- mono(isolated_channel_wave, "right")
  mono_channel_wave
  cscores1 <- corMatch(stereo_channel_wave, ctemps, write.wav=TRUE)
  print(cscores1)
  cscores2 <- corMatch(mono_channel_wave, ctemps, write.wav=TRUE)
  print(cscores2)
}




# experiments to think about
  # using findPeaks function on cscores -- are we in the middle of the pipeline
  # does a bin/cor matching use both channels or just one channel
  # how many templates should we have in our set
  # howler monkeys -- can we use minScore to consistently mark them negative?

