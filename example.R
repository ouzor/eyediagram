# Example code for the eye diagram 
# (c) 2011-2012, Juuso Parkkinen (juuso.parkkinen@gmail.com)

# Read eye diagram preprocessing code
source("./eyediagram.R")

# Set filepaths
input.folder <- "./example/input/"
output.file <- "./example/ExampleEyeDiagram"
if (!file.exists(input.folder))
  dir.create(input.folder)

# Create toy data
toydata <- createToyData()

# Set parameters for the visualization
infotext <- get.infotext.lmr(c("Example eye diagram", "Documents", "Topics", "Words"))
font.sizes <-  c(26, 26, 30, 30, 60, 60)
radius.adjustments <-  c(-300, -300, 1.0, 0.8, 0.8, 90)
curve.factors <-  c(10, 10, 5, 20)

# Write input
writeEyeDiagramInput(toydata$doc2topic, toydata$word2topic, input.folder, output.file, infotext, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors, topic.names=colnames(toydata$doc2topic))

# To produce the final visualization you can either
# - use Processing (www.processing.org) to run the eyediagram.pde script
# - use the precompiled java script to run the script

# Here I have precompiled the script (for linux), so I can run it directly
system("./application.linux/eyediagram")


