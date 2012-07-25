source("/share/mi/exp/multirex/source/circular_component_visualization.R")

## EXAMPLE Create an artificial cell line - patient matching for Leukemia
Ntopics = 5
Ndpt = 6
Nwpt = 8
d2t = matrix(NA, Ntopics*Ndpt, Ntopics)
w2t = matrix(NA, Ntopics*Nwpt, Ntopics)
rownames(d2t) <- paste("Patient_ID",sample(100, Ntopics*Ndpt), sep="")
rownames(w2t) <- paste("CellLine_ID",sample(100, Ntopics*Nwpt), sep="")
topic.names = paste("Z",1:Ntopics,sep="")

in.mean = 0.3
in.sd = 0.2
out.mean = -0.2
out.sd = 0.2
for (i in 1:Ntopics) {
  d2t[(1:Ndpt)+(i-1)*Ndpt,i] = rnorm(Ndpt, in.mean, in.sd)
  w2t[(1:Nwpt)+(i-1)*Nwpt,i] = rnorm(Nwpt, in.mean, in.sd)
}

d2t[which(is.na(d2t))] = rnorm(Ndpt*Ntopics*(Ntopics-1), out.mean, out.sd)
d2t[d2t<0] = 0
w2t[which(is.na(w2t))] = rnorm(Nwpt*Ntopics*(Ntopics-1), out.mean, out.sd)
w2t[w2t<0] = 0

output.file <- "/share/mi/exp/multirex/results/examples/example_patient_leukemia2"
infotext <- get.infotext.lmr(c("Mapping mouse and human tumors", "Mouse tumor", "Shared steady state components", "Human tumor"))
font.sizes = c(26, 26, 30, 30, 60, 60)
radius.adjustments = c(-300, -300, 1.0, 0.8, 0.8, 90)
curve.factors = c(10, 10, 5, 20)

## Write output
writeCircCompVizInput(d2t, w2t, output.file, infotext, font.sizes=font.sizes, radius.adjustments=radius.adjustments, curve.factors=curve.factors, topic.names=topic.names)
## Run CircCompViz shell script to draw the pdf
system("/share/mi/exp/multirex/source/CircCompViz/CircCompViz")



## ## Write output for a new patient
## new.d2t <- matrix(0, 3, Ntopics)
## new.d2t[2,] <- rnorm(Ntopics, 0.4, 0.4)
## new.d2t[new.d2t<0] <- 0
## rownames(new.d2t) <- c("", "Patient_ID132", "")
## output.file <- "example_leukemia_new_patient2"
## font.sizes = c(40, 26, 30, 30, 60, 60)
## curve.adjustments = c(-400, -300, 1.0, 0.8, 0.8)

## writeCircCompVizInput(new.d2t, d2t, output.folder, output.file, infotext, font.sizes=font.sizes,
##                       curve.adjustments=curve.adjustments, topic.names=topic.names)
## system("/share/mi/exp/multirex/source/CircCompViz/CircCompViz")


## ## Create an artificial mouse-human decomposition visualization (March 2011)
## Ntopics = 10
## Ndpt = 4
## Nwpt = 5
## d2t = matrix(NA, Ntopics*Ndpt, Ntopics)
## w2t = matrix(NA, Ntopics*Nwpt, Ntopics)
## rownames(d2t) <- paste("Biological_Process_M",1:(Ntopics*Ndpt), sep="")
## rownames(w2t) <- paste("Biological_Process_H",1:(Ntopics*Nwpt), sep="")
## topic.names = paste("Z",1:Ntopics,sep="")

## in.mean = 0.3
## in.sd = 0.15
## out.mean = -0.1
## out.sd = 0.1
## for (i in 1:Ntopics) {
##   d2t[(1:Ndpt)+(i-1)*Ndpt,i] = rnorm(Ndpt, in.mean, in.sd)
##   w2t[(1:Nwpt)+(i-1)*Nwpt,i] = rnorm(Nwpt, in.mean, in.sd)
## }

## d2t[which(is.na(d2t))] = rnorm(Ndpt*Ntopics*(Ntopics-1), out.mean, out.sd)
## d2t[d2t<0] = 0
## w2t[which(is.na(w2t))] = rnorm(Nwpt*Ntopics*(Ntopics-1), out.mean, out.sd)
## w2t[w2t<0] = 0

## output.folder <- "misc"
## output.file <- "example_mouse_human_temp"
## infotext <- get.infotext.lmr(c("Mapping mouse and human tumors", "Mouse tumor", "Shared steady state components", "Human tumor"))
## font.sizes = c(24, 22, 26, 30, 50, 50)
## curve.adjustments = c(-300, -200, 1.0, 0.8, 0.8)

## ## Run visualization
## WriteCircCompVizInput(d2t, w2t, output.folder, output.file, infotext, font.sizes=font.sizes,
##                       curve.adjustments=curve.adjustments, topic.names=topic.names)
## ## system("/share/mi/exp/multirex/source/CircCompViz/CircCompViz")
