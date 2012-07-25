# Code for processing input data for the eye diagram visualization
# LICENSE: FreeBSD License
# (c) 2011-2012, Juuso Parkkinen (juuso.parkkinen@gmail.com). All rights reserved.


# Main function for writing input for the Eye diagram -script
writeEyeDiagramInput <- function(d2t, w2t, input.folder, output.file, 
                                  infotext = "", hl.topic = -1,
                                  font.sizes = c(24, 22, 26, 30, 50, 50),
                                  radius.adjustments = c(-210, 300, 1.0, 0.8, 0.8, 90),
                                  curve.factors = c(10, 10, 5, 20),
                                  doc.names=NULL, topic.names=NULL, word.names=NULL, 
                                  doc.colors=NA, word.colors=NA) {
  ## d2t: documents x topics matrix of weights (with rownames!)
  ## w2t: words x topics matrix of weights (with rownames!)
  ## output.file: complete path for the output file (without .pdf)
  ## infotext: text to be written on the top left corner of the visualization
  ## hl.topic: highlighted topic index (default = -1 => no highlights)
  ## font.size: docFontSize, wordFontSize, topicFontSize, infoFontSize, topicWidth, topicHeight
  ## radius.adjustments: rAdjustmentDocs, rAdjustmentWords, topicSpreadFactor, docCurveFactor, wordCurveFactor, eyeWidth
  ## curve.factors: d2tCurveWidthFactor, w2tCurveWidthFactor, d2tCurveAlphaFactor, w2tCurveAlphaFactor
  ## doc/topic/word.names: optional, taken otherwise from weight matrices
  ## doc.colors, word.colors: can give vector of /docword colors (currently: "black", "red", "green", "blue")
  ## Note! Change any parameters to see how they affect the visualizations

  # Some checks
  if (any(d2t<0) | any(w2t<0))
    stop("Can't have negative weights in d2t or w2t!\n")
  if (radius.adjustments[6] < 0 | radius.adjustments[6] > 90)
    stop("Can't have eyeWidth > 90 or < 0")
  
#  input.folder <- "/share/mi/exp/multirex/source/CircCompViz/input/"
  if (hl.topic != -1) # Fix for Java indexing
    hl.topic <- hl.topic -1
  parameters <- c(output.file, dim(d2t)[2], hl.topic)
  write.table(parameters, file=paste(input.folder, "parameters.txt", sep=""), row.names=F, col.names=F, quote=F)
  write.table(infotext, file=paste(input.folder, "infotext.txt", sep=""), row.names=F, col.names=F, quote=F)
  
  if (!is.null(doc.names))
    write.table(doc.names, file=paste(input.folder, "doc_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  else
    write.table(rownames(d2t), file=paste(input.folder, "doc_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  if (!is.null(word.names))
    write.table(word.names, file=paste(input.folder, "word_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  else 
    write.table(rownames(w2t), file=paste(input.folder, "word_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  
  write.table(1:dim(d2t)[2], file=paste(input.folder,"topics.txt", sep=""), row.names=F, col.names=F, quote=F)
  if (!is.null(topic.names))
    write.table(topic.names, file=paste(input.folder, "topic_names.txt",sep=""), row.names=F, col.names=F, quote=F)
  else
    write.table(1:dim(d2t)[2], file=paste(input.folder, "topic_names.txt",sep=""), row.names=F, col.names=F, quote=F)
  write.table(d2t, file=paste(input.folder, "doc2topic.txt",sep=""), row.names=F, col.names=F, sep="\t")
  write.table(w2t, file=paste(input.folder, "word2topic.txt",sep=""), row.names=F, col.names=F, sep="\t")

  write.table(font.sizes, file=paste(input.folder, "font_sizes.txt", sep=""), row.names=F, col.names=F, quote=F)
  write.table(radius.adjustments, file=paste(input.folder, "radius_adjustments.txt", sep=""), row.names=F, col.names=F, quote=F)
  write.table(curve.factors, file=paste(input.folder, "curve_factors.txt", sep=""), row.names=F, col.names=F, quote=F)

  if (length(doc.colors)==1) {
    if (is.na(doc.colors))
      doc.colors <- rep("black", nrow(d2t))
    else 
      doc.colors <- rep(doc.colors, nrow(d2t))
  }
  write.table(doc.colors, file=paste(input.folder, "doc_colors.txt", sep=""), row.names=F, col.names=F, quote=F)

  if (length(word.colors)==1) {
    if (is.na(word.colors))
      word.colors <- rep("black", nrow(w2t))
    else 
      word.colors <- rep(word.colors, nrow(w2t))
  }
  write.table(word.colors, file=paste(input.folder, "word_colors.txt", sep=""), row.names=F, col.names=F, quote=F)
}

createToyData <- function(Ntopics=5, NdocsPerTopic=6, NwordsPerTopic=8,
                          in.mean=0.3, in.sd=0.2, out.mean=-0.2, out.sd=0.2) {
  
  # Initialize matrices
  doc2topic <- matrix(NA, Ntopics*NdocsPerTopic, Ntopics)
  word2topic <- matrix(NA, Ntopics*NwordsPerTopic, Ntopics)
  rownames(doc2topic) <- paste("Document", 1:(Ntopics*NdocsPerTopic), sep="_")
  rownames(word2topic) <- paste("Word", 1:(Ntopics*NwordsPerTopic), sep="_")
  colnames(doc2topic) <- paste("Z",1:Ntopics,sep="")
  
  # Sample random weights
  # Higher weights within topics
  for (i in 1:Ntopics) {
    doc2topic[(1:NdocsPerTopic)+(i-1)*NdocsPerTopic,i] <-  rnorm(NdocsPerTopic, in.mean, in.sd)
    word2topic[(1:NwordsPerTopic)+(i-1)*NwordsPerTopic,i] <-  rnorm(NwordsPerTopic, in.mean, in.sd)
  }
  
  # Lower weights elsewhere
  doc2topic[which(is.na(doc2topic))] <-  rnorm(NdocsPerTopic*Ntopics*(Ntopics-1), out.mean, out.sd)
  word2topic[which(is.na(word2topic))] <-  rnorm(NwordsPerTopic*Ntopics*(Ntopics-1), out.mean, out.sd)
  
  # Set negative weights to zero
  doc2topic[doc2topic < 0] <-  0
  word2topic[word2topic < 0] <-  0
  return(list(doc2topic=doc2topic, word2topic=word2topic))
}


################################################
## AUXILIARY CIRCULAR VISUALIZATION FUNCTIONS ##
################################################
totake.topics <- function(topics, doc2topic, word2topic, Ndocs, Nwords) {
  
  totake <- list()
  totake$topics <- topics
  DpT <- ceiling(Ndocs/length(totake$topics))
  WpT <- ceiling(Nwords/length(totake$topics))
  totake$docs <- c(); totake$words <- c()
  for (t in totake$topics) {
    totake$docs <- unique(c(totake$docs, order(doc2topic[,t], decreasing=T)[1:DpT]))
    totake$words <- unique(c(totake$words, order(word2topic[,t], decreasing=T)[1:WpT]))
  }
  return(totake)
}
  
totake.topics.prob <- function(topics, doc2topic, word2topic, doc.p.th, Nwords) {
  
  totake <- list()
  totake$topics <- topics
  topic2doc <- t(prop.table(doc2topic, 2))
  WpT <- ceiling(Nwords/length(totake$topics))
  totake$docs <- c(); totake$words <- c()
  for (t in totake$topics) {
    totake$docs <- unique(c(totake$docs, order(topic2doc[t,], decreasing=T)[1:length(which(topic2doc[t,] > doc.p.th))]))

    totake$words <- unique(c(totake$words, order(word2topic[,t], decreasing=T)[1:WpT]))
  }
  return(totake)
}

totake.docs <- function(documents, doc2topic, word2topic, Ntopics, Nwords) {
  
  totake <- list()
  totake$docs <- documents
  ##  totake$topics <- order(apply(doc2topic[totake$docs,], 2, sum), decreasing=T)[1:Ntopics]
  TpD <- ceiling(Ntopics/length(documents))
  totake$topics <- c()
  for (d in documents)
    totake$topics <- unique(c(totake$topics, order(doc2topic[d,], decreasing=T)[1:TpD]))
  
  WpT <- ceiling(Nwords/length(totake$topics))
  totake$words <- c()
  for (t in totake$topics)
    totake$words <- unique(c(totake$words, order(word2topic[,t], decreasing=T)[1:WpT]))
  return(totake)
}

totake.docs.topics <- function(documents, topics, word2topic, Nwords) {
  
  totake <- list()
  totake$docs <- documents
  totake$topics <- topics
  WpT <- ceiling(Nwords/length(totake$topics))
  totake$words <- c()
  for (t in totake$topics)
    totake$words <- unique(c(totake$words, order(word2topic[,t], decreasing=T)[1:min(WpT, length(which(word2topic[,t]!=0)))]))
  return(totake)
}

get.infotext.lmr <- function(texts) {
  return(paste(c("", "Left: ", "Middle: ", "Right: "), texts,sep=""))
}