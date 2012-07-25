# Code for processing data as input for CircCompViz to produce circular visualizations of decompositions
# Juuso Parkkinen, 18.3.2011
# TODO: Update this information!

# Main function for writing input for the Circular Component Vizualization -script
writeCircCompVizInput <- function(d2t, w2t, output.file, 
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
  
  filepath <- "/share/mi/exp/multirex/source/CircCompViz/input/"
  if (hl.topic != -1) # Fix for Java indexing
    hl.topic <- hl.topic -1
  parameters <- c(output.file, dim(d2t)[2], hl.topic)
  write.table(parameters, file=paste(filepath, "parameters.txt", sep=""), row.names=F, col.names=F, quote=F)
  write.table(infotext, file=paste(filepath, "infotext.txt", sep=""), row.names=F, col.names=F, quote=F)
  
  if (!is.null(doc.names))
    write.table(doc.names, file=paste(filepath, "doc_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  else
    write.table(rownames(d2t), file=paste(filepath, "doc_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  if (!is.null(word.names))
    write.table(word.names, file=paste(filepath, "word_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  else 
    write.table(rownames(w2t), file=paste(filepath, "word_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  
  write.table(1:dim(d2t)[2], file=paste(filepath,"topics.txt", sep=""), row.names=F, col.names=F, quote=F)
  if (!is.null(topic.names))
    write.table(topic.names, file=paste(filepath, "topic_names.txt",sep=""), row.names=F, col.names=F, quote=F)
  else
    write.table(1:dim(d2t)[2], file=paste(filepath, "topic_names.txt",sep=""), row.names=F, col.names=F, quote=F)
  write.table(d2t, file=paste(filepath, "doc2topic.txt",sep=""), row.names=F, col.names=F, sep="\t")
  write.table(w2t, file=paste(filepath, "word2topic.txt",sep=""), row.names=F, col.names=F, sep="\t")

  write.table(font.sizes, file=paste(filepath, "font_sizes.txt", sep=""), row.names=F, col.names=F, quote=F)
  write.table(radius.adjustments, file=paste(filepath, "radius_adjustments.txt", sep=""), row.names=F, col.names=F, quote=F)
  write.table(curve.factors, file=paste(filepath, "curve_factors.txt", sep=""), row.names=F, col.names=F, quote=F)

  if (length(doc.colors)==1) {
    if (is.na(doc.colors))
      doc.colors <- rep("black", nrow(d2t))
    else 
      doc.colors <- rep(doc.colors, nrow(d2t))
  }
  write.table(doc.colors, file=paste(filepath, "doc_colors.txt", sep=""), row.names=F, col.names=F, quote=F)

  if (length(word.colors)==1) {
    if (is.na(word.colors))
      word.colors <- rep("black", nrow(w2t))
    else 
      word.colors <- rep(word.colors, nrow(w2t))
  }
  write.table(word.colors, file=paste(filepath, "word_colors.txt", sep=""), row.names=F, col.names=F, quote=F)
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
#  DpT <- ceiling(Ndocs/length(totake$topics))
  topic2doc <- t(prop.table(doc2topic, 2))
  WpT <- ceiling(Nwords/length(totake$topics))
  totake$docs <- c(); totake$words <- c()
  for (t in totake$topics) {
#    totake$docs <- unique(c(totake$docs, order(doc2topic[,t], decreasing=T)[1:DpT]))
#    totake$docs <- unique(c(totake$docs, which(topic2doc[t,] > doc.p.th)))
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
infotext1 <- get.infotext.lmr(c("Toxicity summary (class 1, GO)", "31 toxic chemicals", "Top 10 toxic components", "Top genesets for the components"))




## Add cell line and toxicity info (if available)
extend.chem.names <- function(chem.ids, values=c("CMAP", "GI50")) {


  chem.names <- c()
  for (i in chem.ids) {
    
    ## Values
    nci60.ind <- match(i, nci60.df$inds2LDA)
    if (!is.na(nci60.ind)) {
      tx <- c()
      for (val in values) {
        if (length(tx)>0)
          tx <- paste(tx,",",val,"=",nci60.df[[val]][nci60.ind],sep="")
        else
          tx <- paste(tx, val,"=",nci60.df[[val]][nci60.ind],sep="")
      }
    } else {
      tx <- "."
    }
    chem.names <- tolower(c(chem.names, paste(toxicity$chem.names[i], " (", tx, ")", sep="")))
  }
  return(chem.names)
}


## Add cell line and toxicity info (if available)
extend.chem.names.old <- function(chem.ids, toxicity, cell.line.type="abbr", tox.info.type="dose.tox") {

  chem.names <- c()
  for (i in chem.ids) {

    ## Cell line
    if (cell.line.type == "abbr")
      cl <- c("M", "P", "H")[match(toxicity$cell.lines[i], c("MCF7", "PC3", "HL60"))]
    else if (cell.line.type == "full")
      cl <- toxicity$cell.lines[i]
    else if (cell.line.type == "none")
      cl <- ""
    else
      stop("Invalid cell.line.type given!")
    
    ## GI50 and dose
    if (tox.info.type == "gi50, dose")
      tx <- paste("GI50:",round(log10(toxicity$doses[i]), digits=1)-toxicity$tox.value[i],", Dose:",round(log10(toxicity$doses[i]), digits=1))
    else if (tox.info.type=="dose.tox") {
      tx = toxicity$tox.value[i]
      if (is.na(tx))
        tx="."
      else if (tx>0)
        tx = paste("+",tx,sep="")
    }
    else if (tox.info.type=="class")
      tx = toxicity$tox.class[i]
    else if (tox.info.type=="none")
      tx = ""
    else
      stop("Invalid tox.info.type given!")
    
    if (cl == "" & tx == "")
      chem.names <- c(chem.names, paste(toxicity$chem.names[i]))
    else if (cl != "" & tx == "")
      chem.names <- c(chem.names, paste(toxicity$chem.names[i], " (", cl, ")", sep=""))
    else if (cl == "" & tx != "")
      chem.names <- c(chem.names, paste(toxicity$chem.names[i], " (", tx, ")", sep=""))
    else if (cl != "" & tx != "")
      chem.names <- c(chem.names, paste(toxicity$chem.names[i], " (", cl, ",", tx, ")", sep=""))
  }
  return(chem.names)
}

## # Main function for writing the Circular Component Vizualization input
## WriteCircCompVizInput20110328 <- function(totake, doc2topic, word2topic, output.folder, output.file, infotext, doc.names=NULL, topic.names=NULL, word.names=NULL) {
## ##  cat("Remember to add infotext!\n")
##   ## Get probability matrices
##   d2t <- matrix(doc2topic[totake$docs, totake$topics], ncol=length(totake$topics), dimnames=list(rownames(doc2topic)[totake$docs]))
##   w2t <- matrix(word2topic[totake$words, totake$topics], ncol=length(totake$topics), dimnames=list(rownames(word2topic)[totake$words]))
##   filepath <- "circviz/towrite/"
##   write.table(rbind(output.folder, output.file, length(totake$topics)),
##               file=paste(filepath, "parameters.txt", sep=""), row.names=F, col.names=F, quote=F)
##   write.table(infotext, file=paste(filepath, "infotext.txt", sep=""), row.names=F, col.names=F, quote=F)
  
##   if (!is.null(doc.names))
##     write.table(doc.names, file=paste(filepath, "doc_names.txt", sep=""), row.names=F, col.names=F, quote=F)
##   else
##     write.table(rownames(d2t), file=paste(filepath, "doc_names.txt", sep=""), row.names=F, col.names=F, quote=F)
##   if (!is.null(word.names))
##     write.table(word.names, file=paste(filepath, "word_names.txt", sep=""), row.names=F, col.names=F, quote=F)
##   else 
##     write.table(rownames(w2t), file=paste(filepath, "word_names.txt", sep=""), row.names=F, col.names=F, quote=F)
  
##   write.table(1:length(totake$topics), file=paste(filepath,"topics.txt", sep=""), row.names=F, col.names=F, quote=F)
##   if (!is.null(topic.names))
##     write.table(topic.names, file=paste(filepath, "topic_names.txt",sep=""), row.names=F, col.names=F, quote=F)
##   else
##     write.table(totake$topics, file=paste(filepath, "topic_names.txt",sep=""), row.names=F, col.names=F, quote=F)
##   write.table(d2t, file=paste(filepath, "doc2topic.txt",sep=""), row.names=F, col.names=F, sep="\t")
##   write.table(w2t, file=paste(filepath, "word2topic.txt",sep=""), row.names=F, col.names=F, sep="\t")
## }
