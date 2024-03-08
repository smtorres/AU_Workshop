#### Code for class at Oxford
#### Date: 01/06/2023

## Import packages
rm(list=ls())
library(stm)
library(plyr)
library(doMC)
library(paletteer)
library(rhdf5)

doMC::registerDoMC(cores=6)

## Main directory
main_dir <- "~/Documents/Github/AU_Workshop/"
data_dir <- "~/Downloads/"

# Load functions for visual STM
source(paste0(main_dir,"Functions2ProcessIVW.R"))

## Import matrixes
ids <- read.csv(paste0(data_dir, "index_imgs.csv"))
ids$X0 <- gsub("^(b\\')|\\'$", "", ids$X0)
ids$ID <- gsub("(\\.[a-z]{3,5})$", "", ids$X0)
bovw_blm <- data.frame(id=ids$X0, t(h5read(paste0(data_dir, "ivwm_trump.hdf5"), "/bovw")))
colnames(bovw_blm)[2:51] <- paste0("vis_",0:49)
bovw_blm$id2 <- gsub("(\\.[a-z]{3,4})$", "", bovw_blm$id)

## Import metadata
metadata <- read.csv(paste0(data_dir, "news_articles_trump.csv"))
head(metadata)
metadata$id2 <- paste0("image_", metadata$id)
metadata <- merge(metadata, bovw_blm[,c("id", "id2")], by.x = "id2", by.y="id2")
bovw_blm <- bovw_blm[,-52]
library(lubridate)
metadata$date <- as.Date(metadata$publishedAt)

## Create the input for STM
stmobj_blm <- GetVisualSTM(ivwmat=bovw_blm, metadata=metadata,
                             outputdir =paste0(main_dir, "Results/"),
                             ivwmatid='id2', metaid = 'id2',
                             outmat =  "stm_ivwmat_blm.Rda")

## Run the different topic models
set.seed(12345)
k=4
stm_k4 <- stm(documents=stmobj_blm$doclist, 
               vocab=stmobj_blm$vocab, 
               K=k, prevalence = ~ date + source_name,
               data = metadata, seed=12345)

## Results
for(i in 1:k){dir.create(paste0(main_dir,"Results/Topic",i))}
top_words <- saveWords(stmobj= stm_k4, numwords=5, crit='frex', 
                       dir_topics= paste0(main_dir,"Results"), dir_vis= paste0(data_dir,"Vocabulary"), copy_vw=TRUE)

# Obtain 5 most representative images of each topic
representative_topics <- showTopPics(stmobj=stmobj_blm, 
                                     visualstm=stm_k4, k=k, numrepimgs = 5, 
                                     dir_topics =paste0(main_dir,"Results"), 
                                     dir_imgs="~/Dropbox/UCLA/CLASS_MATERIAL/IMAGE_ANALYSIS_WORKSHOP/Images3", copy_image = TRUE)

# Estimate effect
set.seed(2901)
estmod <- estimateEffect(c(1:4) ~ as.factor(source_name), stm_k4, metadata = metadata)

# Use the plotting function to extract estimates of the topic proportion by ideological group
plotmod <- plot.estimateEffect(estmod, "source_name", method = "pointestimate", topics = c(1:4), omit.plot = FALSE)


# Beutify it a bit
# Plot 1
cis_low <- plotmod$cis[[1]][1,][order(plotmod$means[[1]], decreasing = FALSE)]
cis_hi <- plotmod$cis[[1]][2,][order(plotmod$means[[1]], decreasing = FALSE)]
means <- plotmod$means[[1]][order(plotmod$means[[1]], decreasing = FALSE)]
labels1 <- gsub("^(Topic\\s[0-9]{1,2}.Covariate\\sLevel.\\s)|(\\))$", "", plotmod$labels)[seq(1,16,5)][order(plotmod$means[[1]], decreasing = FALSE)]
pal <- colorRampPalette(c("blue", "red"))
#pal =  rev(heat.colors(4))
mypal <- pal(4)
plot(0,0, type="n", xaxt="n", yaxt="n", xlim=c(-0.45, 0.45), ylim=c(0,17), xlab="", ylab="")
segments(cis_low, seq(0.5,16.5,5), cis_hi, seq(0.5,16.5,5), lwd=2, col="gray60")
points(means, seq(0.5,16.5,5), pch=16, col=mypal)
abline(v=0, lty=2, col="red")
text(means-0.15, seq(0.75,16.75,5), labels1, cex=0.8)
axis(1, tck=0.01, cex.axis=0.9, cex.lab=0.9, mgp=c(0.3, 0.3, 0), lwd=0, lwd.ticks = 1)
title(xlab = "Mean proportion", line = 1.5, cex.lab = 1, ylab="")
title(xlab = "", line = 1, cex.lab = 1, ylab="Media outlet")
title(line = 1, main="Proportion of Topic 4 by media outlet",
      font.main=1, cex.main=1.1)

# Plot 2
cis_low2 <- plotmod$cis[[2]][1,][order(plotmod$means[[2]], decreasing = FALSE)]
cis_hi2 <- plotmod$cis[[2]][2,][order(plotmod$means[[2]], decreasing = FALSE)]
means2 <- plotmod$means[[2]][order(plotmod$means[[2]], decreasing = FALSE)]
labels2 <- gsub("^(Topic\\s[0-9]{1,2}.Covariate\\sLevel.\\s)|(\\))$", "", plotmod$labels)[seq(2,34,2)][order(plotmod$means[[2]], decreasing = FALSE)]
pal <- colorRampPalette(c("blue", "red"))
pal =  rev(heat.colors(17))
mypal <- pal(17)
plot(0,0, type="n", xaxt="n", yaxt="n", xlim=c(-0.45, 0.45), ylim=c(0,17), xlab="", ylab="")
segments(cis_low2, seq(0.5,16.5,1), cis_hi2, seq(0.5,16.5,1), lwd=2, col="gray60")
points(means2, seq(0.5,16.5,1), pch=16, col=mypal)
abline(v=0, lty=2, col="red")
text(means2-0.15, seq(0.75,16.75,1), labels2, cex=0.8)
axis(1, tck=0.01, cex.axis=0.9, cex.lab=0.9, mgp=c(0.3, 0.3, 0), lwd=0, lwd.ticks = 1)
title(xlab = "Mean proportion", line = 1.5, cex.lab = 1, ylab="")
title(xlab = "", line = 1, cex.lab = 1, ylab="Media outlet")
title(line = 1, main="Proportion of Topic 11 by media outlet",
      font.main=1, cex.main=1.1)

