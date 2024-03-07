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
main_dir <- "~/Documents/CVClass/"
data_dir <- "/Users/mt68/Dropbox/VisualCommunication/Data/BLM_Outlets"

# Load functions for visual STM
source(paste0(main_dir,"Functions2ProcessIVW.R"))

## Import matrixes
ids <- read.csv(paste0(main_dir, "indexBLM.csv"))
ids$X0 <- gsub("^(b\\')|\\'$", "", ids$X0)
ids$ID <- gsub("(\\.[a-z]{3,5})$", "", ids$X0)
bovw_blm <- data.frame(id=ids$X0, t(h5read(paste0(main_dir, "ivwm_blm.hdf5"), "/bovw")))
colnames(bovw_blm)[2:2001] <- paste0("vis_",0:1999)
bovw_blm$id2 <- gsub("(\\.[a-z]{3,4})$", "", bovw_blm$id)

## Import metadata
url_metadata <- read.csv(paste0(data_dir, "/url_images14.csv"), colClasses = c(tweet_id="character", media_key="character"))
tweet_metadata <- read.csv(paste0(data_dir, "/tweets/basedat14.csv"), colClasses = c(tweet_id="character", author_id="character", conversation_id="character"))
users_metadata<- read.csv(paste0(data_dir, "/users/users14.csv"), colClasses = c(id="character"))
users_metadata <- users_metadata[!duplicated(users_metadata$id),]
metadata <- merge(url_metadata, tweet_metadata, by="tweet_id", all.x = TRUE)
metadata <- merge(metadata, users_metadata, by.x = "author_id", by.y = "id", all.x = TRUE)
metadata <- metadata[!duplicated(metadata$media_key),]
metadata <- merge(metadata, bovw_blm[,c("id", "id2")], by.x = "media_key", by.y="id2")
metadata$date <- as.Date(metadata$created_at_tweet)
bovw_blm <- bovw_blm[,-2002]

## Create the input for STM
stmobj_blm <- GetVisualSTM(ivwmat=bovw_blm, metadata=metadata,
                             outputdir =paste0(main_dir, "results/"),
                             ivwmatid='id', metaid = 'id',
                             outmat =  "stm_ivwmat_blm.Rda")

## Run the different topic models
set.seed(12345)
stm_k12 <- stm(documents=stmobj_blm$doclist, 
               vocab=stmobj_blm$vocab, 
               K=12, prevalence = ~ date + user,
               data = metadata, seed=12345)

## Results
for(i in 1:12){dir.create(paste0(main_dir,"results/Topic",i))}
top_words <- saveWords(stmobj= stm_k12, numwords=10, crit='frex', 
                       dir_topics= paste0(main_dir,"results"), dir_vis= paste0(main_dir,"Vocabulary"), copy_vw=TRUE)

# Obtain 10 most representative images of each topic
representative_topics <- showTopPics(stmobj=stmobj_blm, 
                                     visualstm=stm_k12, k=12, numrepimgs = 8, 
                                     dir_topics =paste0(main_dir,"results"), dir_imgs=paste0(data_dir,"/Images"), copy_image = TRUE)

# Estimate effect
set.seed(2901)
metadata$user <- as.factor(metadata$user)
estmod <- estimateEffect(c(4,11) ~ as.factor(user), stm_k12, metadata = metadata)

# Use the plotting function to extract estimates of the topic proportion by ideological group
plotmod <- plot.estimateEffect(estmod, "user", method = "pointestimate", topics = c(4,11), omit.plot = FALSE)


# Beutify it a bit
# Plot 1
cis_low <- plotmod$cis[[1]][1,][order(plotmod$means[[1]], decreasing = FALSE)]
cis_hi <- plotmod$cis[[1]][2,][order(plotmod$means[[1]], decreasing = FALSE)]
means <- plotmod$means[[1]][order(plotmod$means[[1]], decreasing = FALSE)]
labels1 <- gsub("^(Topic\\s[0-9]{1,2}.Covariate\\sLevel.\\s)|(\\))$", "", plotmod$labels)[seq(1,33,2)][order(plotmod$means[[1]], decreasing = FALSE)]
pal <- colorRampPalette(c("blue", "red"))
pal =  rev(heat.colors(17))
mypal <- pal(17)
plot(0,0, type="n", xaxt="n", yaxt="n", xlim=c(-0.45, 0.45), ylim=c(0,17), xlab="", ylab="")
segments(cis_low, seq(0.5,16.5,1), cis_hi, seq(0.5,16.5,1), lwd=2, col="gray60")
points(means, seq(0.5,16.5,1), pch=16, col=mypal)
abline(v=0, lty=2, col="red")
text(means-0.15, seq(0.75,16.75,1), labels1, cex=0.8)
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

