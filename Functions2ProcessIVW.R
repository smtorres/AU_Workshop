# Functions to process the BoVW in R for STM purposes
############################################## Mini functions to be passed on GetVisualSTM
# Function to get matrix ready
getDocReady <- function(vec_words){
  index <- which(vec_words!=0)
  counts <- vec_words[index]
  return(as.matrix(rbind(as.integer(index),as.integer(counts))))
}

############################################################################################

# Important: IVWMAT is the document-term matrix with an ID as the first column
####### PROCESS IVW MATRIX INTO STM DOCUMENT LIST
GetVisualSTM <- function(ivwmat,metadata,
                         outputdir,
                         ivwmatid='id', metaid = 'ID',
                         outmat = 'STM_IVWMAT_001.csv',...){
  # Check number of words
  nw = ncol(ivwmat)-1
  
  # Save the IDs
  docsids <- metadata[,metaid]
  
  # Build the vocabulary
  vocab <- colnames(ivwmat)[2:(nw+1)]
  
  # Check that all words have at least one non-zero count. 
  check <- apply(ivwmat[,-1], 2, sum)
  
  # Adjust the vocabulary and the mat accordingly
  vocab<-vocab[as.numeric(which(check!=0))]
  ivwmat <- ivwmat[,c(1,(as.numeric(which(check!=0))+1))]
  newnw <- ncol(ivwmat)
  
  # Prepare the list with documents and counts of words necessary for stm
  print('Getting the documents in shape...')
  doclist <- alply(ivwmat[,-1],1,function(x) getDocReady(x), .parallel = TRUE)
  names(doclist) <- as.character(ivwmat[,1])
  
  myobj = list(doclist=doclist, vocab=vocab)
  save(myobj, file=paste0(outputdir,"/", outmat))
  return(myobj)
}

# Function to save visual Words
saveWords=function(stmobj, numwords=15, crit='frex', dir_topics, dir_vis, content=FALSE, copy_vw=TRUE){
  if(!content){
  labs = labelTopics(stmobj,n=numwords)
  matlab = labs[crit][[1]]
  k = nrow(matlab)
  for(i in 1:k){
    tdir = paste0(dir_topics,'/Topic',i)
    dir.create(tdir, showWarnings = FALSE)
    fls = paste0(dir_vis,'/',matlab[i,],".jpg")
    if(copy_vw){
      file.copy(fls, tdir)
    }
  }
  }
  else{
    matlab= sageLabels(stmobj, n=numwords) # Rows:topics, columns: words
    marglabs = matlab['marginal']$marginal[crit][[1]]
    covslabs = matlab['cov.betas']$cov.betas
    covslabs2 = list()
    for(i in 1:length(covslabs)){
      covslabs2[[i]] <- covslabs[[i]][paste0(crit,"labels")]
    }
    names(covslabs2) <- matlab$covnames
    k = nrow(marglabs)
    for(i in 1:k){
      tdir = paste0(dir_topics,'/Topic',i)
      dir.create(tdir, showWarnings = FALSE)
      fls = paste0(dir_vis,'/', marglabs[i,],".jpg")
      file.copy(fls, tdir)
      for(j in 1:length(matlab$covnames)){
        tdir_cov = paste0(dir_topics,'/Topic',i,"/",matlab$covnames[j])
        dir.create(tdir_cov, showWarnings = FALSE)
        fls_cov = paste0(dir_vis,'/', covslabs2[[j]][[1]][i,],".jpg")
        if(copy_vw){
          file.copy(fls_cov, tdir_cov)
          }
      }
    }
  }
  return(matlab)
}

# 'Create' text (visual word composition) per image
createText <- function(list, vocab){
  posword = as.numeric(list[1,])
  countword = as.numeric(list[2,])
  txt = sapply(1:length(posword), function(x) paste(rep(vocab[posword[x]],countword[x]),collapse =" " ))
  txt2 = paste(txt, collapse=" ")
  return(txt2)
}

# Choose the most representative images per topic
showTopPics <- function(stmobj, visualstm,
                        k = 10, numrepimgs = 5,
                        dir_topics, dir_imgs, isjpg=FALSE,
                        dir=TRUE,copy_image=TRUE){
  txts <- as.character(unlist(lapply(stmobj[[1]], function(x) createText(x, stmobj[[2]]))))
  if(copy_image){
    file_ls <- llply(1:k,function(x) {
    temp_ls <- findThoughts(visualstm, txts, topics=x,n=numrepimgs)
    index <- temp_ls$index[[1]]
    imgs_ls <- names(stmobj[[1]])[index]
    imgs_ls <- gsub("^(b\\')|\\'$", "", imgs_ls)
    if(dir){
    if(isjpg){
      files <- paste0(dir_imgs,"/",imgs_ls,".jpg")  
    }
    else{
      files <- paste0(dir_imgs,"/",imgs_ls)  
    }
    }
  else{
    files = imgs_ls
    }
    tdir = paste0(dir_topics,'/Topic',x,'/RepImg')
    dir.create(tdir, showWarnings = FALSE)
    file.copy(files, tdir, overwrite = FALSE)},
    .parallel = TRUE)
  }
  else{
    file_ls <- llply(1:k,function(x) {
      temp_ls <- findThoughts(visualstm, txts, topics=x,n=numrepimgs)
      index <- temp_ls$index[[1]]
      imgs_ls <- names(stmobj[[1]])[index]
      imgs_ls <- gsub("^(b\\')|\\'$", "", imgs_ls)
      if(dir){
        if(isjpg){
          files <- paste0(dir_imgs,"/",imgs_ls,".jpg")  
        }
        else{
          files <- paste0(dir_imgs,"/",imgs_ls)  
        }
      }
      else{
        files = imgs_ls
      }
      },
      .parallel = TRUE)    
  }
  return(file_ls)
}

# Draw braces for the plot. Code taken from the swfscMisc (by Eric Archer): https://github.com/EricArcher
braces <- function(xfrom, xto, yfrom, yto, radius = 1, col = par("fg"), lty = par("lty"), lwd = par("lwd")) {
  n <- max(length(xfrom), length(xto), length(yfrom), length(yto))
  if (length(xfrom) < n) xfrom <- rep(xfrom, n)[1:n]
  if (length(xto) < n) xto <- rep(xto, n)[1:n]
  if (length(yfrom) < n) yfrom <- rep(yfrom, n)[1:n]
  if (length(yto) < n) yto <- rep(yto, n)[1:n]
  if (length(radius) < n) radius <- rep(radius, n)[1:n]
  if (length(lwd) < n) lwd <- rep(lwd, n)[1:n]
  if (length(lty) < n) lty <- rep(lty, n)[1:n]
  if (length(col) < n) col <- rep(col, n)[1:n]
  
  theta <- seq(0, pi / 2, length = 100)
  sapply(1:length(xfrom), function(i) {
    xmid <- (xfrom[i] + xto[i]) / 2
    ymid <- (yfrom[i] +yto[i]) / 2
    sx <- sign(xto[i] - xfrom[i])
    sy <- sign(yto[i] - yfrom[i])
    vertical <- abs(xto[i] - xfrom[i]) * par("pin")[1] / (par("usr")[2] - par("usr")[1]) < abs(yto[i] - yfrom[i]) * par("pin")[2] / (par("usr")[4] - par("usr")[3])
    if (vertical) {
      rx <- abs(xfrom[i] - xmid)
      ry <- abs(yfrom[i] - yto[i]) / 10 * radius[i]
      if (min(yfrom[i], yto[i]) + ry > ymid - ry) warning("in braces, radius is too large", call. = FALSE)
      segments(xmid, c(yfrom[i] + sy * ry, ymid + sy * ry), xmid, c(ymid - sy * ry, yto[i] - sy * ry), 
               lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xfrom[i] + sx * rx * sin(theta), yfrom[i] + sy * ry - sy * ry * cos(theta), 
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xfrom[i] + sx * rx * sin(theta), yto[i] - sy * ry + sy * ry * cos(theta),
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xto[i] - sx * rx * sin(theta), ymid - ry + ry * cos(theta), 
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xto[i] - sx * rx * sin(theta), ymid + ry - ry * cos(theta), 
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
    } else {
      rx <- abs(xfrom[i] - xto[i]) / 10 * radius[i]
      ry <- abs(yfrom[i] - ymid)
      if (min(xfrom[i], xto[i]) + rx > xmid - rx) warning("in braces, radius is too large", call. = FALSE)
      segments(c(xfrom[i] + sx * rx, xmid + sx * rx), ymid, c(xmid - sx * rx, xto[i] - sx * rx), ymid, 
               lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xfrom[i] + sx * rx - sx * rx *cos(theta), yfrom[i] + sy * ry * sin(theta),
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xto[i] - sx * rx + sx * rx * cos(theta), yfrom[i] + sy * ry * sin(theta),
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xmid + rx - rx * cos(theta), yto[i] - sy * ry * sin(theta), 
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
      lines(xmid - rx + rx * cos(theta), yto[i] - sy * ry * sin(theta),
            lwd = lwd[i], lty = lty[i], col = col[i]
      )
    }
  })
  invisible(NULL)
}