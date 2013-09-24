




#' Return up to 22 distinct colours.
#' 
#' Useful if you want to colour 22 autosomes, etc, because most R
#' colour palettes only provide 12 or fewer colours, or else provide,
#' a gradient which is not distinguishable for discrete categories.
#' Manually curated so the most similar colours aren't side by side.
#'
#' @param n number of unique colours to return
#' @return returns vector of n colours
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' get.distinct.cols(10)
#' plot(1:22,pch=19,col=get.distinct.cols(22))
get.distinct.cols <- function(n=22)
{
  distinct.cols <- c(38,6,23,11,12,26,30,31,94,134,100,47,139,53,58,68,116,128,172,142,367,656,77)
  # reorder so similar colours aren't adjacent.
  distinct.cols <- distinct.cols[c(16,10,5,1,21,8,13,18,7,11,3,20,22,14,2,6,19,4,17,12,9,15)]
  colz <- colors()[distinct.cols[1:min(n,length(distinct.cols))]]
  if(n>length(distinct.cols)) { warning(paste(n,"requested, but only",length(distinct.cols),"colors were available")) }
  return(colz)
}




#' Unlist a list, starting only from a set depth.
#' 
#' Allows unlisting preserving the top levels of a list.
#' Can specify the number of list depth levels to skip 
#' before running unlist()
#'
#' @param obj the list to unlist
#' @param depth skip to what layer of the list before unlisting; eg.
#'  the base unlist() function would correspond to depth=0
#' @return returns vectors of strings of char, lengths X
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' complex.list <- list(1,1:3,list(2,2:4,list(3,3:4,list(10))),list(4,5:7,list(3)))
#' Unlist(complex.list,0) # equivalent to unlist()
#' Unlist(complex.list,1) # unlist within the top level lists
#' Unlist(complex.list,2) # unlist within the second level lists
#' Unlist(complex.list,10) # once depth >= list-depth, no difference
#' unlist(complex.list,recursive=FALSE) # not the same as any of the above
Unlist <- function(obj,depth=1) {
  if(depth==0) { return(unlist(obj)) }
  if(length(obj)>0) {
    for(cc in 1:length(obj)) {
      if(is.list(obj)) {
        if(is.list(obj[[cc]])) {
          if(depth<=1) {
            names(obj[[cc]]) <- NULL 
            obj[[cc]] <- unlist(obj[[cc]])
          } else {
            obj[[cc]] <- Unlist(obj[[cc]],depth=depth-1)
          }
        }
      }
    }
    return(obj)
  } else {
    return(obj) 
  }
}



#' Convert a numeric vector to Z-scores.
#' 
#' Transform a vector to z scores by subtracting its mean
#'  and dividing by its standard deviation
#'
#' @param X numeric vector to standardize
#' @return vector of the same length in standardised form
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' x1 <- rnorm(10,100,15); x2 <- sample(10)
#' print(x1) ;  standardize(x1)
#' print(x2) ;  standardize(x2)
standardize <- function(X)
{
  if(!is.numeric(X)) { stop("x must be numeric") }
  if(length(X)>1) {
    u <- mean(X,na.rm=T)
    s <- sd(X,na.rm=T)
    return((X-u)/s)
  } else {
    warning("X should have length>1")
    return(X)
  }
}



#' Find data thresholds corresponding to percentiles
#' 
#' Finds the top and bottom bounds corresponding to percentile
#' 'pc' of the data 'dat'.
#'
#' @param dat numeric vector of data
#' @param pc the percentile to seek, c(pc, 1-pc)
#' @return returns the upper and lower threshold
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' pctile(rnorm(100000),.025)
#' pctile(sample(100),.9)
pctile <- function(dat,pc=0.01)
{
  # get top/bottom bounds for percentile 'pc' of data 'dat'
  if(!is.numeric(dat)) { stop("dat must be numeric") }
  rr <- rank(dat,na.last=NA)
  tpbt <- round(c(pc,1-pc)*max(rr,na.rm=T))
  ord <- sort(narm(dat))
  if(tpbt[1]==0) { tpbt[1] <- 1 }
  pcts <- ord[tpbt]
  return(pcts)
}




#' Check whether a given system command is installed (e.g, bash)
#' 
#' Tests whether a command is installed and callable by system().
#' Will return a warning if run on windows
#'
#' @param cmd list of commands to test
#' @return returns true or false for each command 'cmd'
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' check.linux.install("ls") # should be standard
#' check.linux.install(c("perl","sed","fake-cmd"))
check.linux.install <- function(cmd=c("plink","perl","sed")) {
  if(tolower(.Platform$OS.type)=="windows") { warning("function only valid on macOS and Linux") ; return(F) }
  anz <- character(length(cmd))
  for (dd in 1:length(cmd)) {
    anz[dd] <- system(paste("if hash",cmd[dd],"2>/dev/null; then echo 'yes'; else echo 'no'; fi"),intern=T)
  }
  out <- (anz=="yes")
  if(any(!out)) { warning(paste("command '",paste(cmd[!out],collapse=","),"' not installed",sep="")) }
  names(out) <- cmd
  return(out)
}



#' A good way to preview large lists.
#' 
#' An alternative to head(list) which allows limiting of large list 
#'  components in the console display
#'
#' @param x a list to preview
#' @param n The number of values to display for the deepest nodes
#'  of the list
#' @param skip number of first level elements to display before skipping
#'  the remainder
#' @param skip2 number of subsequent level elements to display before 
#'  skipping the remainder
#' @param ind indent character for first level elements
#' @param ind2 indent character for subsequent level elements
#' @return prints truncated preview of a large list
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' sub1 <- list(list(1:100),list(2:101),list(101:200),list(201:300),list(301:400))
#' big.list <- list(sub1,sub1,sub1,sub1,sub1,sub1)
#' headl(sub1)
#' headl(big.list,skip=2)
headl <- function(x,n=6,skip=20,skip2=10,ind="",ind2="  ")
{
  if(!is.list(x)) { warning("not a list"); return(NULL) }
  ll <- length(x)
  if(ll>0) {
    ind.new <- paste(ind,ind2,sep="")
    if(ll>skip) { ll <- skip ; skipped <- T } else { skipped <- F }
    for(cc in 1:ll)
    {
      if(!is.null(names(x))) 
      { cat(ind,"$",names(x)[cc],":\n",sep="") } else { (cat("[[",cc,"]]\n",sep="")) }
      if (is(x[[cc]],"list"))
      {
        headl(x[[cc]],n,ind=ind.new,ind2=ind2,skip=skip2,skip2=skip2)
      } else {
        cat(ind,sep="")
        print(head(x[[cc]],n))
      }
    }
    if(skipped) { cat(ind,"... skipped ",length(x)-skip," ...\n",sep="")}
  } else {
    cat(ind,"<empty>","\n",sep="")
  }
}




#' Return an object with missing values removed.
#' 
#'
#' Convenience function, removes NAs from most standard objects.
#' Uses function na.exclude for matrices and dataframes. 
#' Main difference to na.exlude is that it simply performs the 
#' transformation, without adding attributes
#' For unknown types, leaves unchanged with a warning.
#'
#' @param X The object to remove NAs, any vector, matrix or data.frame
#' @return Vector minus NA's, or the matrix/data.frame minus NA rows
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' narm(c(1,2,4,NA,5))
#' DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
#' DF; narm(DF)
#' # if a list, will only completely remove NA from the lowest levels
#' # empty places will be left at top levels
#' print(narm(list(1,2,3,NA,list(1,2,3,NA))))
narm <- function(X) { 
  if(is.data.frame(X) | is.matrix(X)) {
    X <- na.exclude(X)
    attr(X,"na.action") <- NULL
    return(X)
  } else {
    if(is.vector(X)) {
      if(is.list(X)) {
        if(any(sapply(X,length)>1)) {
          X <- lapply(X,narm) 
          return(X)
        } else {
          return(X[!is.na(X)])
        }
      } else {
        return(X[!is.na(X)])  
      }
    } else {
      warning("unsupported type, X unchanged"); return(X)
    }
  }
}


#' Return a string with each first letter of each word in upper case.
#' 
#' @param txt a character string
#' @param strict whether to force non-leading letters to lowercase
#' @return Vector minus NA's, or the matrix/data.frame minus NA rows
#' @export 
#' @author via R Core
#' @examples
#' toheader(c("using AIC for model selection"))
#' toheader(c("using AIC", "for MODEL selection"), strict=TRUE)
toheader <- function(txt, strict = FALSE) {
  if(!is.character(txt)) { stop("text must be character()") }
  cap <- function(txt) { 
    txt.m <- substring(txt,2); if(strict) { txt.m <- tolower(txt.m) } 
    paste(toupper(substring(txt,1,1)),txt.m,sep = "", collapse = " " ) 
  }
  sapply(strsplit(txt, split = " "), cap, USE.NAMES = !is.null(names(txt)))
}



#' Print heading text with a border.
#'
#' Makes highly visible headings, can separately horizontal, 
#' vertical and corner characters
#'
#' @param txt The text to display in the centre
#' @param h the ascii character to use on the horizontal sections of
#'  the border, and used for v,corner too if not specified separately
#' @param v the character to use on vertical sections of the border
#' @param corner the character to use on corner sections of the border
#' @param align alignment of the writing, when there are multiple lines,
#'  e.g, "right", "left", "centre"/"center"
#' @return returns nothing, simply prints the heading to the console
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' Header("Section 1")
#' Header("Section 1",h="-",v="|",corner="*")
#' Header(c("SPACE","The final frontier"))
#' Header(c("MY SCRIPT","Part 1"),align="left",h=".")
Header <- function(txt,h="=",v=h,corner=h,align="center") {
  ## Make a heading with a box for text (can be multiple lines) optional horiz/vert/corner symbols
  if(!is.character(txt)) { stop("txt must be character()") }
  nC <- nchar(txt); align <- tolower(align); if(align!="right" & align!="left") { align <- "center" }
  v <- substr(v,1,1); h <- substr(h,1,1); corner <- substr(corner,1,1)
  extend <- function(X,L,align) { 
    nn <- (L-nchar(X))
    switch(align,right=paste(spc(nn),X,sep=""),
           left=paste(X,spc(nn),sep=""),
           center=paste(spc(floor(nn/2)),X,spc(ceiling(nn/2)),sep="")) }
  mC <- max(nC)
  txt <- extend(txt,mC,align)
  aline <- c(corner,rep(h,mC+2),corner)
  cat("\n",aline,"\n",sep="")
  cat(paste(v," ",txt," ",v,"\n",sep=""),sep="")
  cat(aline,"\n\n",sep="")
}



#' Print a character a specified number of times.
#' 
#' Returns 'char' X_i number of times for each element i of X.
#' Useful for padding for alignment purposes.
#'
#' @param X numeric vector of number of repeats
#' @param char The character to repeat (longer will be shortened)
#' @return returns vectors of strings of char, lengths X
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @seealso \code{\link{rmv.spc}}
#' @examples
#' cat(paste(spc(9),"123\n"))
#' cat(paste(spc(8),"1234\n"))
#' spc(c(1:5),".")
spc <- function(X,char=" ") { 
  if(!is.numeric(X)) { stop("X must be numeric") }
  ch <- substr(paste(char)[1],1,1)
  lX <- length(X); out <- rep("",lX)
  for(j in 1:lX) {
    if(X[j]>0) { out[j] <- paste(rep(ch,times=X[j]),collapse="") }
  } 
  return(out) 
}


#' Remove leading and trailing spaces (or other character).
#'
#' @param str character vector, may containing leading or trailing chars
#' @param before logical, whether to remove leading spaces
#' @param after logical, whether to remove trailing spaces
#' @param char an alternative character to be removed instead of spaces
#' @return returns vectors without the leading/trailing characters
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @seealso \code{\link{spc}}
#' @examples
#' rmv.spc("  mid sentence  ")
#' rmv.spc("0012300",after=FALSE,char="0")
#' rmv.spc(" change nothing ",after=FALSE,before=FALSE)
rmv.spc <- function(str,before=T,after=T,char=" ") {
  # remove spaces at start and end of string
  if(!is.character(str)) { warning("not a character() type") ; return(str) }
  ch <- substr(paste(char)[1],1,1)
  kk <- (length(str))
  if(kk<1) { return(str) }
  for (cc in 1:kk) {
    if(before){
      while(substr(str[cc],1,1)==ch) {
        if(nchar(str[cc])>1) {
          str[cc] <- substr(str[cc],2,nchar(str[cc])) 
        } else {
          str[cc] <- gsub(ch,"",str[cc])
        }
      }
    }
    if(after) {
      while(substr(str[cc],nchar(str[cc]),nchar(str[cc]))==ch) {
        if(nchar(str[cc])>1) {
          str[cc] <- substr(str[cc],1,nchar(str[cc])-1)
        } else {
          str[cc] <- gsub(ch,"",str[cc])
        }
      }
    }
  }
  return(str)
}


#' Estimate the memory required for an object.
#'
#' An existing object or just dim/length of a proposed object
#'
#' @param dat either a matrix/dataframe, or else dims; c(nrow,ncol)
#' @return returns minimum memory requirement in GB (numeric, scalar)
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' estimate.memory(matrix(rnorm(100),nrow=10))
#' estimate.memory(c(10^6,10^4))
#' estimate.memory(5.4*10^8)
estimate.memory <- function(dat)
{
  # based on a numeric object, estimate the minimum memory requirement
  if(!is.null(dim(dat))) { dimz <- dim(dat) } else { dimz <- dat }
  if(length(dimz)==1) { dimz[2] <- 1 }
  if(length(dimz)==2) {
    rws <- dimz[1]; cls <- dimz[2]
    cells.per.gb <- 2^27  # size of double() resulting in ~1GB of memory use by R 2.15
    memory.estimate <- as.double((as.double(rws)*as.double(cls))/cells.per.gb)
    return(memory.estimate)
  } else {
    warning("tried to estimate memory for object which is neither a pair of dimension sizes or a dataframe/matrix") 
  }
}


#' Wait for a period of time.
#' 
#' Waits a number of hours minutes or seconds (doing nothing).
#' This will use 100% of 1 cpu.
#'
#' @param dur waiting time
#' @param unit time units h/m/s, seconds are the default
#' @param silent print text showing that waiting is in progress
#' @return no return value
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' wait(.5,silent=FALSE) # wait 0.5 seconds
#' wait(0.001, "m")
#' wait(0.0001, "Hours", silent=FALSE)
wait <- function(dur,unit="s",silent=T) {
  ## do nothing for a period of time
  if(!is.numeric(dur)) { stop("dur must be a number") }
  if(!is.logical(silent)) { silent <- F }
  unit <- tolower(substr(paste(unit),1,1))
  jj <- proc.time()[3]; mm <- 1
  if(unit=="s") { mm <- 1 }
  if(unit=="m") { mm <- 60 }
  if(unit=="h") { mm <- 3600 }
  if(!silent) { timz <- c("hour","minute","second");
                cat("waiting ",dur," ",timz[grep(unit,timz)],"s...",sep="") }
  while((proc.time()[3]-jj)< (mm*dur)) { NULL  }
  if(!silent) { cat("done\n") }
}


#' Times an expression, with breakdown of time spent in each function
#' 
#' A wrapper for the proftools package Rprof() function.
#' It is to Rprof() as system.time() is to proc.time() (base)
#' Useful for identifying which functions are taking the
#' most time. This procedure will return an error unless
#' expr takes more than ~0.1 seconds to evaluate. I 
#' could not see any simple way to avoid this limitation.
#' 
#' @param expr an expression, must take at least 1 second (roughly)
#' @param suppressResult logical, if true, will return timing 
#'   information rather than the result of expr
#' @param total.time to sort by total.time, else by self.time
#' @return returns matrix where rows are function names, and columns
#'  are self.time and total.time. total.time is total time spent 
#'  in that function, including function calls made by that function.
#'  self.time doesn't count other functions within a function
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' timeit(wait(0.1,"s") ,total.time=TRUE)
#' timeit(wait(0.1,"s") ,total.time=FALSE)
timeit <- function(expr,suppressResult=F,total.time=T) {
  # function to measure in detail which function calls take the most time
  # during the evaluation of an expression. NB: will error with use of a trivial/instant expression
  require("proftools")
  tf <- "Rproftemp.out"
  Rprof("Rproftemp.out")
  #do the stuff
  result <- { expr }
  Rprof()
  rd <- readProfileData("Rproftemp.out")
  tab <- flatProfile(rd,F)
  if(total.time) { col <- 5 } else { col <- 3 }
  summary <- head(tab[rev(order(tab[,col])),],30)[,c(3,5)]
  unlink(tf)
  if(suppressResult) {
    return(summary)
  } else {
    print(summary)
    return(result)
  }
}


#' Creates a progess bar within a loop
#' 
#' Only requires a single line within a loop to run, in contrast
#' with the built-in tracker which requires a line to initialise,
#' and a line to close. Also has option to backup objects during long loops.
#' Ideal for a loop with a counter such as a for loop.
#' Tracks progress as either percentage of time remaining or
#' by intermittently displaying the estimated number of minutes to go
#'  
#' @param cc integer, current value of the loop counter
#' @param max integer, final value of the loop counter
#' @param st.time 'start time' when using 'time to go' mode, taken 
#'  from a call to proc.time()
#' @param sav.obj optionally an object to backup during the course of 
#'  a very long loop, to restore in the event of a crash.
#' @param sav.fn the file name to save 'save.obj'
#' @param sav.freq how often to update 'sav.obj' to file, in terms of 
#'  percentage of run-time
#' @param unit time units h/m/s if using 'time to go' mode
#' @return returns nothing, simply prints progress to the console
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' # simple example with a for-loop
#' max <- 100; for (cc in 1:max) { loop.tracker(cc,max); wait(0.004,"s") }
#' #example using the 'time to go' with a while loop
#' cc <- 0; max <- 10; start <- proc.time()
#' while(cc < max) { cc <- cc + 1; wait(0.05,"s"); loop.tracker(cc,max,start,unit="s") }
#' # example with saving an object, and restoring after a crash
#' X <- matrix(rnorm(5000),nrow=50); max <- nrow(X); sums <- numeric(max)
#' for (cc in 1:max) { 
#'   sums[cc] <- sum(X[cc,])
#'   wait(.1) # just so this trivial loop doesn't finish so quickly
#'   loop.tracker(cc,max, sav.obj=sums, sav.fn="temp.rda", sav.freq=5);
#'   if(cc==29) { warning("faked a crash at iteration 29!"); rm(sums); break }
#' }
#' cat("\nloaded latest backup from iteration 28:",paste(load("temp.rda")),"\n")
#' print(sav.obj); unlink("temp.rda")
loop.tracker <- function(cc, max, st.time=NULL,  
                         sav.obj=NULL, sav.fn=NA,
                         sav.freq=10, unit=c("m","s","h")[1])
{
  # insert this function into any loop with a counter and it will provide
  # a progress tracker either as a percentage or estimate of time remaining
  ## cc is loop counter, max is the last value the loop should take
  ## st.time is the result of a call to proc.time().
  cc <- round(as.numeric(cc)); max <- round(as.numeric(max))
  freq <- round(max/min(max,50)); 
  if(cc<1) { return() }
  if(cc>max) { cc <- max; warning("count passed to loop.tracker() exceed 'max'") }
  if(cc==1)
  {
    if(is.null(st.time)) {
      scale <- "0         25%         50%         75%         100%\n"
      cat(scale)
    } else {
      cat("Processing: time left (",unit,"):\n",sep="")
    }
  }
  if (cc %% freq == 0) {
    if(is.null(st.time))
    {
      intv <- diff(round(seq(from=1,to=51,length.out=(max/freq))))[cc %/% freq]
      if(!is.na(intv)) { if(intv>0) { cat(rep(".",intv),sep="") } } else {
        if(max==1) { cat(rep(".",50),sep="") }
      }
    } else {
      time.now <- proc.time()[3]-st.time[3]; time.per <- time.now/cc
      tm.u <- switch(unit,m=60,s=1,h=3600)
      to.go <- round(((max-cc)*time.per/tm.u))
      cat(to.go,"..",sep="") 
    }
    if((cc+freq)>max) { cat("\n") }
    ## save as we go - in case of crash
    if(abs(sav.freq)<1) { sav.freq <- abs(sav.freq)*100 } # allow decimal or integer percentage
    sav.freq <- round(max(1,min(50,(sav.freq/2))))
    if ((cc %% (sav.freq*freq)) == 0)
    {
      if(!is.null(sav.obj) & !is.na(sav.fn) & ((max-cc)>1) ) {
        save(sav.obj,file=sav.fn)
      } 
    }
  }
}




#' Make an ascii histogram in the console.
#' 
#' Uses a call to base::hist(...) and uses the densities to make a
#' a text histogram in the console
#' Particularly useful when working in the terminal without graphics.
#'
#' @param X numeric vector of data
#' @param range optional sub-range of X to test; c(low,high)
#' @param ... additional arguments passed to base::hist()
#' @return outputs an ascii histogram to the console
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' textogram(runif(100000))
#' textogram(rnorm(10000),range=c(-3,3))
textogram <- function(X,range=NA,...)
{
  # print a text based histogram of data, extra args may be passed to hist()
  if(!is.numeric(X)) { warning("X must be numeric") ; return(NULL) }
  if(all(!is.na(range))) { X <- X[X>=min(range) & X<=max(range)] }
  hdat <- hist(X,plot=F,...)
  dens <- round(100*hdat$density/sum(hdat$density))
  if(max(dens)>90) {
    cat(" using halved %'s as maximum freq is too big for terminal\n")
    dens <- dens/2
  }
  label <- pad.left(hdat$breaks,char=" ")
  for (cc in 1:length(dens))
  {
    cat(label[cc]," ",paste(rep(".",times=dens[cc]),collapse=""),"\n")
  }
}




#' Print a vector with appropriate padding so each has equal char length.
#' 
#' @param X vector of data to pad to equal length
#' @param char character to pad with, space is default, but zero might
#'  be a desirable choice for padding numbers
#' @param numdigits if using numeric data, the number of digits to keep
#' @return returns the vector in character format with equal nchar()
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' pad.left(1:10)
#' phone.numbers <- c("07429719234","7876345123","7123543765")
#' pad.left(phone.numbers,"0")
#' pad.left(rnorm(10),numdigits=3)
pad.left <- function(X, char=" ", numdigits=NA)
{
  pad <- function(X,L,char=" ") { paste(spc(L-nchar(X),char=char),X,sep="") }
  if (!is.na(numdigits)) { X <- round(X,numdigits)}
  max.len <- max(nchar(X))
  padn <- pad(X,L=max.len,char=char)
  return(padn)
}


#' Do everything possible to load an R package.
#' 
#' Like 'require()' except it will attempt to install a package if
#' necessary, and will also deal automatically with bioconductor
#' packages too.
#'
#' @param pcknms list of packages to load/install, shouldn't mix 
#'  bioconductor/CRAN in one call
#' @param bioC whether the listed packages are from bioconductor
#' @param reload indicates to reload the package even if loaded
#' @param avail when bioC=FALSE, see whether pcknms are in the list 
#'  of available CRAN packages
#' @param ask whether to get the user's permission to install a
#'  required package, or just go ahead and do it
#' @param quietly passed to library/require, display installation
#'  text or not
#' @return nothing, simply loads the packages specified if possible
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' # not run : run if you are ok to install/already have these packages
#' # must.use.package(c("MASS","nlme","lme4"),ask=FALSE)
#' # must.use.package("limma",bioC=TRUE)
#' # search() # show packages have loaded, then detach them again:
#' # sapply(paste("package",c("limma","MASS","nlme","lme4"),sep=":"),detach,character.only=TRUE)
must.use.package <- function(pcknms,bioC=F,ask=F,reload=F,avail=F,quietly=F)  
{
  ## like 'base::library()' but can easily work for bioconductor packages, and will
  # automatically attempt to install any required packages not found
  # reload is for when there might a conflict, so will detach and reload packages 
  # to force their version of a function with a duplicate name
  if(!bioC) { 
    repos <- getOption("repos")
    if(repos=="@CRAN@") { repos <- "http://cran.ma.imperial.ac.uk/" }
    if(avail) {
      goty <- getOption("pkgType"); 
      av.pk <- available.packages(type=goty,
          contrib.url(repos=repos, type=goty)) 
    }
  }
  for (cc in 1:length(pcknms))
  {
    nxt.pck <- pcknms[cc]
    if(!bioC & avail) { if(!nxt.pck %in% av.pk) { 
      warning(nxt.pck,
              " is not in the list of CRAN packages for the current version of R. ",
              "Either it has not compiled successfully for this version, or the name (",
              nxt.pck,") is wrong") } }
    if(reload) {
      fp <- paste("package:",nxt.pck,sep="")
      if(fp %in% search())  { 
        detach(name=fp,character.only=T)
      }
    }
    checklib <- function(package,character.only=F,warn.conflicts=T,quietly=F) { 
      do.call("require",args=list(package=package,character.only=character.only,
                             warn.conflicts=warn.conflicts,quietly=quietly)) 
    }
    got1 <- suppressWarnings(checklib(nxt.pck,character.only=T,warn.conflicts=F))
    if(!got1) {
      if(ask) {
        # ask whether to install a package
        ans <- select.list(c("yes","no"),"yes",F,paste("ok to install",nxt.pck," (required)?"))
      } else { 
        ans <- "yes" 
      }
      if(ans=="yes") {
        if(bioC) {
          biocLite <- function(x) { print("please load biocLite function from http://bioconductor.org/biocLite.R") }
          source("http://bioconductor.org/biocLite.R") # biocLite() should now be replaced
          biocLite(nxt.pck)
          suppressWarnings(checklib(nxt.pck,character.only=T,warn.conflicts=F,quietly=quietly))
        } else {
          install.packages(nxt.pck,repos=repos); 
          suppressWarnings(checklib(nxt.pck,character.only=T,warn.conflicts=F,quietly=quietly)) 
        }
      } else {
        warning("please manually install package ",nxt.pck," to continue")
      }
    } 
  }
}


#' Search all CRAN packages for those containing keyword(s).
#' 
#' Can be useful for trying to find new packages for a particular
#' purpose. No need for these packages to be installed or loaded.
#' Further searching can be done using utils::RSiteSearch()
#'
#' @param txt text to search for, a character vector, not case-sensitive
#' @param repos repository (CRAN mirror) to use, "" defaults to getOption("repos")
#' @return list of hits for each keyword (txt)
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' rep <- "http://cran.ma.imperial.ac.uk/" # OR: rep <- getOption("repos")
#' search.cran("useful",rep)
#' search.cran(c("hmm","markov","hidden"),repos=rep)
search.cran <- function(txt,repos="") {
  goty <- getOption("pkgType"); 
  if(repos=="") { repos <- getOption("repos") }
  av.pk <- available.packages(type=goty,
           contrib.url(repos=repos, type=goty))
  if(is.matrix(av.pk)) { 
    if("Package" %in% colnames(av.pk)) {
      av.pk <- av.pk[,"Package"]; dim(av.pk) <- NULL
    } else { av.pk <- av.pk[[1]] }
  } else { warning("lookup did not return table with header 'Package'") }
  if(is.character(av.pk) & is.character(txt)) {
    if(!is.null(names(av.pk))) { names(av.pk) <- NULL }
    if(length(txt)>0) {
      out <- vector("list",length(txt)); names(out) <- txt
      for(cc in 1:length(txt)) {
        out[[cc]] <- av.pk[grep(txt[cc],av.pk,ignore.case=T)]
      }
    }
  } else {
    warning("txt must be character, and must be online to search for available.packages()")
  }
  return(out)
}



#' Find the mode of a vector.
#' 
#'
#' The mode is the most common value in a series.
#' This function can return multiple values if there are equally
#' most frequent values, and can also work with non-numeric types.
#'
#' @param x The data to take the mode from. Dimensions and NA's are 
#'  removed if possible, strings, factors, numeric all permitted
#' @param multi Logical, whether to return multiple modes if values
#'  have equal frequency
#' @param warn Logical, whether to give warnings when multiple values
#'  are found (if multi=FALSE)
#' @return The most frequent value, or sorted set of most frequent
#'  values if multi==TRUE and there are more than one. Numeric if x 
#'  is numeric, else as strings
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' Mode(c(1,2,3,3,4,4)) # 2 values are most common, as multi=FALSE, 
#' # selects the last value (after sort)
#' Mode(c(1,2,3,3,4,4),multi=TRUE) # same test with multi=T, 
#' # returns both most frequent
#' Mode(matrix(1:16,ncol=4),warn=TRUE) # takes mode of the entire
#' # matrix treating as a vector, but all values occur once
#' Mode(c("Tom","Dick","Harry"),multi=FALSE,warn=TRUE) # selects last
#' # sorted value, but warns there are multiple modes
#' Mode(c("Tom","Dick","Harry"),multi=TRUE,warn=TRUE) # multi==TRUE so
#' # warning is negated
Mode <- function(x,multi=F,warn=F) {  
  ## mode function that should work for vectors or arrays of numeric, character, factor
  dim(x) <- NULL; x <- narm(x); tt <- 1
  if(length(x)>0) {
    ii <- table(x); ii <- sort(ii); ll <- length(ii)
    if(length(unique(as.numeric(ii)))==1 & length(as.numeric(ii))>1) {
      if(multi) {
        tt <- length(ii)
      } else { if(warn) { warning("all values of x had equal frequency, returning greatest") } }
    } else {
      if(ll>1) {
        if(ii[ll]==ii[ll-1]) { 
          if(multi) {
            tt <- length(which(ii==ii[ll]))
          } else { if(warn) { warning("multiple values of x had the maximum frequency, returning greatest") } }
        }
      }
    }
    result <- tail(names(ii),tt)
    nresult <- suppressWarnings(as.numeric(result))
    if(all(!is.na(nresult))) { result <- nresult }
    return(result) 
  } else {
    warning("no valid values passed to Mode")
    return(NA)
  }
}




#' Create an index file for an R function file
#'
#' Create a html index for an R function file by looking for functions,
#'  add descriptions using comments directly next to the function()
#'  command. Note that if too much code other than well-formatted
#'  functions is in the file then the result is likely not to be
#'  a nicely formatted index.
#' 
#' @param fn an R file containing functions in standard R script
#' @param below whether to search for comment text below or above
#'  the function() calls
#' @param fn.out optional name for the output file, else will be 
#'  based on the name of the input file
#' @param skip.indent whether to skip functions that are indented,
#'  the assumption being they are functions within functions
#' @return creates an html file with name and description of each function
#' @export 
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
#' @examples
#' # not run:  rfile <- file.choose() # choose an R script file with functions
#' # not run:  out <- Rfile.index(rfile,fn.out="temp.htm")
#' # unlink("temp.htm") # run once you've inspected this file in a browser
Rfile.index <- function(fn,below=T,fn.out="out.htm", skip.indent=T) 
{
  # makes html index of each function in a large functions file
  #require(reader)
  grp <- function(what,ins) { grep(what,ins,fixed=T) }
  if(toupper(get.ext(fn))!="R") { warning("expecting 'fn' to be a *.R file, if not, expect failure") }
  #if(is.null(fn.out)) { fn.out <- cat.path(fn=fn,suf="index",ext="htm") }
  neg <- 1; if(!below) { neg <- -1 }
  if(file.exists(fn))  {
    fl <- readLines(fn)
    fn.lines <- unique(c(grp("<- function",fl),grp("<-function",fl)))
    if(skip.indent) { 
      indz <- which(substr(fl[fn.lines],1,1) %in% c(" ","\t"))
      if(length(indz)>0) { fn.lines <- fn.lines[-indz] }
    }
    fl <- rmv.spc(fl)
    indz2 <- grep("#",fl[fn.lines]); 
    indz3 <- grep("function",sapply(strsplit(fl[fn.lines][indz2],split="#"),"[",1))
    if(length(indz3)>0) {  indz2 <- indz2[-indz3] }
    if(length(indz2)>0) {  fn.lines <- fn.lines[-indz2] }
    nfn <- length(fn.lines)
    fn.list <- vector("list",nfn)
    if(nfn<1) { warning("no functions found in R file"); return(NULL) }
    for (kk in 1:nfn) {
      first.ln <- fl[fn.lines[kk]]
      n <- 1; while(substr(first.ln,n,n)!="<" & substr(first.ln,n,n)!=" ") { n <- n+1 }
      fn.nm <- substr(first.ln,1,n-1);# cat(fn.nm,"\n")
      names(fn.list)[kk] <- paste("<p></p><b>",fn.nm,"</b>",sep=""); descr <- c()
      lnn <- fn.lines[kk]; 
      if(below) { while(length(grp("{",fl[lnn]))==0) { lnn <- lnn+neg } }
      #print(fl[lnn])
      lnn <- lnn+neg ; 
      while(length(grp("#",fl[lnn]))>0) { 
        descr <- c(descr,gsub("#","",gsub("#'","",fl[lnn],fixed=T),fixed=T))
        lnn <- lnn+neg 
      }
      if(!below) { descr <- rev(descr) }
      # remove lines after @ characters (for roxygen)
      roxy <- grep("@",descr); if(length(roxy)>0) { 
        descr <- descr[-c(min(roxy):length(descr))] }
      fn.list[[kk]] <- rmv.spc(paste(descr))
    }
  } else {
    warning("could not find function file to index")
    return(NULL)
  }
  fil.cont <- sapply(fn.list,paste,collapse="\n")
  #return(fil.cont)
  write.table(fil.cont,file=fn.out,quote=F,col.names=F)
  return(fn.list)
}




#' INTERNAL: Get the file extension from a file-name.
#'
#' @param fn filename(s) (with full path is ok too)
#' @return returns the (usually) 3 character file extension of a filename
#' @author Nicholas Cooper \email{nick.cooper@@cimr.cam.ac.uk}
get.ext <- function(fn) {
  # get file extension from a filename character string
  if(length(fn)<1) { warning("fn had length of zero"); return(fn) }
  if(all(is.na(fn)) | !is.character(fn)) { stop("fn should not be NA and should be of type character()") }
  strip.file.frags <- function(X) {
    file.segs <- strsplit(X,".",fixed=T)[[1]]
    lss <- length(file.segs)
    if (lss>1) { out <- paste(file.segs[lss]) } else { out <- "" }
    return(out)
  }
  return(sapply(fn,strip.file.frags))
}


