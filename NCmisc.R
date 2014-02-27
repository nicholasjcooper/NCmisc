###NAMESPACE ADDITIONS###
# Depends: R (>= 2.10), grDevices, graphics, stats, utils
# Imports: tools, proftools
# Suggests:
# importFrom(proftools, readProfileData, flatProfile)
# importFrom(tools, toHTML)
# import(grDevices, graphics, stats, utils)
###END NAMESPACE###

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


#internal
head2 <- function(X,...) { if(length(dim(X))==2) { prv.large(X,...,warn=F) } else { print(utils::head(X,...)) } }



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
headl <- function (x, n = 6, skip = 20, skip2 = 10, ind = "", ind2 = "  ") 
{
  if (!is.list(x)) {
    warning("not a list")
    return(NULL)
  }
  ll <- length(x)
  if (ll > 0) {
    ind.new <- paste(ind, ind2, sep = "")
    if (ll > skip) {
      ll <- skip
      skipped <- T
    }
    else {
      skipped <- F
    }
    for (cc in 1:ll) {
      if (!is.null(names(x))) {
        cat(ind, "$", names(x)[cc], ":\n", sep = "")
      }
      else {
        (cat("[[", cc, "]]\n", sep = ""))
      }
      if (is(x[[cc]], "list")) {
        headl(x[[cc]], n, ind = ind.new, ind2 = ind2, 
              skip = skip2, skip2 = skip2)
      }
      else {
        cat(ind, sep = "")
        head2(x[[cc]], n)
      }
    }
    if (skipped) {
      cat(ind, "... skipped ", length(x) - skip, " ...\n", 
          sep = "")
    }
  }
  else {
    cat(ind, "<empty>", "\n", sep = "")
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
rmv.spc <- function(str,before=TRUE,after=TRUE,char=" ") {
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
#' wait(.25,silent=FALSE) # wait 0.25 seconds
#' wait(0.005, "m")
#' wait(0.0001, "Hours", silent=FALSE)
wait <- function(dur,unit="s",silent=TRUE) {
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
#' # this function writes and removes a temporary file
#' # run only if ok to do this in your working directory
#' #not run# timeit(wait(0.1,"s") ,total.time=TRUE)
#' #not run# timeit(wait(0.1,"s") ,total.time=FALSE)
timeit <- function(expr,suppressResult=F,total.time=TRUE) {
  # function to measure in detail which function calls take the most time
  # during the evaluation of an expression. NB: will error with use of a trivial/instant expression
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
#'   wait(.05) # just so this trivial loop doesn't finish so quickly
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
must.use.package <- function(pcknms,bioC=FALSE,ask=FALSE,reload=FALSE,avail=FALSE,quietly=FALSE)  
{
  ## like 'base::library()' but can easily work for bioconductor packages, and will
  # automatically attempt to install any required packages not found
  # reload is for when there might a conflict, so will detach and reload packages 
  # to force their version of a function with a duplicate name
  if(!bioC) { 
    repos <- getOption("repos")
    if(any(repos=="@CRAN@")) { repos <- "http://cran.ma.imperial.ac.uk/" }
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
    checklib <- function(package,character.only=FALSE,warn.conflicts=TRUE,quietly=FALSE) { 
      do.call("require",args=list(package=package,character.only=character.only,
                             warn.conflicts=warn.conflicts,quietly=quietly)) 
    }
    got1 <- suppressWarnings(checklib(nxt.pck,character.only=TRUE,warn.conflicts=FALSE))
    if(!got1) {
      if(ask) {
        # ask whether to install a package
        ans <- select.list(c("yes","no"),"yes",FALSE,paste("ok to install",nxt.pck," (required)?"))
      } else { 
        ans <- "yes" 
      }
      if(ans=="yes") {
        if(bioC) {
          biocLite <- function(x) { print("please load biocLite function from http://bioconductor.org/biocLite.R") }
          source("http://bioconductor.org/biocLite.R") # biocLite() should now be replaced
          biocLite(nxt.pck)
          suppressWarnings(checklib(nxt.pck,character.only=TRUE,warn.conflicts=FALSE,quietly=quietly))
        } else {
          install.packages(nxt.pck,repos=repos,dependencies=TRUE); 
          suppressWarnings(checklib(nxt.pck,character.only=TRUE,warn.conflicts=FALSE,quietly=quietly)) 
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
#' repos <- "http://cran.ma.imperial.ac.uk/" # OR: repos <- getOption("repos")
#' search.cran("useful",repos)
#' search.cran(c("hmm","markov","hidden"),repos=repos)
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
Mode <- function(x,multi=FALSE,warn=FALSE) {  
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
Rfile.index <- function(fn,below=TRUE,fn.out="out.htm", skip.indent=TRUE) 
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




#' Tidy display function for matrix objects
#'
#' This function prints the first and last columns and rows of a matrix, and
#' more, if desired. Allows previewing of a matrix without 
#' overloading the console. Most useful when data has row and column names.
#'
#' @param largeMat a matrix
#' @param rows number of rows to display
#' @param cols number of columns to display
#' @param digits number of digits to display for numeric data
#' @param rL row label to describe the row names/numbers, e.g, row number, ID, etc
#' @param rlab label to describe the data rows
#' @param clab label to describe the data columns
#' @param rownums logical, whether to display rownumbers or ignore them
#' @param ret logical, whether to return the result as a formatted object, or just print to console
#' @param warn logical, whether to warn if the object type is not supported
#' @export
#' @examples
#' mat <- matrix(rnorm(1000),nrow=50)
#' rownames(mat) <- paste("ID",1:50,sep="")
#' colnames(mat) <- paste("Var",1:20,sep="")
#' prv.large(mat)
#' prv.large(mat,rows=9,cols=4,digits=1,rlab="samples",clab="variables",rownums=FALSE)
prv.large <- function(largeMat,rows=3,cols=2,digits=4,rL="Row#",
                      rlab="rownames",clab="colnames",rownums=T,ret=FALSE,warn=TRUE) 
{
  # nicely print a large matrix without overloading the output space
  # can return result as lines of text instead of printing to screen (for printing to file)
  # allows customization of row and column labels
  # only worth using with data that has row/col names
  # DEFINE INTERNAL FUNCTIONS #
  pad <- function(X,L) { X<-paste(X); if(is.character(X)) { paste(spc(L-nchar(X)),X,sep="") } else { stop(X) } }
  RND <- function(X,...) { if (is.numeric(X)) { round(X,...) } else { X }}
  #
  if(packages.loaded("bigmemory",cran.check=F)) { TF <- !do.call("is.big.matrix",args=list(largeMat)) } else { TF <- TRUE }
  if(!is.data.frame(largeMat) & !is.matrix(largeMat) & TF ) { 
    if(warn) { warning("unsupported object type, using 'head'") }
    print(head(largeMat))
    return()
  }
  if(length(dim(largeMat))!=2) { stop("expected largeMat to have 2 dimensions") }
  nC <- ncol(largeMat); nR <- nrow(largeMat); 
  if(nC<2 | nR<3) { 
    if(warn) {
      warning("prv.large only works for matrices with dims >= c(3,2), passed to print(head())")
    }
    print(head(largeMat,rows+1)); return(NULL) 
  }
  rows <- min(max(1,rows),nR); cols <- min(max(1,cols),nC)
  cN <- colnames(largeMat); rN <- rownames(largeMat)
  if(is.null(cN)) { cN <- paste(1:ncol(largeMat)); clab <- "col#" }
  if(is.null(rN)) { rN <- paste(1:nrow(largeMat)); rlab <- "row#"; rownums=F }
  rD <- spc(min(2,max(nchar(paste(nR)))),".")
  rnD <- spc(min(4,max(nchar(rN[c(1:rows,nR)]))),".")
  linez <- vector("list",rows+3) #row,col =number of rows,cols to print
  rown <- max(nchar(paste(nR)),nchar(rL))*as.numeric(rownums)
  hdr <- (nchar(cN[c(1:cols,nC)]))
  if(is.numeric(largeMat[1,])) {
    ## assess what the largest numbers are likely to be to adjust header spacing if necessary
    long.nums <- max(max(abs(largeMat[1,]),na.rm=T),max(abs(largeMat[,1]),na.rm=T))
    max.before.dp <- nchar(round(long.nums))+3
  } else { max.before.dp <- 6 }
  hdr[hdr<7] <- 7; hdr[hdr<(digits+max.before.dp)] <- (digits+max.before.dp)
  idln <- max(nchar(rlab),nchar(rN[c(1:rows,nR)]))
  long.text.filt <- T
  if(long.text.filt) {
    # look ahead for lengths of textdata > nchar(colnames()) which wouldn't get picked up elsewhere
    dat.cols <- c(1:cols,nC); varln <- a1 <- b1 <- numeric(length(dat.cols))
    for(cc in 1:length(dat.cols)) {
      a1[cc] <- nchar(cN[dat.cols[cc]]); b1[cc] <- max(nchar(paste(RND(largeMat[c(1:rows,nR),dat.cols[cc]],digits))))
      varln[cc] <- max(a1[cc],b1[cc])
    }
    #prv(a1,b1,dat.cols,hdr,varln)
    hdr[varln>hdr] <- varln[varln>hdr]
  }
  if(!ret) { cat("\n"); cat(spc(rown),spc(idln),clab,"\n") }
  dotz <- "  ...  "; dotzh <- " ..... "; dotzn <- "..."
  # make adjustments if matrix is small enough to display all rows/cols
  if(nC<=cols) { dotz <- dotzh <- "" ; cols <- cols-1 }
  if(nR<=rows) { lstln <- 1 } else {  lstln <- 3 }
  ## make adjustments if not displaying rownumbers
  if(!rownums) {
    lstR <- "" ; rD <- ""; jstr <- rep("",times=rows); rL=""
  } else {
    lstR <- nR; jstr <- paste(1:rows)
  }
  linez[[1]] <- c(pad(rL,rown),pad(rlab,idln),pad(cN[c(1:cols)],hdr[1:cols]),
                  dotzh,pad(cN[nC],tail(hdr,1)))
  for (j in 1:rows) { 
    linez[[j+1]] <- c(pad(jstr[j],rown),pad(rN[j],idln),
                      pad(RND(unlist(largeMat[j,1:cols]),digits),hdr[1:cols]),dotz,
                      pad(RND(largeMat[j,nC],digits),tail(hdr,1)))
  }
  linez[[rows+2]] <- c(pad(rD,rown),pad(rnD,idln),pad(rep(dotzn,times=cols),
                                                      hdr[1:cols]),dotz,pad(dotzn,tail(hdr,1)))
  linez[[rows+3]] <- c(pad(lstR,rown),pad(rN[nR],idln),
                       pad(RND(unlist(largeMat[nR,1:cols]),digits),hdr[1:cols]),
                       dotz,pad(RND(largeMat[nR,nC],digits),tail(hdr,1)))
  if(!ret) {
    for (j in 1:(rows+lstln)) {
      cat(paste(linez[[j]],collapse=" "),"\n")
    }
  } else {
    # remove last two lines if all rows are displayed
    if(lstln==1) { for(ii in 1:2) { linez[[length(linez)]] <- NULL }  }
    return(linez)
  }
}




#' Convert objects as arguments to object names
#' 
#' Equivalent to the base function substitute() but can do any length of arguments instead
#' of just one. Converts the objects in parentheses into text arguments as if they
#' had been entered with double quote strings. The objects must exist and be accessible in
#' the environment the function is called from for the function to work (same as for substitute()).
#' One application for this is to be able to create functions where object arguments can be
#' entered without quotation marks (simpler), or where you want to use the name of the object
#' as well as the data in the object.
#'
#' @param x compulsory, simply the first object in the list, no difference to any further objects
#' @param ... any further objects to return string names for.
#' @return character list of x,... object names
#' @export
#' @seealso \code{\link{prv}}, \code{\link{preview}} 
#' @author Nicholas Cooper 
#' @examples
#' myvar <- list(test=c(1,2,3)); var2 <- "testme"; var3 <- 10:14
#' print(myvar)
#' # single variable case, equivalent to base::substitute()
#' print(substitute(myvar))
#' print(Substitute(myvar))
#' # multi variable case, substitute won't work
#' Substitute(myvar,var2,var3)
#' # prv() is a wrapper for preview() allowing arguments without parentheses
#' # which is achieved internally by passing the arguments to Substitute()
#' preview(c("myvar","var2","var3"))
#' prv(myvar,var2,var3)
Substitute <- function(x=NULL,...) {
  varlist <- list(...); out <- character(1)
  if(length(varlist)>0) { 
    extr <- Substitute(...)
  } else {
    extr <- NULL
  }
  if(!is.null(x)) { out[1] <- paste(substitute(x)) }
  out <- c(out,extr)
  return(out[out!=""])
}


#' Output variable states within functions/loops during testing/debugging
#'
#' Same as preview but no labels command, and input is without quotes
#' and should be plain variable names of existing variables (no indices, args, etc)
#' A versatile function to compactly display most common R objects. Will
#' return the object name, type, dimension, and a compact representation of
#' object contents, for instance using prv.large() to display matrices,
#' so as to not overload the console for large objects. Useful for debugging,
#' can be placed inside loops and functions to track values, dimensions, and data types.
#' Particularly when debugging complex code, the automatic display of the variable name
#' prevents confusion versus using regular print statements.
#' By listing variables to track as character(), provides 'cat()' output 
#' of compact and informative variable state information, e.g, variable name, value,
#' datatype and dimension. Can also specify array or list elements, or custom labels.
#' prv() is the same as preview() except it can take objects without using double quotes
#' and has no 'labels' command (and doesn't need one). If expressions are entered rather
#' than variable names, then prv() will attempt to pass the arguments to preview().
#' @param ... series of variable(s) to report, separated by commas, which will trigger
#'  automatic labelling of the variable name
#' @param counts a list of array index values; so if calling during a counting loop, the
#'  value can be reported each iteration, also printing the count index; if the list is
#'  named the name will also appear, e.g, variable[count=1]. This list must be the same
#'  length as the variable list ... , and each element [[i]] must contain as many values
#'  as the original corresponding variable list[i] has dimensions
#' @seealso \code{\link{Dim}}
#' @export
#' @examples
#' # create variables of different types to show output styles #
#' testvar1 <- 193
#' testvar2 <- "Atol"
#' testvar3 <- c(1:10)
#' testvar4 <- matrix(rnorm(100),nrow=25)
#' testvar5 <- list(first="test",second=testvar4,third=100:110)
#' preview("testvar1"); prv(testvar1)
#' prv(testvar1,testvar2,testvar3,testvar4)
#' prv(matrix(rnorm(100),nrow=25)) # expression sent to preview() with no label
#' prv(193) # fails as there are no object names involved
prv <- function(...,counts=NULL) {
  options(warn=2)
  txt <- (tryCatch(varlist <- Substitute(...), error = function(e) e))
  options(warn=0)
  if(is(txt)[1]=="simpleError") { 
    #warning("not a function name")
    varlist <- list(...)
    sapply(varlist,preview)
    return(NULL)
  }
  return(preview(varlist,labels=NULL,counts=counts))
}


# internal function for print.large
display.var <- function(val,label,cnts=NULL) {
  if(is(cnts)[1]=="list") {
    ## if vars to debug have a counter, update the value and label with count(s)
    if(is(val)[1]=="list") { 
      for (dd in 1:length(cnts)) {
        val <- val[[ cnts[[dd]] ]] 
        if(!is.null(names(cnts))) { 
          label <- paste(label,"[[",names(cnts)[dd],"=",cnts[[dd]],"]]",sep="") 
        } else {
          label <- paste(label,"[[",cnts[[dd]],"]]",sep="")
        }
      }
    } else {
      #val <- val[cnts[[dd]] ]
      #preview(c("val","cnts"))
      if(length(Dim(val))!=length(cnts)) {
        val <- val ; warning("counts did not match dimensions")
      } else {
        arg.list <- vector("list",1+length(cnts)); arg.list[[1]] <- val
        arg.list[2:(1+length(cnts))] <- cnts
        val <- do.call("[",args=arg.list)
        if(!is.null(names(cnts))) { 
          label <- paste(label,"[",
                         paste(paste(names(cnts),"=",cnts,sep=""),collapse=","),"]",sep="") 
        } else {
          label <- paste(label,"[",paste(cnts,collapse=","),"]",sep="")
        }
      }
    }
  } else {
    #counts not a list
  }
  ## display appropriately according to datatype ##
  typ <- is(val)[1]
  if(is.function(val)) {
    cat(label,": function",sep=""); return(invisible())
  }
  if(packages.loaded("bigmemory",cran.check=F)) {
    if(typ=="big.matrix") {
      if(exists("prv.big.matrix",mode="function")) {
        do.call("prv.big.matrix",args=list(val,name=label))
      } else {
        warning("preview() needs the package bigmisc to display a big.matrix object")
      }
      return(invisible())
    }
  }
  dv <- Dim(val)
  if(is.numeric(dv)) { if(all(dv==1)) {
    cat(label,": ",val," (",typ,", ",paste(Dim(val),collapse="*"),")",sep=""); return(invisible())
  } }
  if(is(val)[1]=="list") {
    cat(label," (",typ,", ",paste(Dim(val),collapse="*"),")\n",sep=""); print(headl(val)); return(invisible())
  } else {
    #print(Dim(val))
    if(!is.null(dim(val))) {
      cat(label," (",typ,", ",paste(Dim(val),collapse="*"),")\n",sep="");
      if(length(dim(val))==2) {
        if(ncol(val)>=2 & nrow(val)>=3) {
          prv.large(val,warn=F)
        } else {
          print(head(val))
          if(nrow(val)>6) {
            # if any part not displayed, then indicate using ...
            cat(if(!is.null(rownames(val))) { "  ...    " } else { "" },rep("  ..  ",ncol(val)),"\n")
          }
        }
      } else {
        print(c(head(val),if(length(val)>6) { (" ...") } else { NULL }))  # e.g, for a table
      }
      return(invisible())
    } else {
      cat(label," (",typ,", ",paste(Dim(val),collapse="*"),") [head]:\n",sep="")
      print(head(val))
      return(invisible())
    }
  }
}


#' Output variable states within functions during testing/debugging
#'
#' A versatile function to compactly display most common R objects. Will
#' return the object name, type, dimension, and a compact representation of
#' object contents, for instance using prv.large() to display matrices,
#' so as to not overload the console for large objects. Useful for debugging,
#' can be placed inside loops and functions to track values, dimensions, and data types.
#' Particularly when debugging complex code, the automatic display of the variable name
#' prevents confusion versus using regular print statements.
#' By listing variables to track as character(), provides 'cat()' output 
#' of compact and informative variable state information, e.g, variable name, value,
#' datatype and dimension. Can also specify array or list elements, or custom labels.
#' prv() is the same as preview() except it can take objects without using double quotes
#' and has no 'labels' command (and doesn't need one).
#' @param varlist character vector, the list of variable(s) to report, which will trigger
#'  automatic labelling of the variable name, otherwise if entered as the variable value (ie.
#'  without quotes, then will by default be displayed as 'unknown variable')
#' @param labels, will label 'unknown variables' (see above) if entered as variables without quotes
#' @param counts a list of array index values; so if calling during a counting loop, the
#'  value can be reported each iteration, also printing the count index; if the list is
#'  named the name will also appear, e.g, variable[count=1]. This list must be the same
#'  length as varlist (and labels if not NULL), and each element [[i]] must contain as many values
#'  as the original corresponding varlist[i] has dimensions. The dimensions must result in a 1x1 scalar
#' @param assume.char usually 'varlist' is a character vector of variable names, but in the case
#'  that it is actually a character variable, using assume.char=TRUE will ensure that it will be assumed
#'  the character variable is the object to preview, rather than a list of variable names. So long
#'  as none of the values are found to be variable names in the global environment. preview() can
#'  also find variables in local environments, and if this is where the target variable lies, it is
#'  best to use assume.char=FALSE, otherwise the search for alternative environments might not happen.
#'  Note that in most cases the automatic detection of the input should understand what you want, regardless
#'  of the value of assume.char.
#' @seealso \code{\link{Dim}} 
#' @export
#' @examples
#' # create variables of different types to show output styles #
#' testvar1 <- 193
#' testvar2 <- "Atol"
#' testvar3 <- c(1:10)
#' testvar4 <- matrix(rnorm(100),nrow=25)
#' testvar5 <- list(first="test",second=testvar4,third=100:110)
#' preview("testvar1")
#' preview("testvar4")
#' preview(paste("testvar",1:5,sep=""))
#' preview(testvar1,"myvarname")
#' preview(testvar1)
#' # examples with loops and multiple dimensions / lists
#' for (cc in 1:4) {
#'  for (dd in 1:4) { preview("testvar4",counts=list(cc,dd)) }}
#'
#' for (dd in 1:3) { preview("testvar5",counts=list(dd=dd)) }
preview <- function(varlist,labels=NULL,counts=NULL,assume.char=FALSE) {
  ## for debugging, simplify code to print vars you are checking
  lab <- varlist
  if(is.character(varlist) & (length(labels)<length(varlist))) {
    if(assume.char | length(varlist)>10) {
      if(!any(varlist %in% ls())) {
        unknown.variable <- varlist
        preview("unknown.variable",labels=labels,counts=counts)
        return(invisible(NULL))
      }
    }
  } 
  if(is.character(varlist)) {
    t1 <- grep("[",varlist,fixed=T)
    t2 <- grep("(",varlist,fixed=T)
    if(length(t1)>0 | length(t2)>0) {
      warning("preview() only works with plain variable names, cannot use an index or function",
              "call containing ['s or ('s. To access object indices use the 'counts' argument")    
      return()
    }
  }
  # test whether 'counts' sublists are all of the same length as varlist, else ignore 'counts'
  if(is.list(counts)) {  if(!all(sapply(counts,length)==length(varlist))) { 
    counts <- NULL } } else { if(length(counts)==length(varlist)) { counts <- list(counts) } else { counts <- NULL } }
  #val <- vector("list",length(lab))
  
  ## if data not entered with a label, or as a string (not including prv() converted calls)
  if(!is.character(varlist) | !is.null(labels)) {
    if(is.null(labels) | ((length(labels)!=1) & (length(varlist)!=length(labels)))) {
      display.var(varlist,"unknown variable"); cat("\n")
    } else { 
      for(cc in 1:length(labels)){
        if(is.list(counts)) { cnts <- lapply(counts,"[",cc) } else { cnts <- NULL }
        if(is.list(varlist)) {
          display.var(varlist[[cc]],labels[cc],cnts=cnts)
        } else {
          display.var(varlist[cc],labels[cc],cnts=cnts)
        }
        cat("\n") 
      }
      return(invisible())
    }
    return(invisible())
  }
  ENVIR <- parent.frame()
  for(cc in 1:length(lab)) {
    label <- lab[cc]
    #print(sys.parent())
    #print(sys.nframe())
    #print(sys.frame(-1))#
    mymode <- "any"
    if(exists(label,mode="function")) { if(exists.not.function(label)) { 
      mymode <- exists.not.function(label,T) } } # if object is also a function, what type is the other type?
    #if(mymode=="") { mymode <- "any" }
    val <- NULL
    try(val <- get(label,envir=ENVIR, mode=mymode),silent=T)
    sf <- sys.frames(); cc <- 1
    while(is.null(val) & cc<=length(sf)) { (try(val <- get(label,envir=sf[[cc]],mode=mymode),silent=T)); cc <- cc + 1 }
    if(!is.null(val)) {
      if(is.list(counts)) { cnts <- lapply(counts,"[",cc) } else { cnts <- NULL }
      display.var(val,label,cnts=cnts)
      cat("\n") 
    } else {
      cat("preview() couldn't find variable '",label,"'\n",sep="")
    }
  }
  return(invisible())
}




#' Simulate a dataset with correlated measures
#'
#' Simulate a dataset with correlated measures (normal simulation with e.g, rnorm() usually
#'  only gives small randomly distributed correlations between variables). This is a quick
#'  and unsophisticated method, but should be able to provide a dataset with slightly more
#'  realistic structure than simple rnorm() type functions. Varying the last three parameters
#'  gives some control on the way the data is generated. It starts with a seed random variable,
#'  then creates 'k' random variables with an expected correlation of r=genr() with that seed 
#'  variable. Then after this, one of the variables in the set (including the seed) is randomly
#'  selected to run through the same process of generating 'k' new variables; this is repeated
#'  until columns are full up. 'mix.order' then randomizes the column order destroying the
#'  relationship between column number and correlation structure, although in some cases,
#'  such relationships might be desired as representative of some real life datasets. 
#' @param nrow integer, number of rows to simulate
#' @param ncol integer, number of columns to simulate
#' @param genx the generating function for data, e.g rnorm(), runif(), etc
#' @param genr the generating function for desired correlation, e.g, runif()
#' @param k number of steps generating from the same seed before choosing a new seed
#' @param mix.order whether to randomize the column order after simulating
#' @export
#' @seealso \code{\link{cor.with}}
#' @author Nicholas Cooper 
#' @examples
#' corDat <- sim.cor(200,5)
#' prv(corDat) # preview of simulated normal data with r uniformly varying
#' cor(corDat) # correlation matrix
#' corDat <- sim.cor(500,4,genx=runif,genr=function(x) { 0.5 },mix.order=FALSE)
#' prv(corDat) # preview of simulated uniform data with r fixed at 0.5
#' cor(corDat) # correlation matrix
sim.cor <- function(nrow=100,ncol=100,genx=rnorm,genr=runif,k=3,mix.order=TRUE) {
  #ncol <- 100
  #nrow <- 100
  new.mat <- matrix(numeric(ncol*nrow),nrow=nrow)
  X <- genx(nrow)
  new.mat[,1] <- X
  cnt <- 0
  for (cc in 2:ncol) {
    dd <- cor.with(X,r=genr(1))
    new.mat[,cc] <- dd
    cnt <- cnt+1
    if(cnt>=k) { X <- new.mat[,sample(cc,1)]; cnt <- 0 }
  }
  if(mix.order) {
    new.mat <- new.mat[,sample(ncol(new.mat))]
  }
  return(new.mat)
}


#' Simulate a correlated variable
#'
#' Simulate a variable correlated at level 'r' with cector x (of the same length). Can
#' either 'preserve' the mean and standard-deviation, leave standardizeed, 
#' or select new mean 'mn' and standard deviation 'st'.
#' @param x existing variable, to which you want to simulate a new correlated variable
#' @param r the 'expected' correlation you want to target (randomness 
#'  will mean that the actual correlation will vary around this value)
#' @param preserve logical, whether to preserve the same mean and standard deviation(SD)
#'  as x, for the new variable
#' @param mn optional, set the mean for the new simulated variable [must also set st if using this]
#' @param st optional, set the SD for the new simulated variable [must also set mn if using this]
#' @return return the new variable with an expected correlation of 'r' with x
#' @references http://www.uvm.edu/~dhowell/StatPages/More_Stuff/CorrGen.html
#' @export
#' @seealso \code{\link{sim.cor}}
#' @author Nicholas Cooper 
#' @examples
#' X <- rnorm(10,100,14)
#' cor.with(X,r=.5) # create a variable correlated .5 with X
#' cor(X,cor.with(X)) # check the actual correlation
#' # some variability in the actual correlation, so run 1000 times:
#' print(mean(replicate(1000,{cor(X,cor.with(X))})))
#' cor.with(X,preserve=TRUE) # preserve original mean and standard deviation
#' X[c(4,10)] <- NA # works fine with NAs, but new var will have same missing
#' cor.with(X,mn=50,st=2) # specify new mean and standard deviation
cor.with <- function(x,r=.5,preserve=FALSE,mn=NA,st=NA) {
  # inspired by David C. Howell
  # http://www.uvm.edu/~dhowell/StatPages/More_Stuff/CorrGen.html
  X <- standardize(x)
  L <- length(X)
  y <- rnorm(L)
  a <- r/(sqrt(1-(r^2)))
  Z = a*X + y
  z <- standardize(Z)
  if(preserve) {
    mn <- mean(x,na.rm=T)
    st <- sd(x,na.rm=T)
  }
  if(preserve | (!is.na(mn) & !is.na(st))) {
    z <- (z*st)+mn
  }
  return(z)
}




#' Summarise the dimensions and type of available R example datasets
#' 
#' This function will parse the current workspace to see what R datasets
#' are available. Using the toHTML function from the tools package to interpret
#' the data() call, each dataset is examined in turn for type and dimensionality.
#' Can also use a filter for dataset types, to only show, for instance, matrix 
#' datasets. Also you can specify whether to only look for base datasets, or to
#' search for datasets in all available packages. Result is a printout to the
#' console of the available datasets and their characteristics.
#'
#' @param filter logical, whether to filter datasets by 'types'
#' @param types if filter=TRUE, which data types to include in the result
#' @param all logical, if all=TRUE, look for datasets in all available packages, else just base
#' @param ... if all is false, further arguments to the data() function to search datasets
#' @export
#' @author Nicholas Cooper 
#' @examples
#' summarise.r.datasets()
#' summarise.r.datasets(filter=TRUE,"matrix")
## create a summary of R datasets you could use
summarise.r.datasets <- function(filter=FALSE,types=c("data.frame","matrix"),all=FALSE,...) { 
  # eg., package = .packages(all.available = TRUE)
  if(all) {
    ll <- unlist(strsplit((toHTML(data(package = .packages(all.available = TRUE), envir = environment()))),"\n"))
  } else {
    ll <- unlist(strsplit((toHTML(data(..., envir = environment()))),"\n"))
  }
  ll <- ll[-grep("<",ll,fixed=T)]
  datasets <- ll[-grep(" ",ll,fixed=T)]
  
  for (cc in 1:length(datasets)) { 
    if(exists(datasets[cc])) {
      dd <- NULL
      try(dd <- get(datasets[cc]))
      if(is.null(dd)) { ddd <- ""; iz <- "" } else { ddd <- Dim(dd); iz <- is(dd)[1] }
      if(filter) { if(any(types %in% is(dd))) { disp <- T } else { disp <- F } } else { disp <- T }
      if(disp) {
        cat(paste(datasets[cc])," [",paste(ddd,collapse=","),"] (",iz,")\n",sep="")
      }
    }
  }
}



#' Does object exist ignoring functions
#'   
#' The exists() function can tell you whether an object exists
#' at all, or whether an object exists with a certain type, but
#' it can be useful to know whether an object exists as genuine 
#' data (and not a function) which can be important when a variable
#' or object is accidently or intentionally given the same name as
#' a function. This function usually returns a logical value as to
#' the existence of the object (ignoring functions) but can also
#' be set to return the non-function type if the object exists.
#' @param x the object name to search for
#' @param ret.type logical, if TRUE then will return the objects' type (if it exists) rather
#' than TRUE or FALSE. If the object doesn't exist the empty string will be returned as the type.
#' @return logical, whether non-function object exists, or else the type if ret.type=TRUE
#' @export
#' @author Nicholas Cooper 
#' @examples
#' x <- "test"
#' # the standard exists function, for all modes, correct mode, and other modes:
#' exists("x")
#' exists("x",mode="character")
#' exists("x",mode="numeric")
#' # standard case for a non-function variable
#' exists.not.function("x",TRUE)
#' # compare results for a non-existent variable
#' exists("aVarNotSeen")
#' exists.not.function("aVarNotSeen")
#' # compare results for variable that is a function
#' exists("mean")
#' exists.not.function("mean")
#' # define a variable with same name as a function
#' mean <- 1.4
#' # exists.not.function returns the type of the variable ignoring the function of the same name
#' exists.not.function("mean",TRUE)
#' exists("mean",mode="function")
#' exists("mean",mode="numeric")
exists.not.function <- function(x,ret.type=FALSE) {
  if(!is.character(x)) {
    stop("x should be the name of an object [as character type]")
  }
  other.modes <- c("logical", "integer", "list", "double", "character", "raw", "complex", "NULL")
  ex <- F; type <- ""
  for(cc in 1:length(other.modes)) {
    if(exists(x,mode=other.modes[cc])) { ex <- T ; type <- other.modes[cc] }
  }
  if(ret.type) {
    return(type)
  } else {
    return(ex)
  }
}


#' A more general dimension function
#'
#' A more general 'dim' function. For arrays simply calls the dim() function, but for other data types, tries to
#' provide an equivalent, for instance will call length(x) for vectors, and will
#' recursively report dims for lists, and will attempt something sensible for other datatypes.
#' 
#' @param x the object to find the dimension for
#' @param cat.lists logical, for lists, TRUE will concatenate the dimesions to a single string,
#'  or FALSE will return the sizes as a list of the same structure as the original.
#' @seealso \code{\link{prv}}, \code{\link{preview}}
#' @return dimension(s) of the object
#' @export
#' @examples
#' # create variables of different types to show output styles #
#' Dim(193)
#' Dim(1:10)
#' testvar <- matrix(rnorm(100),nrow=25)
#' Dim(matrix(rnorm(100),nrow=25))
#' Dim(list(first="test",second=testvar,third=100:110))
#' Dim(list(first="test",second=testvar,third=100:110),FALSE)
Dim <- function(x,cat.lists=TRUE) {
  max.dims <- 100
  rez <- NULL
  try(rez <- dim(x))
  if(!is.null(rez)) { return(dim(x)) }
  if(is(x)[1]=="list") { 
    out <- lapply(x,Dim) 
    if(cat.lists) {
      if(length(out)>max.dims) { suf <- paste("... + ",length(out)-max.dims," more",sep="") } else { suf <- "" }
      out <- paste(out[1:min(max.dims,length(out))],collapse="; ")
      out <- paste(out,suf)
    }
  } else { out <- length(x) }
  return(out)  
}


#' Force argument to be a numeric type with length one
#'
#' Sometimes arguments must be numeric, scalar and within a certain range.
#' Rather than using many if statements, this will do everything possible to 
#' coerce input to a scalar, failing that will replace with a default value.
#' Can also provide a maximum and minimum range that the result must lie within.
#' 
#' @param x the object to ensure is a scalar
#' @param default the value to revert to if the format of x is illegal
#' @param min a lower bound for the output, anything below this is set to min
#' @param max an upper bound for the output, anything above this is set to max
#' @seealso \code{\link{force.percentage}}
#' @return the object x if already legal, first element if a vector, the min or
#'  max value if x is outside the specified bounds, or the value of default otherwise
#' @export
#' @examples
#' force.scalar(1.5)
#' force.scalar(NULL,default=.5)
#' force.scalar(NA,default=.4,min=5,max=10) # default is outside range!
#' force.scalar(rnorm(1000))
#' force.scalar(101,max=50)
#' force.scalar(list(0.4,1,2,3,4,"test"))
#' force.scalar(data.frame(test=c(1,2,3),name=c("test","me","few")))
#' force.scalar(Inf)
force.scalar <- function(x,default=1, min=-10^10, max=10^10) {
  if(is.list(x)) { x <- unlist(x) }
  if(!is.numeric(default)) { default <- mean(c(min[1],max[1]),na.rm=T) ; warning("bad default, reverting to max,min mean") }
  if(length(Dim(default))!=1) { default <- mean(c(min[1],max[1]),na.rm=T) ; warning("bad default, reverting to max,min mean") }
  if(length(x)<1) { x <- default }
  if(length(Dim(x))!=1) { x <- default }
  x <- suppressWarnings(as.numeric(x[1]))
  if(!is.numeric(x)) { x <- default }
  if(is.na(x)) { x <- default }
  if(x<min) { x <- min }
  if(x>max) { x <- max }
  return(x)
}


#' Force argument to be a percentage with length one
#'
#' Sometimes it is nice to be able to take a percentage as an argument and not
#' have to specify whether it should be entered as a number between 0 and 100, 
#' e.g, 50 = 50%, or as a decimal between 0 and 1, e.g, 0.5 = 50%. Anything greater
#' than 1 and less than 100 will be divided by 100. Anything outside 0,100 will be
#' set to 0,100 respectively.
#' 
#' @param x the object to ensure is a oercentage
#' @param default the value to revert to if the format of x is illegal
#' @seealso \code{\link{force.scalar}}
#' @return the object x if already legal, first element if a vector, the min or
#'  max value if x is outside the specified bounds, or the value of default otherwise
#' @export
#' @examples
#' # create variables of different types to show output styles #
#' force.percentage(45)
#' force.percentage(450)
#' force.percentage(.45)
#' force.percentage(-45)
#' force.percentage("twenty")
#' force.percentage(NA,default=0.25)
force.percentage <- function(x,default=.5) {
  x <- force.scalar(x,default=default, min=0,max=100)
  while(x>1) { x <- x/100 }
  return(x)
}


#' Create fake text for testing purposes
#' 
#' Returns randomized input as if reading lines from a file, like 'readLines()'
#' Can be used to test i/o functions, robustness.
#' 
#' @param max.lines maxmimum number of fake lines to read
#' @param max.chars maximum number of characters per line
#' @param pc.space percentage of randomly generated characters that should be a delimiter
#' @param delim what should the simulated delimiter be, e.g, a space, comma etc. If you wish not
#'  to include such either set the delimiter as "", or set pc.space=0.
#' @param can.null whether with probability 1/max.lines to return NULL instead of any lines of text,
#'  which simulates an empty file, which for testing purposes you may want to be able to handle
#' @return a vector of character entries up 'max.chars' long, or sometimes only NULL if can.null=TRUE
#' @export
#' @author Nicholas Cooper
#' @examples
#' fakeLines() # should produce between zero and ten lines of random text, 35% of which are spaces
fakeLines <- function(max.lines=10,max.chars=100,pc.space=.35,delim=" ",can.null=TRUE) {
  all.char <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$%&*()_+-=;'#,./<>?:@~{}[]| "
  ncs <- nchar(all.char)
  pct <- force.percentage(pc.space,default=.35)
  if(pct<=0) { wantx <- 0 } else { wantx <- ncs/((1/pct)-1) }
  all.chars <- c(strsplit(all.char,"")[[1]],rep(delim,times=round(wantx)))
  nlines <- sample(max.lines,1)
  txt <- NULL
  if(!can.null | nlines!=max.lines){
    for(cc in 1:nlines) {
      nc <- sample(max.chars,1)
      lineind <- sample(length(all.chars),size=nc,replace=T)
      charz <- paste(all.chars[lineind],collapse="",sep="")
      txt <- c(txt,charz)
    }
  } else { txt <- c() }
  return(txt)
}



#' Monitor CPU, RAM and Processes
#' 
#' This function runs the unix 'top' command and returns the overall CPU and RAM usage,
#' and optionally the table of processes and resource use for each. Works only with
#' unix-based systems such as Mac OS X and Linux, where 'top' is installed. Default
#' is to return CPU and RAM overall stats, to get detailed stats instead, set Table=TRUE.
#'
#' @param CPU logical, whether to return overall CPU usage information
#' @param RAM logical, whether to return overall RAM usage information
#' @param Table logical, whether to return system information for separate processes. This
#'  is returned as table with all of the same columns as a command line 'top' command. If
#'  'Table=TRUE' is set, then the default becomes not to return the overall CPU/RAM usage stats.
#'  The dataframe returned will have been sorted by descending memory usage.
#' @param procs integer, if Table=TRUE, then the maximum number of processes to return (default 20)
#' @param mem.key character, default for Linux is 'mem' and for Mac OS X, 'physmem', but if the 'top'
#'  command on your system displays memory usage using a different label, then enter it here
#'  (case insensitive) to override defaults.
#' @param cpu.key character, default for Linux and Mac OS X is 'cpu', but if the top
#'  command on your system displays CPU usage using a different label, then enter it here.
#' @return a list containing CPU and RAM usage, or with alternate parameters can return stats for each process
#' @export
#' @author Nicholas Cooper
#' @examples
#' # not run #  top()
#' # not run #  top(Table=TRUE,proc=5)
top <- function(CPU=!Table,RAM=!Table,Table=FALSE,procs=20,mem.key=NULL,cpu.key=NULL) {
  if(!RAM & !CPU & !Table) { warning("Deselected all options, null will be returned"); return(NULL) }
  if(!check.linux.install("top")) {
    warning("'top' command only works on Mac OS X and Linux")
    return(NULL)
  }
  if(toupper(Sys.info()["sysname"])=="DARWIN") { macos <- T } else { macos <- F }
  if(macos) {
    # MAC OS X
    txt <- tryCatch(system("top -l 1",intern=T), error = function(e) e)
    if(length(txt)==0) { warning("command failed"); return(NULL) }
    if(!is.character(txt)) { warning("command failed"); return(NULL) }
    dtt <- divide.top.txt(txt)
    if(!is.list(dtt) | any(is.na(dtt)) ) { warning("unexpected output from top command"); return(NULL) }
    parz <- dtt$table; headr <- dtt$header
    if(!is.character(mem.key)) { mem.key <- "physmem" }
    if(RAM) { ram.gb.list <- suck.mem(headr,key=mem.key) }
  }
  if(!macos) {
    ## LINUX
    txt <- tryCatch(system("top -n 1 -b",intern=T), error = function(e) e)
    if(length(txt)==0) { warning("command failed"); return(NULL) }
    if(!is.character(txt)) { warning("command failed"); return(NULL) }
    dtt <- divide.top.txt(txt)
    if(!is.list(dtt) | any(is.na(dtt)) ) { warning("unexpected output from top command"); return(NULL) }
    parz <- dtt$table; headr <- dtt$header
    if(!is.character(mem.key)) { mem.key <- "mem" }
    if(RAM) { ram.gb.list <- suck.mem(headr,key=mem.key) }
  }
  if(!is.character(cpu.key)) { cpu.key <- "cpu" }
  if(CPU) { cpu.pc.list <- suck.cpu(headr,key=cpu.key) }
  if(Table) {
    tab <- make.top.tab(parz); if(all(is.null(tab))) { return(NULL) }
    mem.col <- grep("mem",colnames(tab),ignore.case=T)[1]
    if(is.na(mem.col)) { mem.col <- grep("RSIZE",colnames(tab),ignore.case=T)[1] }
    cpu.col <- grep("cpu",colnames(tab),ignore.case=T)[1]
    if(is.na(mem.col) | (is.na(mem.col))) {
      warning("did not find 'mem', 'RSIZE' or 'CPU' entries in 'top' output")
    } else {
      tab <- tab[rev(order(tab[,mem.col])),]
      tab <- tab[rev(order(tab[,cpu.col])),]
      tab <- tab[rev(order(tab[,mem.col])),]
    }
    if(is.na(as.numeric(procs))) { procs <- nrow(tab) } else { procs <- round(procs) }
    procs <- min(c(procs,nrow(tab)),na.rm=T)
    if(is.null(dim(tab))) { Table <- FALSE }
  }
  outlist <- NULL; outnms <- NULL
  if(CPU) { outlist <- c(outlist,list(cpu.pc.list)); outnms <- c(outnms,"CPU") }
  if(RAM) { outlist <- c(outlist,list(ram.gb.list)); outnms <- c(outnms,"RAM") }
  if(Table) { outlist <- c(outlist,list(tab[1:procs,])); outnms <- c(outnms,"Table") }
  names(outlist) <- outnms
  return(outlist)
}


# internal function to support top() function
make.top.tab <- function(parz) {
  if(!is.list(parz)) { warning("unexpected input"); return(NULL) }
  cnts <- sapply(parz,length)
  exp.lines <- Mode(cnts)
  shortz <- which(cnts<exp.lines)
  longz <- which(cnts>exp.lines)
  if(length(longz)>0) {  parz[longz] <- lapply(parz[longz],function(X) { X[1:exp.lines] }) }
  if(length(shortz)>0) { parz <- parz[-shortz] }
  LL <- length(parz[[1]]); if(LL < 1) { warning("unexpected input"); return(NULL) }
  df <- as.data.frame(matrix(ncol=LL,nrow=length(parz)))
  if(nrow(df)<1) { warning("unexpected input"); return(NULL)  }
  for(cc in 1:length(parz[[1]])) { df[,cc] <- sapply(parz,"[",cc) }
  if(nrow(df)<2) { warning("expected header row and at least 1 data row"); return(NULL)  }
  tab <- df[-1, ,drop=FALSE]; colnames(tab) <- df[1,]; rownames(tab) <- NULL
  return(tab)
}

# internal function to support top() function
divide.top.txt <- function(txt) {
  parz <- strsplit(txt," +|\t")
  parz <- lapply(parz,function(X) { X <- X[!is.na(X)] ; X[X!=""] } ) 
  headline <- which(sapply(parz,function(X) { all(c("PID","USER") %in% toupper(X)) }))
  if(length(headline)<1) { warning("expected PID and USER column - at least 1 not found"); return(NA) }
  parz <- parz[headline[1]:length(parz)]
  headr <- txt[1:(headline[1]-1)]
  return(list(header=headr,table=parz))
}

# internal function to support top() function
suck.num.from.txt <- function(txt) {
  if(is.na(txt)) { return(NA) }
  if(length(txt)<1) { return(NA) }
  splt <- strsplit(txt,"")
  nmall <- numeric()
  anm <- function(X) { suppressWarnings(as.numeric(X)) }
  for(cc in 1:length(splt)) {
    nm <- sapply(splt[[cc]],function(X) {
      if(!is.na(anm(X))) { anm(X) } else { if(X==".") { X } else { NA } } } )
    nmall[cc] <- anm(paste(narm(nm),collapse="",sep=""))
  }
  return(nmall)
}

# internal function to support top() function
suck.cpu <- function(headr,key="cpu") {
  cpz <- grep(key,headr,ignore.case=T)
  if(length(cpz)>0) {
    cpuline <- headr[cpz[1]]
    ms <- strsplit(cpuline,",")[[1]]
    ms <- gsub("cpu","",ms,ignore.case=T)
    user <- ms[grep("us",ms,ignore.case=T)]
    sys <- ms[grep("sy",ms,ignore.case=T)]
    idle <- ms[grep("id",ms,ignore.case=T)]
    if(length(user)>0) {
      user1 <- rmv.spc(gsub("us","",gsub("user","",user,ignore.case=T)))
      user.gb <- suck.num.from.txt(user1)
    } else { user.gb <- NA }
    if(length(sys)>0) {
      sys1 <- rmv.spc(gsub("sy","",gsub("sys","",sys,ignore.case=T)))
      sys.gb <- suck.num.from.txt(sys1)
    } else { sys.gb <- NA }
    if(length(idle)>0) {
      idle1 <- rmv.spc(gsub("id","",gsub("idle","",idle,ignore.case=T)))
      idle.gb <- suck.num.from.txt(idle1)
    } else { idle.gb <- NA }
    if(is.na(idle.gb) & !is.na(sys.gb) & !is.na(user.gb)) { idle.gb <- 100-user.gb-sys.gb }
    if(is.na(sys.gb) & !is.na(idle.gb) & !is.na(user.gb)) { sys.gb <- 100-user.gb-idle.gb }
    if(is.na(user.gb) & !is.na(sys.gb) & !is.na(idle.gb)) { user.gb <- 100-idle.gb-sys.gb }
  } else { 
    cat("no CPU usage information found\n")
    return(NULL)
  }
  return(list(total=user.gb,idle=idle.gb,sys=sys.gb,unit="%"))
}

# internal function to support top() function
suck.mem <- function(headr,key="Mem") {
  memz <- grep(key,headr,ignore.case=T)
  if(length(memz)>0) {
    memline <- headr[memz[1]]
    ms <- strsplit(memline,",")[[1]]
    ms <- gsub("mem","",ms,ignore.case=T)
    tot <- ms[grep("total",ms,ignore.case=T)]
    free <- ms[grep("free",ms,ignore.case=T)]
    used <- ms[grep("used",ms,ignore.case=T)]
    if(length(tot)>0) {
      tot1 <- rmv.spc(gsub("total","",tot,ignore.case=T))
      tot.gb <- suck.bytes(tot1)
    } else { tot.gb <- NA }
    if(length(free)>0) {
      free1 <- rmv.spc(gsub("free","",free,ignore.case=T))
      free.gb <- suck.bytes(free1)
    } else { free.gb <- NA }
    if(length(used)>0) {
      used1 <- rmv.spc(gsub("used","",used,ignore.case=T))
      used.gb <- suck.bytes(used1)
    } else { used.gb <- NA }
    if(is.na(used.gb) & !is.na(free.gb) & !is.na(tot.gb)) { used.gb <- tot.gb-free.gb }
    if(is.na(free.gb) & !is.na(used.gb) & !is.na(tot.gb)) { free.gb <- tot.gb-used.gb }
    if(is.na(tot.gb) & !is.na(free.gb) & !is.na(used.gb)) { tot.gb <- used.gb+free.gb }
  } else { 
    cat("no RAM usage information found\n")
    return(NULL)
  }
  return(list(total=tot.gb,used=used.gb,free=free.gb,unit="Gb"))
}

# internal function to support top() function  
suck.bytes <- function(tot1,GB=TRUE) {
  mult <- 0
  if(length(grep("k",tot1,ignore.case=T))>0) { mult <- 1000 }
  if(length(grep("m",tot1,ignore.case=T))>0) { mult <- 10^6 }
  if(length(grep("g",tot1,ignore.case=T))>0) { mult <- 10^9 }
  if(mult==0 & length(grep("b",tot1,ignore.case=T))>0) { mult <- 1 }
  if(mult==0) { warning("expected symbol indicating units, defaulting to bytes"); mult <- 1 }
  lst <- c("kb","gb","mb","b","g","m","k")
  tot1 <- suck.num.from.txt(tot1)
  if(is.na(tot1)) { tot1 <- 0 ; warning("no numbers found in text, setting to zero") }
  tot2 <- (as.numeric(tot1)*mult)/10^9 ; 
  if(!GB) { tot2 <- tot2/10^3 }
  return(tot2)
}




#' Check whether a set of packages has been loaded
#' 
#' Returns TRUE if the whole set of packages entered has been loaded, or FALSE
#' otherwise. This can be useful when developing a package where there is optional
#' functionality depending if another package is in use (but the other package is
#' not part of 'depends' because it is not essential). Because 'require' cannot
#' be used within functions submitted as part of a CRAN package.
#' @param pcks character, a package name, or vector of names, if left blank will return all loaded
#' @param ... further package names as character (same as entering via pcks, 
#'  but avoids need for c() in pcks)
#' @param cran.check logical, in the case at least one package is not found, whether
#'  to search CRAN and see whether the package(s) even exist on CRAN.
#' @param repos repository to use if package is not loaded and cran.check=TRUE,
#'  if NULL, will attempt to use the repository in getOptions("repos") or will
#'  default to the imperial.ac.uk mirror.
#' @return logical TRUE or FALSE whether the whole list of packages are available
#' @export
#' @author Nicholas Cooper 
#' @examples
#' repos <- "http://cran.ma.imperial.ac.uk/"
#' packages.loaded("NCmisc","reader",repos=repos)
#' packages.loaded(c("bigmisc","nonsenseFailTxt"),repos=repos)
#' packages.loaded(c("bigmisc","nonsenseFailTxt"),cran.check=FALSE)
#' packages.loaded() # no argument means all loaded packages are listed
packages.loaded <- function(pcks="",...,cran.check=TRUE,repos=NULL) {
  more <- c(...); if(length(more)>0) { pcks <- c(pcks,paste(more)) }
  if(!is.character(pcks)) { stop("must enter package names as character strings") }
  pt <- "package:"; pkgset <- gsub(pt,"",search()[grep(pt,search(),fixed=TRUE)])
  if(all(pcks=="")) { return(pkgset) }
  answer <- (all(pcks %in% pkgset))
  if(is.null(repos)) { try(repos <- getOption("repos") ) }
  if(is.null(repos)) { repos <- "http://cran.ma.imperial.ac.uk/" }
  #print(repos)
  if(!answer & cran.check) {
    check.exist <- search.cran(pcks,repos=repos)
    for(cc in 1:length(check.exist)) {
      if(!pcks[cc] %in% check.exist[[cc]]) { cat("NB: package",pcks[cc],"is not on CRAN\n") }
    }
  }
  return(answer)
}



