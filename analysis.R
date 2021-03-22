a <- read.table("taylor_lewis.txt",skip=1,header=TRUE)

`minmax` <- function(x,TOL=1e-11){
  if(max(x)-min(x) > TOL){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

`score_to_numeric` <- function(score){
  if(score=="NA"){return(NA)}
  if(score=="bull"){
    return(50)
  } else if(score == "25"){
    return(25)
  }
  p <- substr(score,1,1)
  if(p == "s"){
    m <- 1  # "s" = single -> multiple=1
  } else if(p == "d"){
    m <- 2  # "d" = double -> multiple=2
  } else if(p == "t"){
    m <- 3  # "t" = triple -> multiple=3
  } else {
    stop("prefix not recognised: must be one of s,d,t")
  }
    
  n <- as.numeric(substr(score,2,100))
  stopifnot(n %in% 1:20)
  
  return(m*n)
  }


actual_scores <- function(a){
  out <- matrix(0,nrow(a),3)
  for(i in seq_len(nrow(a))){
    for(j in 1:3){
      out[i,j] <- score_to_numeric(a[i,1+2*j])
    }
  }
  return(out)
}

`is_ok_leg` <- function(a){
  players <- sort(unique(a$player))
  stopifnot(minmax(which(a$player==players[1])%%2))
  stopifnot(minmax(which(a$player==players[2])%%2))

    
  
  return(TRUE)
}



is_ok_leg(subset(a,a$leg==1))
