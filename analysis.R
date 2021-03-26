a <- read.table("taylor_lewis.txt",skip=1,header=TRUE)

`minmax` <- function(x,TOL=1e-11){
  if(max(x)-min(x) > TOL){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

`leg` <- function(n){subset(a,a$leg==n)[,-1]}

`score_to_numeric` <- function(scores){sapply(scores,singlescore_to_numeric)}
`singlescore_to_numeric` <- function(score){
  stopifnot(length(score)==1)
  if(is.na(score)){return(NA)}
  if(score == "0"){return(0)}
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

`is_ok_leg` <- function(x){  # e.g.: is_ok_leg(leg(3))
  players <- sort(unique(x$player))
  stopifnot(length(players)==2) # exactly  2 players

  stopifnot(minmax(which(x$player==players[1])%%2)) # alternating play
  stopifnot(minmax(which(x$player==players[2])%%2)) # alternating play

  for(i in 1:2){  # for both players...
    d_oneplayer <- subset(x,x$player==players[i])
    darts_scores <- 
      cbind(
          score_to_numeric(d_oneplayer$score1),
          score_to_numeric(d_oneplayer$score2),
          score_to_numeric(d_oneplayer$score3)
      )
    o1 <- rowSums(darts_scores,na.rm=TRUE)
    names(o1) <- NULL
    o1 <- as.numeric(o1)
    o2 <- d_oneplayer$announce
    o2 <- as.numeric(o2)
    stopifnot(identical(o1,o2))
    }

  winning_player <- x$player[nrow(x)]
  d_winner <- subset(x,x$player==winning_player)
  n <- nrow(d_winner)
  stopifnot(sum(d_winner$announced,na.rm=TRUE) == 501)
  return(TRUE)
}




