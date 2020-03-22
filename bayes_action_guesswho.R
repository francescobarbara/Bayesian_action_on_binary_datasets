rows <- function(A,x, number=1){
  if(is.vector(A)==TRUE){
    return(A)
  }
  if(nrow(A)==0){
    return(A)
  }
  if (number == 1){
    A = A[A[,x]==1,]
  }
  if (number == 0){
    A = A[A[,x]==0,]
  }
  return(A)
}



shuffle <-function(A,x){
  if(is.vector(A)==TRUE){
    return(A)
  }
  
  #newpart
  if(nrow(A)==0){
    
    return(A)
  }
  vec=A[x,]
  A= A[-x,]
  A = rbind(vec,A)
  return(A)
}



no_moves <- function(A,v,roww,threshold=1,count,y){
 
  A=shuffle(A,roww)
  
  
  if(is.vector(A)==TRUE){
    
    return(count)
  }
  #newpart
  if(nrow(A)==0){
    
    return(count)
  }
  
    
  x=v[y]
  print('y is')
  print(y)
  print('x is')
  print(x)
 
  
  if(A[1,x]== 1){
    
    A=rows(A,x,number=1)
    
    y=(y-1-sum(2^(c(0:count))))*2+sum(2^c(0:(count+1)))+1
    
    x=v[y]
    
  }
  else if(A[1,x]==0){
    
    A=rows(A,x,number=0)
    
    y=(y-1-sum(2^(c(0:count))))*2+sum(2^c(0:(count+1)))+2
    
    x=v[y]
    
  }
  count=count+1
  
  
  return(no_moves(A,v,1,threshold=1,count,y))
}



exp_vec <- function(A,v){
  n=nrow(A)
  vec=numeric(n)
  for(i in 1:n){
    count=0
    y=1
    
    z=no_moves(A,v,i,threshold=1,count,y)
    
    vec[i]=z
    
  }
  return(sum(vec)/n)
}



double_vec <- function(v){
  n = length(v)
  w=numeric(2*n)
  for(i in 1:n){
    w[2*i-1]=v[i]
    w[2*i]=v[i]
  }
  return(w)
}
#perfetto
check <- function(w){
  if(length(w) <=3){
    return(FALSE)
  }
  n=length(w)
  y=log2(n+1)
  m=2^(y-2)
  vec_1=w[m:(2*m-1)]
  vec_1=double_vec(vec_1)
  vec_2=w[(2*m):(4*m-1)]
  
  if (isTRUE(all.equal(vec_1,vec_2,check.names=FALSE))==TRUE){
    return(TRUE)
  }
  if (isFALSE(all.equal(vec_1,vec_2,check.names=FALSE))==FALSE){
    return(FALSE)
  }
}



pick_best <- function(A){
  if(is.vector(A)== TRUE){
    return(TRUE)
  }
  n=ncol(A)
  m=nrow(A)
  vec=numeric(n)
  for(i in 1:n){
    vec[i]=abs(sum(A[,i])-m/2)
  }
  which.min(vec)
  
}
#perfetto
greed_vec<-function(A){
  v=list(A)
  w=pick_best(A)
  z=list()
  m=ncol(A)
  while(TRUE){
    for(A in v){
      C=rows(A,pick_best(A),number=1)
      D=rows(A,pick_best(A),number=0)
      w=c(w,pick_best(C),pick_best(D))
      z=c(z,list(C),list(D))
    }
    if(check(w)==TRUE){
      break
    }
    if(length(z) >= 2^m){
      break
    }
    v=z
    z=list()
  }
  return(w)
}

##(sqrt(sum(A[,i])-m)^2+(sum(A[,i]))^2)^(-1)
pick_suboptimal <- function(A){
  if(is.vector(A)== TRUE){
    return(TRUE)
  }
  if(nrow(A)==0){
    return(TRUE)
  }
  n=ncol(A)
  m=nrow(A)
  vec=numeric(n)
  for(i in c(1:n)){
    vec[i]= (abs(sum(A[,i])-m/2)+1)^(-1)
    if((sum(A[,i]==m) | (sum(A[,i]==0)){
      vec[i]=0
    }
  }
  vv=rep(c(0),n)
  if(isTRUE(all.equal(vec,vv,check.names=FALSE))==TRUE){
    return(1)
  }
  vec=vec/(sum(vec))
  
  return(sample(c(1:n),1,prob=vec))
}

subopt_vec<-function(A){
  v=list(A)
  w=c(pick_suboptimal(A))
  
  z=list()
  m=ncol(A)
  counting=1
  while(TRUE){
    for(A in v){
      
      
      C=rows(A,w[counting],number=1)
      
      D=rows(A,w[counting],number=0)
      
      w=c(w,pick_suboptimal(C),pick_suboptimal(D))
      
      z=c(z,list(C),list(D))
      counting=counting+1
      
    }
    if(check(w)==TRUE){
      break
    }
    if(length(z) >= 2^m){
      break
    }
    v=z
    z=list()
  }
  return(w)
}

optimal_tree<-function(A,iterations){
  threshold=(exp_vec(A,greed_vec(A)))
  
  initial_threshold=threshold
  counting=0
  optimal_vec=greed_vec(A)
  improvement=FALSE
  while(counting <= iterations){
    w = subopt_vec(A)
    
    E=tryCatch(
      exp_vec(A,w),
      error=function(e) e
    )
    suppressWarnings(if(class(E)!='numeric'){
      E=threshold+1
    }
    )
    
    
    if(E <= threshold){
      threshold=E
      optimal_vec=w
      improvement=TRUE
    }
    counting=counting+1
  }
  return(list(threshold,initial_threshold,w,optimal_vec,greed_vec(A),improvement))
}

competition<-function(A,v,w){
  n=ncol(A)
  w_1=0
  w_2=0
  for(i in 1:n){
    a=no_moves(A,v,i,threshold=1,count=0,y=1)
    b=tryCatch(
      no_moves(A,w,i,threshold=1,count=0,y=1),
      error=function(e) e
    )
    suppressWarnings(if(class(b)!='numeric'){
      b=n+1
    }
    )
    if(a<b){
      w_1=w_1 +1
    }
    if(b<a){
      w_2=w_2 +1
    }
    if(a==b){
      w_1=w_1+0.5
      w_2=w_2+0.5
    }
  }
  return((w_2)/(w_1))
}


optimal_tree2<-function(A,iterations){
  
  threshold=1
  greed=greed_vec(A)
  counting=0
  optimal_vec=greed
  improvement=FALSE
  while(counting <= iterations){
    w = subopt_vec(A)
    
    odds=tryCatch(
      competition(A,greed,w),
      error=function(e) e
    )
    suppressWarnings(if(class(odds)!='numeric'){
      odds=0
    }
    )
    
    
    if(odds > threshold){
      threshold=odds
      optimal_vec=w
      improvement=TRUE
    }
    counting=counting+1
  }
  return(list(threshold,w,optimal_vec,greed_vec(A),improvement))
}

