


ddd<-diag(p)

for(l in 1:p){
  
  for(j in 1:p){
    
    if (j>l & j==(l+1)){
      
      ddd[l,j]<- 0.5
      ddd[j,l]<-ddd[l,j]
      
    }
    
    if (j>l & j==(l+2)){
      
      ddd[l,j]<- 0.4
      ddd[j,l]<-ddd[l,j]
      
    }
    
    
    }
  }
 
is.positive.semi.definite(ddd)

ddd


dddd<-solve(ddd)




Sigma<- function(p,k,constr){
  
  S<-list(c())
  Sp<-list(c())
  
  
  ddd<-diag(20)
  
  for(l in 1:p){
    
    for(j in 1:p){
      
      if (j>l & j==(l+1)){
        
        ddd[l,j]<- 0.5
        ddd[j,l]<-ddd[l,j]
        
      }
      
      if (j>l & j==(l+2)){
        
        ddd[l,j]<- 0.4
        ddd[j,l]<-ddd[l,j]
        
      }
      
      
    }
  }
  
  
  
  
  Sp[[1]]<-ddd
  S[[1]]<-solve(Sp[[1]])
  
  if (constr=="precision"){
    
    
    m<-list(c())
    
    mlwf<-list(c())
    
    spars<-c()
    
    magn<-c()
    
    mlwf[[1]]<-rep(0,p) # vector of betaX LWF (p x 1)
    
    nof<-10
    
    pnof<-1:10 # for which nodes betaX LWF!=0
    
    posb0<-c(1:p)[-pnof] # positions of betaX LWF!=0
    
    adj.X<-rep(0,p) 
    
    adj.X[pnof]<-1
    
    adj.Y<-list(c())
    
    adj.s<-list(c())
    
    adj.s[[1]]<-adj.X
    
    adj.Y[[1]]<-diag(p)
    
    
    
    m[[1]]<-S[[1]]%*%mlwf[[1]] # vector of betaX R (the mean of the rnormal)
    
    
    
    
    
    for(l in 1:p){
      
      for(j in 1:p){
        
        if (j>l & Sp[[1]][l,j]!=0){
          
          adj.Y[[1]][[l,j]]<-1                  # 0-constraints now is on prec. mat.
          adj.Y[[1]][[j,l]]<-adj.Y[[1]][[l,j]]
        }
      }
    }
    
    adj.s[[2]]<-adj.Y[[1]]
    
    
    for(i in 2:k){
      
      Sp[[i]]<-Sp[[1]]
      
      adj.Y[[i]]<-adj.Y[[1]]
      
      for(l in 1:p){
        
        for(j in 1:p){
          
          if (j>l & Sp[[i]][l,j]!=0){
            
            
            if (adj.X[l]!=0 | adj.X[j]!=0){ # if at leat one betaX LWF is not 0
              
              Sp[[i]][l,j]<-sample(c(0,Sp[[1]][l,j]),1) # we can put to 0 the element of the prec.
              
              Sp[[i]][j,l]<-Sp[[i]][l,j]
            }
            
            
            if (Sp[[i]][l,j]==0){
              
              adj.Y[[i]][[l,j]]<-0
              adj.Y[[i]][[j,l]]<-adj.Y[[i]][[l,j]]
            }
            
            
          }
        }
      }
      
      adj.s[[i+1]]<-adj.Y[[i]]
      
      mlwf[[i]]<- mlwf[[1]] # initialization of betaX LWF
      
        
      mlwf[[i]][pnof]<-rep(1,nof)
        
        # we generate the value of betaX LWF where we do not have 0-constraints
        

      
      
      
      m[[i]]<-solve(Sp[[i]])%*%mlwf[[i]] # vector of betaX R (the mean of the rnormal)
      
      S[[i]]<-solve(Sp[[i]])
      
      
    }
    
    spars<-0
    magn<-0
    
    for(i in 1:k){
      
      for(l in 1:p){
        
        for(j in 1:p){
          
          if (j>l){
            
            if (Sp[[i]][l,j]!=0){
              
              spars<-spars+1  
              magn<-magn+abs(Sp[[i]][l,j])
              
            } 
          }
        }
      }
    }
    
    magn<-magn/spars
    
    spars<-spars/(k*((p*(p-1))/2))
    
    
    # WE OBTAIN THE CORRELATION MATRIX 
    
    
#     for (i in 1:k) {
      
      
#      S[[i]]<-solve(Sp[[i]]) # from now covariance matrix 
      
#      ddiag<-sqrt(diag(S[[i]]))
      
#      ddiag<-diag(ddiag,p,p)
      
      
#      S[[i]]<-solve(ddiag)%*%S[[i]]%*%solve(ddiag) # from now correlation matrix
      
#    }
    
    
    
  } else if (constr=="covariance") {
    
    S[[1]]<-Sp[[1]]
    
    m<-list(c())
    
    spars<-c()
    
    magn<-c()
    
    m[[1]]<-rep(0,p)
    
    nof<-10
    
    pnof<-1:10 # for which nodes betaX LWF!=0
    
    posb0<-c(1:p)[-pnof] # positions of betaX=0
    
    adj.X<-rep(0,p)
    
    adj.X[pnof]<-1
    
    adj.Y<-list(c())
    
    adj.s<-list(c())
    
    adj.s[[1]]<-adj.X
    
    adj.Y[[1]]<-diag(p)
    
    
    for(l in 1:p){
      
      for(j in 1:p){
        
        if (j>l & S[[1]][l,j]!=0){
          
          adj.Y[[1]][[l,j]]<-1
          adj.Y[[1]][[j,l]]<-adj.Y[[1]][[l,j]]
        }
      }
    }
    
    adj.s[[2]]<-adj.Y[[1]]
    
    
    for(i in 2:k){
      
      S[[i]]<-S[[1]]
      
      adj.Y[[i]]<-adj.Y[[1]]
      
      for(l in 1:p){
        
        for(j in 1:p){
          
          if (j>l & S[[i]][l,j]!=0){
            
            
            if (adj.X[l]!=0 | adj.X[j]!=0){
              
              S[[i]][l,j]<-sample(c(0,S[[1]][l,j]),1)
              
              S[[i]][j,l]<-S[[i]][l,j]
            }
            
            
            if (S[[i]][l,j]==0){
              
              adj.Y[[i]][[l,j]]<-0
              adj.Y[[i]][[j,l]]<-adj.Y[[i]][[l,j]]
            }
            
            
          }
        }
      }
      
      adj.s[[i+1]]<-adj.Y[[i]]
      
      m[[i]]<-rep(0,p)
      
      m[[i]][pnof]<-rep(1,nof)
        

      
    }
    
    spars<-0
    magn<-0
    
    for(i in 1:k){
      
      for(l in 1:p){
        
        for(j in 1:p){
          
          if (j>l){
            
            if (S[[i]][l,j]!=0){
              
              spars<-spars+1  
              magn<-magn+abs(S[[i]][l,j])
              
            } 
          }
        }
      }
    }
    
    magn<-magn/spars
    
    spars<-spars/(k*((p*(p-1))/2))
    
  }
  
  
  
  results<-list("Sigma.s"=S,"spars"=spars,"magn"=magn,"m"=m,"adj.s"=adj.s)
  
  return(results)
  
}
