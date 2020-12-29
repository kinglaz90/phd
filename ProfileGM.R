
###################################################################
#####                                                         #####
#####           PROFILE GRAPHICAL MODELS SELECTION            #####
#####                                                         #####
#####       Lazzerini, A., Lupparelli M., Stingo F.C.         #####
#####                                                         #####
###################################################################

# SIMULATION STUDY

library(MASS)

U=1

Q=1

p=20

s=0.035

k=5

pr=c(0.5,0.5)

spars<-c()

magn<-c()


ACC.a<-c()
TPR.a<-c()
TNR.a<-c()
ACC.e<-c()
TPR.e<-c()
TNR.e<-c()

ACC.f<-matrix(0,Q,3)
ACC.d<-matrix(0,Q,3)
ACC.m<-matrix(0,Q,3)





DATA1<-list(c())
DATA2<-list(c())
DATA3<-list(c())
DATA4<-list(c())




SIGMA<-list(c())

M<-list(c())

ADJ<-list(c())

SPARS<-list(c())

MAGN<-list(c())

for (u in 1:U){
  
  
  Sigma.s<-Sigma(p,k,constr="precision")
  
  SIGMA[[u]]<-Sigma.s$Sigma.s
  
  ADJ[[u]]<-Sigma.s$adj.s
  
  M[[u]]<-Sigma.s$m
  
  SPARS[[u]]<-Sigma.s$spars
  
  MAGN[[u]]<-Sigma.s$magn
  
  N<-c(10,100,1000)
  
  for (q in 1:Q){
    
    
    G<-gen.data(M[[u]],SIGMA[[u]],N)
    
    DATA1[[q+(Q*(u-1))]]<-G$data.s[[1]]
    DATA2[[q+(Q*(u-1))]]<-G$data.s[[2]]
    DATA3[[q+(Q*(u-1))]]<-G$data.s[[3]]
    DATA4[[q+(Q*(u-1))]]<-G$data.s[[4]]
    
    
    
    
  }
  
}



spars<-0

for (i in 1:U){
  
  spars<-spars+SPARS[[i]]
  
}

spars<-spars/U



magn<-0



for (i in 1:U){
  
  magn<-magn+MAGN[[i]]
  
}

magn<-magn/U

spars


# Start the clock!
ptm <- proc.time()


for (u in 1:U){
  
    adj.pr.s<-adj.profile(ADJ[[u]])
    
 for (q in 1:Q){
  
   
    sel<-ProfileGM(DATA1[[q+(Q*(u-1))]],rep(N[[1]],k),type="undirected",strategy="AND")
     
    adj<-sel$adj
   
      f<-check(ADJ[[u]],adj)
      
      adj.pr<-adj.profile(adj)
      
      ff<-check.profile(adj.pr.s,adj.pr)
      
      n.edge.fdm<-ff$n.edge.fdm
   
   ACC.a<-c(ACC.a,f$ACC.a)
   TPR.a<-c(TPR.a,f$TPR.a)
   TNR.a<-c(TNR.a,f$TNR.a)
   ACC.e<-c(ACC.e,f$ACC.e)
   TPR.e<-c(TPR.e,f$TPR.e)
   TNR.e<-c(TNR.e,f$TNR.e)
   ACC.f[q,]<-ff$ACC.f
   ACC.d[q,]<-ff$ACC.d
   ACC.m[q,]<-ff$ACC.m
    
    print(c(u,q))   
 }
  
}


# Stop the clock
proc.time() - ptm


RES10<-rbind(ACC.a,TPR.a,TNR.a,ACC.e,TPR.e,TNR.e)

resu10<-round(apply(RES10,1,mean),2)

sdu10<-round(apply(RES10,1,sd),2)


RES10e<-cbind(ACC.f,ACC.d,ACC.m)

res10e<-round(apply(RES10e,2,mean),2)

sd10e<-round(apply(RES10e,2,sd),2)



RESb10<-rbind(ACC.a,TPR.a,TNR.a,ACC.e,TPR.e,TNR.e)

resb10<-round(apply(RESb10,1,mean),2)

sdb10<-round(apply(RESb10,1,sd),2)


RESb10e<-cbind(ACC.f,ACC.d,ACC.m)

resb10e<-round(apply(RESb10e,2,mean),2)

sdb10e<-round(apply(RESb10e,2,sd),2)




RES100<-rbind(ACC.a,TPR.a,TNR.a,ACC.e,TPR.e,TNR.e)

resu100<-round(apply(RES100,1,mean),2)

sdu100<-round(apply(RES100,1,sd),2)


RES100e<-cbind(ACC.f,ACC.d,ACC.m)

res100e<-round(apply(RES100e,2,mean),2)

sd100e<-round(apply(RES100e,2,sd),2)



RESb100<-rbind(ACC.a,TPR.a,TNR.a,ACC.e,TPR.e,TNR.e)

resb100<-round(apply(RESb100,1,mean),2)

sdb100<-round(apply(RESb100,1,sd),2)


RESb100e<-cbind(ACC.f,ACC.d,ACC.m)

resb100e<-round(apply(RESb100e,2,mean),2)

sdb100e<-round(apply(RESb100e,2,sd),2)



RES1000<-rbind(ACC.a,TPR.a,TNR.a,ACC.e,TPR.e,TNR.e)

resu1000<-round(apply(RES1000,1,mean),2)

sdu1000<-round(apply(RES1000,1,sd),2)


RES1000e<-cbind(ACC.f,ACC.d,ACC.m)

res1000e<-round(apply(RES1000e,2,mean),2)

sd1000e<-round(apply(RES1000e,2,sd),2)



RESb1000<-rbind(ACC.a,TPR.a,TNR.a,ACC.e,TPR.e,TNR.e)

resb1000<-round(apply(RESb1000,1,mean),2)

sdb1000<-round(apply(RESb1000,1,sd),2)


RESb1000e<-cbind(ACC.f,ACC.d,ACC.m)

resb1000e<-round(apply(RESb1000e[1:30,],2,mean),2)

sdb1000e<-round(apply(RESb1000e[1:30,],2,sd),2)


# CHECK FUNCTION

check<-function(adj.s,adj){
  
  P <-0     # number of 1 in real data
  N <-0     # number of 0 in real data  
  FP<-0     # number of 0 in real data classified as 1
  TP<-0     # number of 1 in real data classified as 1
  FN<-0     # number of 1 in real data classified as 0
  TN<-0     # number of 0 in real data classified as 0
  N <-0     # total 
  
  Pa <-0     # number of 1 in real data
  Na <-0     # number of 0 in real data  
  FPa<-0     # number of 0 in real data classified as 1
  TPa<-0     # number of 1 in real data classified as 1
  FNa<-0     # number of 1 in real data classified as 0
  TNa<-0     # number of 0 in real data classified as 0
  
  for(i in 1:k){
    
    
    if (i==1){
      
      for(l in 1:p){
      
      if(adj.s[[i]][l]==1 & adj[[i]][l]==1){
        
        Pa<-Pa+1
        TPa<-TPa+1
        
      } else if (adj.s[[i]][l]==1 & adj[[i]][l]==0){
        
        Pa<-Pa+1
      
      
      } else if (adj.s[[i]][l]==0 & adj[[i]][l]==0){
        
        Na<-Na+1
        TNa<-TNa+1
        
      } else if (adj.s[[i]][l]==0 & adj[[i]][l]==1){
        
        Na<-Na+1
        
        
       }
      }
        
      } else {
    
    
    for(l in 1:p){
      
      for(j in 1:p){
        
        if (j>l){
          
                 if (adj.s[[i]][l,j]==0 & adj[[i]][l,j]==0){
            
             N<-N+1
              
            TN<-TN+1  
              
          } else if (adj.s[[i]][l,j]==0 & adj[[i]][l,j]==1) {
              
             N<-N+1
            
          } else if (adj.s[[i]][l,j]==1 & adj[[i]][l,j]==1) {
            
            P<-P+1
              
           TP<-TP+1  
              
          } else if (adj.s[[i]][l,j]==1 & adj[[i]][l,j]==0) {
            
            P<-P+1
            
          }
            
          }
          
          
        }
       }
      }
  }
  

  ACC.e<-(TP+TN)/(P+N)
  
  TPR.e<-TP/P
    
  TNR.e<-TN/N  
  
  
  ACC.a<-(TPa+TNa)/(Pa+Na)
  
  TPR.a<-TPa/Pa
  
  TNR.a<-TNa/Na 
  
  
  results<-list("ACC.a"=ACC.a,"TPR.a"=TPR.a,"TNR.a"=TNR.a,"ACC.e"=ACC.e,"TPR.e"=TPR.e,"TNR.e"=TNR.e)
  
  return(results)

}


adj.profile<-function(adj){
  
    adj.pr<-diag(length(adj[[1]]))
    
    k<-length(adj)
    
    p<-length(adj[[1]])
    
    
    for(l in 1:p){
      
      for(j in 1:p){
        
        for(i in 2:k){
        
        if (j>l){
  
         adj.pr[l,j]<-adj.pr[l,j]+adj[[i]][l,j]

         }
        }
        
         adj.pr[l,j]<-adj.pr[l,j]/(k-1)
     }
    }
    
    diag(adj.pr)<-1
  
    return(adj.pr)
    
}



check.profile<-function(adj.pr.s,adj.pr){
  
  Fu <-0     # number of full edge
  D <-0     # number of dotted edge  
  M <-0     # number of missing edge
  
  FFm<-0    # number of full classified as missing
  FFd<-0    # number of full classified as dotted
  TF<-0     # number of full classified as full
  
  FDm<-0    # number of dotted classified as missing
  FDf<-0    # number of dotted classified as full
  TD<-0     # number of dotted classified as dotted
  
  FMf<-0    # number of missing classified as full
  FMd<-0    # number of missing classified as dotted
  TM<-0     # number of missing classified as missing
  

    
    for(l in 1:p){
      
      for(j in 1:p){
        
        if (j>l){
          
                 if (adj.pr.s[l,j]==1 & adj.pr[l,j]==1){
            
           Fu<-Fu+1
            
           TF<-TF+1  
            
          } else if (adj.pr.s[l,j]==1 & adj.pr[l,j]==0) {
            
            Fu<-Fu+1
            
          FFm<-FFm+1  

          } else if (adj.pr.s[l,j]==1 & adj.pr[l,j]>0 & adj.pr[l,j]<1) {
            
            Fu<-Fu+1
            
          FFd<-FFd+1  
            
          } else if (adj.pr.s[l,j]>0 & adj.pr.s[l,j]<1 & adj.pr[l,j]>0 & adj.pr[l,j]<1) {
            
            D<-D+1
            
           TD<-TD+1  
            
          
          } else if (adj.pr.s[l,j]>0 & adj.pr.s[l,j]<1 & adj.pr[l,j]==1) {
          
           D<-D+1
          
          FDf<-FDf+1  
          
          } else if (adj.pr.s[l,j]>0 & adj.pr.s[l,j]<1 & adj.pr[l,j]==0) {
          
          D<-D+1
          
          FDm<-FDm+1  
          
          } else if (adj.pr.s[l,j]==0 & adj.pr[l,j]==0) {
          
          M<-M+1
          
         TM<-TM+1  
          
          } else if (adj.pr.s[l,j]==0 & adj.pr[l,j]==1) {
          
          M<-M+1
          
        FMf<-FMf+1  
          
          } else if (adj.pr.s[l,j]==0 & adj.pr[l,j]>0 & adj.pr[l,j]<1) {
          
          M<-M+1
          
        FMd<-FMd+1  
          
        }
        
        
      }
     } 
    }
  
  TFR<-   TF/Fu
  FFmR<-  FFm/Fu  
  FFdR<-  FFd/Fu  
  
  TDR<-   TD/D
  FDmR<-  FDm/D 
  FDfR<-  FDf/D  
  
  TMR<-   TM/M
  FMdR<-  FMd/M  
  FMfR<-  FMf/M  
    
  
  ACC.f<-c(TFR,FFmR,FFdR)
  ACC.d<-c(TDR,FDmR,FDfR)
  ACC.m<-c(TMR,FMdR,FMfR)
  
  n.edge.fdm<-c(Fu,D,M)
  
  
  results<-list("n.edge.fdm"=n.edge.fdm,"ACC.f"=ACC.f,"ACC.d"=ACC.d,"ACC.m"=ACC.m)
  
  return(results)
  
}



# FUNCTION TO GENERATE MULTIPLE COVARIANCE MATRICES

Sigma<- function(p,k,s,pr,constr){
  
  S<-list(c())
  
  M<-diag(p)
  
  B<-matrix(0,p,p)
  
  lb<-length(B[upper.tri(B)])
  
  psumB<-round(s*lb)
  
  vpos<-sample(1:lb,psumB)
  
  v<-rep(0,lb)
  
  v[vpos]<-1
  
  B[upper.tri(B)]<-v
  
  for (i in 1:p){
    
    for (j in (1:p)){
      
      if(j>i & B[i,j]==1){
        
        M[i,j]<- runif(1,0.1,0.4)*sample(c(-1,1),1)
        
        M[j,i]<-M[i,j]
        
      }
      
    }
    
  }
  

  M1<-M
  
  for (i in 1:p){
    
    T<-1.5*sum(abs(M[i,-i]))
    
    if (T!=0){
      
      for (j in (1:p)){
        
        if(j!=i){
          
          M1[i,j]<- M[i,j]/T
          
        }
        
      }
    }
  }
  
  
  A<-(M1+t(M1))/2  
  
  A<-solve(A)
  
  S[[1]]<-diag(p)
  
  for (i in 1:p){
    
    for (j in (1:p)){
      
      if(j==i){
        
        S[[1]][i,j]<- A[i,j]/sqrt(A[i,i]%*%A[j,j])
        
      } else {
        
        S[[1]][i,j]<- (0.6*A[i,j])/sqrt(A[i,i]%*%A[j,j])
        
      }
      
    }
    
  }
  
  
  
  if (constr=="precision"){
    

    m<-list(c())
    
    mlwf<-list(c())
    
    Sp<-S
    
    Sp[[1]]<-solve(S[[1]])
    
    spars<-c()
    
    magn<-c()
    
    mlwf[[1]]<-rep(0,p) # vector of betaX LWF (p x 1)
    
    nof<-sample(1:p,1)  # number of betaX LWF!=0
    
    pnof<-sample(1:p,nof) # for which nodes betaX LWF!=0
    
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
      
      if (nof!=1){
        
        mlwf[[i]][pnof]<-diag(sample(c(-1,1),nof,replace = TRUE))%*%runif(nof,0.5,1.5)
        
        # we generate the value of betaX LWF where we do not have 0-constraints
        
        
      } else {
        
        mlwf[[i]][pnof]<-sample(c(-1,1),nof)%*%runif(nof,0.5,1.5)
        
      }
      
      
      
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
    
    
    for (i in 1:k) {
    
    
    S[[i]]<-solve(Sp[[i]]) # from now covariance matrix 
    
    ddiag<-sqrt(diag(S[[i]]))
    
    ddiag<-diag(ddiag,p,p)
    
    
    S[[i]]<-solve(ddiag)%*%S[[i]]%*%solve(ddiag) # from now correlation matrix
    
  }
    
    
    
  } else if (constr=="covariance") {
  
  
  
  m<-list(c())
  
  spars<-c()
  
  magn<-c()
  
  m[[1]]<-rep(0,p)
  
  nof<-sample(1:p,1)  # number of betaX!=0
  
  pnof<-sample(1:p,nof) # for which nodes betaX!=0
  
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
    
    if (nof!=1){
      
      m[[i]][pnof]<-diag(sample(c(-1,1),nof,replace = TRUE))%*%runif(nof,0.5,1.5)
      
    } else {
      
      m[[i]][pnof]<-sample(c(-1,1),nof)%*%runif(nof,0.5,1.5)
      
    }
    
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





# FUNCTION TO GENERATE A DATASET


gen.data<-function(m,Sigma.e,N){

data.s<-list(c())  
  
for (h in 1:length(N)){ 
  
n<-rep(N[h],k)

D<-list(c()) 

D[[1]]<-mvrnorm(n[1],m[[1]],Sigma.e[[1]])

X<-rep(0,n[1])

D[[1]]<-cbind(D[[1]],X)

D[[1]]<-as.data.frame(D[[1]])

 for (i in 2:k){

  D[[i]]<-mvrnorm(n[i],m[[i]],Sigma.e[[i]])
  
  X<-rep(i-1,n[i])
  
  D[[i]]<-cbind(D[[i]],X)
  
  D[[i]]<-as.data.frame(D[[i]])
  
  
}

D1<-D[[1]]

 for( i in 2:k){
   
   D1<-rbind(D1,D[[i]])
   
 }

D1$X<-as.factor(D1$X)

data.s[[h]]<-D1

}

results<-list("data.s"=data.s)

  return(results)
}





# FUNCTION TO MODEL SELECTION

ProfileGM<-function(data,n,type,strategy){
  
  p<-(ncol(data)-1)
  
  k<-length(levels(data[,p+1]))
  
  if (type=="bidirected") {
  
  library(glmnet)
  
  res<-data[,-(p+1)] # residuals matrix
  
  coef<-list(c())
  
  coef[[1]]<-matrix(0,p,(k-1))
  
  adj<-list(c())
  
  adj[[1]]<-matrix(rep(0,p),1,p)
  
  for(i in 1:p){
    
    modX <- lm(data[,i] ~ data[,p+1], data=data)
    
    mod <- lm(data[,i] ~ 1, data=data)
    
    dev<-anova(mod, modX, test="Chisq")
    
    res[,i]<-modX$residuals
    
    if (dev$`Pr(>Chi)`[2] <= 0.05){
      
      coef[[1]][i,]<-modX$coefficients[-1]
      
      adj[[1]][i]<-1
      
    } 
    
  }
  
  
  
  
  X<-matrix(0,nrow=sum(n),ncol=k)
  
  
  ns<-c()
  
  ns[1]<-n[1]
  
  for (i in 2:k){
    
    ns[i]<- n[i]+ns[i-1]
    
  }
  
  for (i in 1:k){
  
  coef[[i+1]]<-diag(p)
  
   adj[[i+1]]<-diag(p)
   
  }

   
  for(i in 1:p){
    
    for(j in 1:p){
      
      if (i!=j){
        
        if (adj[[1]][i]==0 &  adj[[1]][j]==0){
          
          modj<-lm(res[,i]~res[,j], data = res)
        
          mod<- lm(res[,i]~1, data = res)
          
          dev<- anova(mod, modj, test="Chisq")
          
          
          if (dev$`Pr(>Chi)`[2] <= 0.05){
            
            for (z in 2:(k+1)){
            
            coef[[z]][i,j]<-modj$coefficients[2]
            
            adj[[z]][i,j]<-1
            
            } 
          }
          
          
        } else {
          
          
          
          for (z in 1:k){
            
            if (z==1){
              
            X[1:ns[z],z]<-res[1:ns[z],j]  
              
            } else {
            
            X[(ns[z-1]+1):ns[z],z]<-res[(ns[z-1]+1):ns[z],j]
            
            }
            
          }
          
          
          W<-as.matrix(cbind(res[,i],X))   ######################################## rivedere
          
          mod<-cv.glmnet(W[,-1],W[,1],family = "gaussian",nfolds = 10)
          
          coeff<-coef(mod, s = mod$lambda.1se)[2:(k+1)]
          
          for (z in 2:(k+1)){
            
            coef[[z]][i,j]<-coeff[z-1]
            
            if (coeff[z-1] != 0){
            
            adj[[z]][i,j]<-1
            
            }
            
          } 
        
        }
      }
      
    }
  }
  
  adj1<-list(c())
  
  adj1[[1]]<-adj[[1]]
  
  
  for (i in 1:k){
    
    adj1[[i+1]]<-diag(p)
    
  }
  
  if (strategy=="AND"){
  
  for (z in 2:(k+1)){
    
    for (i in 1:p){
      
      for (j in 1:p){
        
        if (coef[[z]][i,j]!=0 & coef[[z]][j,i]!=0){
          
          adj1[[z]][i,j]<-1
          adj1[[z]][j,i]<-1
          
        }
      }
    }
    
  }
    
 } else if (strategy=="OR"){
    
   for (z in 2:(k+1)){
     
     for (i in 1:p){
       
       for (j in 1:p){
         
         if (coef[[z]][i,j]!=0 | coef[[z]][j,i]!=0){
           
           adj1[[z]][i,j]<-1
           adj1[[z]][j,i]<-1
           
         }
       }
     }
     
   } 
    
  }
  
  
  results<-list("coef"=coef,"adj"=adj1)
  
  return(results)
  
  
  
  } else if (type=="undirected") {
    
    
    library(glmnet)
    
    res<-data[,-(p+1)] # residuals matrix
    
    coef<-list(c())
    
    coef[[1]]<-matrix(0,p,(k-1))
    
    adj<-list(c())
    
    adj[[1]]<-matrix(rep(0,p),1,p)
    
    
    
    
  #  for(i in 1:p){
      
  #    modX <- lm(data[,i] ~ ., data=data[,-c(i)])
      
  #     mod <- lm(data[,i] ~ ., data=data[,-c(i,(p+1))])
      
  #    dev<-anova(mod, modX, test="Chisq")
      
  #    if (dev$`Pr(>Chi)`[2] <= 0.05){
        
  #      coef[[1]][i,]<-modX$coefficients[-c(1:p)]
        
  #      adj[[1]][i]<-1
        
  #    } 
      
  #  }
    
    
    
    data2<-data.matrix(data)
    
    for (i in 1:p){
    
    modX<-cv.glmnet( data2[,-i] , data2[,i] ,family = "gaussian", nfolds = 10)
    
    coeff<-coef(modX, s = modX$lambda.1se)[p+1]
    
    coeff<-round(coeff,6)
    
       if (coeff!=0){
    
         adj[[1]][i]<-1
    
       } 
    
    
    }
    
    
    
    
    
    
     for(i in 1:p){
    
       modX <- lm(data[,i] ~ data[,p+1], data=data)
      
       res[,i]<-modX$residuals
    
     }
    
    
#      round(cor(data[1:n0,-(p+1)]),10) == round(cor(res[1:n0,]),10)
    
#      round(cor(data[(n0+1):(n0+n1),-(p+1)]),10) == round(cor(res[(n0+1):(n0+n1),]),10)
    
#      round(cor(data[(n0+n1+1):(n0+n1+n2),-(p+1)]),10) == round(cor(res[(n0+n1+1):(n0+n1+n2),]),10)
    
    
    ns<-c()
    
    ns[1]<-n[1]
    
    for (i in 2:k){
      
      ns[i]<- n[i]+ns[i-1]
      
    }
    
    for (i in 1:k){
      
      coef[[i+1]]<-diag(p)
      
      adj[[i+1]]<-diag(p)
      
    }
    
    
    for(i in 1:p){
      
      X<-matrix(999,nrow=sum(n),1)
      
      pos<-c()
      
      
      for(j in 1:p){
        
        if (i!=j){
          
          if (adj[[1]][i]==0 &  adj[[1]][j]==0){
            
            pos<-c(pos,1)
            
            X<- cbind(X,res[,j])
            
          } else {
            
            pos<-c(pos,0)
            
            Xp<-matrix(0,nrow=sum(n),ncol=k)
            
            for (z in 1:k){
              
              if (z==1){
                
                Xp[1:ns[z],z]<-res[1:ns[z],j]  
                
              } else {
                
                Xp[(ns[z-1]+1):ns[z],z]<-res[(ns[z-1]+1):ns[z],j]
                
              }
              
            }
            
            X<-cbind(X,Xp)
            
          }
          
        } else{
          
          pos<-c(pos,8)
          
        }
        
      }
      
      X<-X[,-1]
      
      
      
      W<-as.matrix(cbind(res[,i],X))
      
      mod<-cv.glmnet(W[,-1],W[,1],family = "gaussian",nfolds = 10)
      
      coeff<-coef(mod, s = mod$lambda.1se)[-1]
      
      coeff<-round(coeff,6)
      
      
      l<-c()
      
      for (j in 1:p){
        
        if (j!=i){
          
          if (pos[j]==1){
            
            for (z in 2:(k+1)){
              
              if (coeff[length(l)+1]!=0){
                
                coef[[z]][i,j]<-coeff[length(l)+1]
                
                adj[[z]][i,j]<-1
                
              }
              
            }
            
            l<-c(l,1)
            
            
          } else if (pos[j]==0){
            
            for (z in 2:(k+1)){
              
              if (coeff[length(l)+z-1]!=0){
                
                coef[[z]][i,j]<-coeff[length(l)+z-1]
                
                adj[[z]][i,j]<-1
                
              }
              
            }
            
            l<-c(l,rep(1,k))
            
          }
          
        } 
        
      }
      
    }
    
    
    
    adj1<-list(c())
    
    adj1[[1]]<-adj[[1]]
    
    
    for (i in 1:k){
      
      adj1[[i+1]]<-diag(p)
      
    }
    
    
    
    if (strategy=="AND"){
      
      for (z in 2:(k+1)){
        
        for (i in 1:p){
          
          for (j in 1:p){
            
            if (coef[[z]][i,j]!=0 & coef[[z]][j,i]!=0){
              
              adj1[[z]][i,j]<-1
              adj1[[z]][j,i]<-1
              
            }
          }
        }
        
      }
      
    } else if (strategy=="OR"){
      
      for (z in 2:(k+1)){
        
        for (i in 1:p){
          
          for (j in 1:p){
            
            if (coef[[z]][i,j]!=0 | coef[[z]][j,i]!=0){
              
              adj1[[z]][i,j]<-1
              adj1[[z]][j,i]<-1
              
            }
          }
        }
        
      } 
      
    }
    
    
    results<-list("coef"=coef,"adj"=adj1)
    
    return(results)
    
  }
  
}


edge.type<-function(mat){
  
  ef<-0
  ed<-0
  ne<-0 
  
  for (i in 1:nrow(mat)){
    
    for (j in 1:ncol(mat)){
  
      if (j>i){
        
        if(mat[i,j]==1){
          
          ef<-ef+1
          
        } else if (mat[i,j]==0){
          
          ne<-ne+1
          
        } else {
          
          ed<-ed+1
          
        }
      
        
      }
      
  }
  }
  
  results<-c(ne,ed,ef)
  
  return(results)
  
}

