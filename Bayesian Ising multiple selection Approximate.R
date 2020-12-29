

### SCENARIO A metodi approssimati


######### METHODS COMPARISON , SIMULATION STUDY ################




# G0-G1-G2-G3

DATA<-list()


RSL<-matrix(0,10,2)
RDSSL<-matrix(0,10,2)
RBA<-matrix(0,10,2)
RBAL<-matrix(0,10,2)
RBE<-matrix(0,10,2)

RBEL<-matrix(0,10,2)

predSL<-list()
predDSSL<-list()
predBA<-list()
predBAL<-list()
predBE<-list()

predBEL<-list()



AUCSL<-c()
AUCDSSL<-c()
AUCBA<-c()
AUCBAL<-c()
AUCBE<-c()

AUCBEL<-c()




for (x in 1:X){
  
  predSL[[x]]<-matrix(0,10,45)
  predDSSL[[x]]<-matrix(0,10,45)
  predBA[[x]]<-matrix(0,10,45)
  predBAL[[x]]<-matrix(0,10,45)
  predBE[[x]]<-matrix(0,10,45)
  predBEL[[x]]<-matrix(0,10,45)
  
  
}



D<-matrix(0,50000,45)

ciccio<-list()

PREDBAL<-list()
PREDBA<-list()
PREDBE<-list()
PREDBEL<-list()


for (t in 1:10){
  
  ciccio[[t]]<-D
  
}

for (x in 1:X){
  
  PREDBAL[[x]]<-ciccio
  
  PREDBA[[x]]<-ciccio
  
  PREDBE[[x]]<-ciccio
  
  PREDBEL[[x]]<-ciccio
  
  
  
}


K<-50000

L.testXBAL<-list()

for (P in 1:10){
  
  L.testXBAL[[P]]<-list()
  
  for (k in 1:K){
    
    L.testXBAL[[P]][[k]]<-list()
    
    for (x in 1:X) {
      
      L.testXBAL[[P]][[k]][[x]]<-matrix(0,p,p)
    }
    
  }
  
}




L.testXBA<-list()

for (P in 1:10){
  
  L.testXBA[[P]]<-list()
  
  for (k in 1:K){
    
    L.testXBA[[P]][[k]]<-list()
    
    for (x in 1:X) {
      
      L.testXBA[[P]][[k]][[x]]<-matrix(0,p,p)
    }
    
  }
  
}







for (P in 4:10){ # INIZIO
  
  
  X<-4
  p<-10
  K<-200000
  n<-100
  burn<-n
  alpha<--1
  beta<-1.5
  
  
  #########################################################################
  #################            G0-G1-G2-G3             ####################
  #########################################################################
  
  
  
  G0<-matrix(0,p,p)
  
  
  for (i in 1:(p-1)){
    
    G0[i,i+1]<-1
    
  }
  
  
  G0[lower.tri(G0)] = t(G0)[lower.tri(G0)]
  
  G1<-G0
  
  G2<-G0
  
  G3<-G0
  
  
  # INIZIO DATA
  
  
  dataf<-matrix(0,K,p)
  
  dataf[1,]<-rep(1,p)
  
  for (k in 2:K){
    
    for (r in 1:p){
      
      s<-sum(dataf[k,which(G0[r,]==1)])
      
      odds<-exp(beta*s+alpha)
      
      prob<-odds/(1+odds)
      
      dataf[k,r]<-rbinom(1,1,prob)
      
    }
    
  }
  
  data<-dataf[(nrow(dataf)-(n-1)):nrow(dataf),]
  
  data0<-cbind(data,0)
  
  
  dataf<-matrix(0,K,p)
  
  dataf[1,]<-rep(1,p)
  
  for (k in 2:K){
    
    for (r in 1:p){
      
      s<-sum(dataf[k,which(G1[r,]==1)])
      
      odds<-exp(beta*s+alpha)
      
      prob<-odds/(1+odds)
      
      dataf[k,r]<-rbinom(1,1,prob)
      
    }
    
  }
  
  data<-dataf[(nrow(dataf)-(n-1)):nrow(dataf),]
  
  data1<-cbind(data,1)
  
  
  
  
  
  dataf<-matrix(0,K,p)
  
  dataf[1,]<-rep(1,p)
  
  for (k in 2:K){
    
    for (r in 1:p){
      
      s<-sum(dataf[k,which(G2[r,]==1)])
      
      odds<-exp(beta*s+alpha)
      
      prob<-odds/(1+odds)
      
      dataf[k,r]<-rbinom(1,1,prob)
      
    }
    
  }
  
  data<-dataf[(nrow(dataf)-(n-1)):nrow(dataf),]
  
  data2<-cbind(data,2)
  
  
  
  
  dataf<-matrix(0,K,p)
  
  dataf[1,]<-rep(1,p)
  
  for (k in 2:K){
    
    for (r in 1:p){
      
      s<-sum(dataf[k,which(G3[r,]==1)])
      
      odds<-exp(beta*s+alpha)
      
      prob<-odds/(1+odds)
      
      dataf[k,r]<-rbinom(1,1,prob)
      
    }
    
  }
  
  data<-dataf[(nrow(dataf)-(n-1)):nrow(dataf),]
  
  data3<-cbind(data,3)
  
  
  
  ####### FINE DATA
  
  ###### INIZIO REGISTRAZIONE
  
  dataX<-list()
  
  dataX[[1]]<-data0[,1:p]
  dataX[[2]]<-data1[,1:p]
  dataX[[3]]<-data2[,1:p]
  dataX[[4]]<-data3[,1:p]
  
  
  
  DATA[[P]]<-dataX
  
  
}

GX<-list()
GX[[1]]<-G0
GX[[2]]<-G1
GX[[3]]<-G2
GX[[4]]<-G3

MX.test<-list()


MX.test[[1]]<-as.vector(t(G0[upper.tri(G0)]))
MX.test[[2]]<-as.vector(t(G1[upper.tri(G1)]))
MX.test[[3]]<-as.vector(t(G2[upper.tri(G2)]))
MX.test[[4]]<-as.vector(t(G3[upper.tri(G3)]))


######FINE REGISTRAZIONE

for (P in 4:10){
  
  
  dataX<-list()
  
  dataX[[1]]<-DATA[[P]][[1]]
  dataX[[2]]<-DATA[[P]][[2]]
  dataX[[3]]<-DATA[[P]][[3]]
  dataX[[4]]<-DATA[[P]][[4]]
  
  
  
  #### SEPlogit ####
  
  print("SL"); print(P)
  
  library(glmnet)
  
  
  err<-matrix(0,1,2)
  
  
  
  for(x in 1:X){
    
    ppar<-matrix(0,p,p)
    
    a<-1
    
    for (i in 1:p) {
      
      
      
      # Perform 10-fold cross-validation to select lambda ---------------------------
      lambdas_to_try <- seq(0.001,0.2, length.out = 500)
      
      
      # Use information criteria to select lambda -----------------------------------
      aic <- c()
      bic <- c()
      aic2<- c()
      bic2<- c()
      for (lambda in seq(lambdas_to_try)) {
        # Run model
        mod<-glmnet(dataX[[x]][,-i],dataX[[x]][,i],lambda = lambdas_to_try[lambda],family = "binomial")
        
        # Extract coefficients and residuals (remove first row for the intercept)
        #betas <- as.vector((as.matrix(coef(mod))[-1, ]))
        #resid <- dataX[[x]][,i] - (dataX[[x]][,-i] %*% betas)
        
        # Compute information criteria
        #aic[lambda] <- nrow(dataX[[x]][,-i]) * log((t(resid) %*% resid)/nrow(dataX[[x]][,-i])) + 2 * (sum(betas!=0))
        #bic[lambda] <- nrow(dataX[[x]][,-i]) * log((t(resid) %*% resid)/nrow(dataX[[x]][,-i])) + log(nrow(dataX[[x]][,-i])) * (sum(betas!=0))
        
        
        tLL <- - deviance(mod)
        kk <- mod$df
        nn <- mod$nobs
        aic2[lambda] <- -tLL+2*kk
        bic2[lambda]<- -tLL+log(nn)*kk
        
        
      }
      
      
      # Optimal lambdas according to both criteria
      lambda_aic <- lambdas_to_try[which.min(aic)]
      lambda_bic <- lambdas_to_try[which.min(bic)]
      lambda_aic2 <- lambdas_to_try[which.min(aic2)]
      lambda_bic2 <- lambdas_to_try[which.min(bic2)]
      
      mod<-glmnet(dataX[[x]][,-i],dataX[[x]][,i],lambda = lambda_bic2,family = "binomial")
      
      par<-coef(mod,lambda_bic2)
      
      ppar[i,i]<-par[1]
      
      par<-as.vector(par[-1])
      
      indic<-0
      
      for (z in 1:(p-1)){
        
        if (ppar[i,z]==0 & indic==0){
          
          ppar[i,z]<-par[z]
          
        } else {
          
          ppar[i,z+1]<-par[z]
          
          indic<-1
        }
        
      }
      
      for (j in 1:p){
        
        if (ppar[i,j]!=0){
          
          ppar[i,j]<-1
          
        }
        
        if (i==j){
          
          ppar[i,j]<-0
          
        }
        
      }
      
    }
    
    for (i in 1:p){
      for (j in 1:p){
        if (j>i){
          
          if (ppar[i,j]+ppar[j,i]<2){
            
            ppar[i,j]<-0
            ppar[j,i]<-0
            
            
            
          }
          
        }
        
      }
      
    }
    
    predSL[[x]][P,]<-as.vector(t(ppar[upper.tri(ppar)]))
    ER<-ppar-GX[[x]]
    
    FP<-(length(which(ER==1))/2)
    FN<-(length(which(ER==-1))/2)
    
    err<-rbind(err,c(FN,FP))
    
    
  }
  
  
  
  
  err<-err[-1,]
  
  
  
  
  
  RSL[P,]<-apply(err,2,mean)
  
  aucSL<-c()
  
  for (x in 1:X){
    
    aucc<-roc(predSL[[x]][P,],as.vector(t(GX[[x]][upper.tri(GX[[x]])])))
    
    aucSL[x]<-aucc$auc
    
  }
  
  AUCSL[P]<-mean(aucSL)
  
  #####################
  
  #### DataShared-SEPlogit ####
  
  print("DSSL"); print(P)
  
  
  RES<-matrix(0,1,2)
  
  
  par<-matrix(0,(p-1)*(X+1),p)
  
  for(i in 1:p){
    
    Y<-9
    
    for (x in 1:X){
      
      Y<-c(Y,dataX[[x]][,i])
      
    }
    
    Y<-Y[-1]
    
    
    
    a<-1
    
    
    XX1<-rep(0,p-1)
    
    
    datastX<-list()
    
    datastX[[1]]<-dataX[[1]]
    datastX[[2]]<-dataX[[2]]
    datastX[[3]]<-dataX[[3]]
    datastX[[4]]<-dataX[[4]]
    
    
    for(x in 1:X){
      
      for(r in 1:p){
        
        datastX[[x]][,r]<-scale(dataX[[x]][,r])
        
      }
      
    }
    
    
    
    for (v in 1:X){
      
      data2<-datastX[[v]][,-i]
      
      XX1<-rbind(XX1,data2)
      
    }
    
    XX1<-XX1[-1,]
    
    
    XX2<-list()
    
    for (v in 1:X){
      
      data2<-datastX[[v]][,-i]
      
      d<-rep(0,X)
      
      d[v]<-1
      
      XX2[[v]]<-kronecker(diag(d),data2)
      
    }
    
    XXb<-matrix(0,n*X,(p-1)*X)
    
    for (v in 1:X){
      
      XXb<-XXb+XX2[[v]]
      
    }
    
    XXb<-XXb*(1/sqrt(X))
    
    XX<-cbind(XX1,XXb)
    
    
    
    
    # Perform 10-fold cross-validation to select lambda ---------------------------
    lambdas_to_try <- seq(0.001,0.2, length.out = 500)
    
    
    # Use information criteria to select lambda -----------------------------------
    aic <- c()
    bic <- c()
    aic2<- c()
    bic2<- c()
    for (lambda in seq(lambdas_to_try)) {
      # Run model
      mod<-glmnet(XX,Y,lambda = lambdas_to_try[lambda],family = "binomial")
      
      
      # Extract coefficients and residuals (remove first row for the intercept)
      #betas <- as.vector((as.matrix(coef(mod))[-1, ]))
      #resid <- Y - (XX %*% betas)
      # Compute information criteria
      #aic[lambda] <- nrow(XX) * log((t(resid) %*% resid)/nrow(XX)) + 2 * (sum(betas!=0))
      #bic[lambda] <- nrow(XX) * log((t(resid) %*% resid)/nrow(XX)) + log(nrow(XX)) * (sum(betas!=0))
      
      tLL <- - deviance(mod)
      kk <- mod$df
      nn <- mod$nobs
      aic2[lambda] <- -tLL+2*kk
      bic2[lambda]<- -tLL+log(nn)*kk
      
    }
    
    
    # Optimal lambdas according to both criteria
    #lambda_aic <- lambdas_to_try[which.min(aic)]
    #lambda_bic <- lambdas_to_try[which.min(bic)]
    
    lambda_aic2 <- lambdas_to_try[which.min(aic2)]
    lambda_bic2 <- lambdas_to_try[which.min(bic2)]
    
    
    mod<-glmnet(XX,Y,lambda = lambda_bic2,family = "binomial")
    
    par[,i]<-coef(mod,lambda_bic2)[2:((p-1)*(X+1)+1)]
    
    
    
  }
  
  
  
  TEST<-list()
  
  for (d in 1:X){
    
    par2<-par
    
    PAR<-list()
    
    for (x in 0:X){
      
      PAR[[(x+1)]]<-par2[(1+(p-1)*x):((p-1)+(p-1)*x),]
      
    }
    
    PAR2<-list()
    
    
    for (x in 1:X){
      
      PAR2[[(x+1)]]<-PAR[[1]]+ PAR[[(x+1)]]
      
    }
    
    
    mat<-matrix(0,p,p)
    
    
    
    for (i in 1:p) {
      
      for (j in 1:p) {
        
        
        if (j<i){
          
          mat[j,i]<-PAR2[[d+1]][j,i]
          
        } else if (j==i){
          
          mat[j,i]<-0
          
          
        } else if (j>i){
          
          mat[j,i]<-PAR2[[d+1]][j-1,i]
          
          
        }
        
        
      }
    }
    
    
    
    mat2<-matrix(0,p,p)
    
    
    for (i in 1:p) {
      
      for (j in 1:(p-1)) {
        
        
        if (mat[j,i]!=0 & mat[i,j]!=0){
          
          mat2[j,i]<-1
          mat2[i,j]<-1
          
          
        } else {
          
          mat2[j,i]<-0
          mat2[i,j]<-0
          
        }
        
        
      }
    }
    
    TEST[[d]]<-mat2
    
  }  
  
  
  AX<-list()
  ER<-matrix(0,X,2)
  
  for (x in 1:X) {
    
    AX[[x]]<-TEST[[x]]-GX[[x]]
    
    predDSSL[[x]][P,]<-as.vector(t(TEST[[x]][upper.tri(TEST[[x]])]))
    
    
    ER[x,1]<-sum(AX[[x]]==-1)/2
    ER[[x,2]]<-(sum(AX[[x]]==1))/2
  } 
  
  
  
  RES<-apply(ER,2,mean)
  
  
  
  
  aucDSSL<-c()
  
  for (x in 1:X){
    
    aucc<-roc(predDSSL[[x]][P,],as.vector(t(GX[[x]][upper.tri(GX[[x]])])))
    
    aucDSSL[x]<-aucc$auc
    
  }
  
  AUCDSSL[P]<-mean(aucDSSL)
  
  
  
  RDSSL[P,]<- RES
  
  
  
  
  
  
  
  
  # End
  
  ########################################################################################
  
  
  
  
  
  
  
  
  
  ##### Bayesian aprox link
  
  print("BAL"); print(P)
  
  
  ### Bayesian log-linear model selection for multiple graphs ###
  
  library(MASS)
  
  logl.r<- function(data,delta.r,L.dr,L.drc,r){ # quasi-likelihood nodo r-esimo
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    datacn<-data
    datacn[,r]<-1
    logli.ri <- diag(data[,r])%*%(data %*% L.dr) - log(1+exp(datacn %*% L.dr))
    logli.r<-sum(logli.ri)
    return(logli.r)
  }
  
  
  grad.h.r<- function(data,delta.r,L.dr,L.drc,r){ # gradiente funzione h nodo r-esimo
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc) 
    grad<-rep(0,p)
    datacn<-data
    datacn[,r]<-1
    datacn1<-datacn
    datacn1[,which(delta.r==0)]<-0
    datacn1<-t(datacn1)
    data1<-data
    data1[,which(delta.r==0)]<-0
    data1<-diag(data[,r])%*%data1
    grad<- apply(data1,2,sum) - datacn1%*%(diag(as.vector(exp(datacn %*% L.dr)))%*%as.vector(( 1/ (1+exp(datacn %*% L.dr)) )))
    grad<-grad-(L.dr/rho)-(L.drc/gamma) 
    return(grad)
  }
  
  
  h.r<-function(data,rho,gamma,delta.r,L.dr,L.drc,r){ # funzione h_r
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    h<-logl.r(data,delta.r,L.dr,L.drc,r)-(sum(L.dr^2)/(2*rho)) - (sum(L.drc^2)/(2*gamma))
    return(h)
  }
  
  
  
  K<-50000
  RES<-matrix(0,K,2)
  
  ### theta and nu start values
  
  nu<-matrix(0,p,p)
  nuK<-list()
  nuK[[1]]<-nu
  
  aa<-1
  bb<-2
  aap<-aa
  bbp<-bb
  
  
  theta<-matrix(0,X,X)
  diag(theta)<-0
  w<-0.6
  alpha<-1
  chi<-2
  de0<-1
  
  epsilon<-matrix(0,X,X)
  diag(epsilon)<-0
  
  
  epsilonK<-list()
  thetaK<-list()
  
  epsilonK[[1]]<-epsilon
  thetaK[[1]]<-theta
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  pdelta.r<-function(k,x,r,nu,the,DX){
    
    
    for (j in 1:p){
      
      DXrest<-c()
      
      for (z in 1:X){
        
        DXrest[z]<-DX[[z]][[r]][k,j]
        
      }
      
      odds<-exp( DX[[x]][[r]][k,j]%*%(nu[r,j]+2*the[x,]%*%DXrest ) )
      
      prob<-odds/(1+odds)
      
      q[j]<-prob
      
    }
    
    PROB<-prod(q)
    
    return(PROB)
    
  }
  
  
  
  
  
  
  lpost.r<-function(data,omega.r,gamma,rho,delta.r,L.dr,L.drc,r){ # log quasi-posterior nodo r
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    lpost<- log(omega.r) + log( (sqrt(gamma/rho))^sum(delta.r) ) + logl.r(data,delta.r,L.dr,L.drc,r)- ( sum(L.dr^2)/(2*rho) ) -
      ( sum(L.drc^2)/(2*gamma) )
    return(lpost)
  }
  
  
  G.r <- function(data,delta.r,L.dr,L.drc,r){ # funzione G del gradiente troncato h_r
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    c<- 1
    G <- ( c/( max(c,sqrt(sum(grad.h.r(data,delta.r,L.dr,L.drc,r)^2))) ) ) * grad.h.r(data,delta.r,L.dr,L.drc,r) # gradiente troncato
    return(G)
  }
  
  
  
  
  
  #u<-1.5
  #q<- p^-(1+u)
  #c0<- 1
  #c1<- 1
  #gamma<-c0/max(n,p)
  #rho<-c1*sqrt(n/log(p))
  
  
  q<-0.5
  gamma<-0.5
  rho<-2
  sigma<-2 # costante step size?
  D.r<-matrix(0,K,p)
  La.dr<-matrix(0,K,p)
  La.drc<-matrix(0,K,p)
  delta.r<-rep(0,p)
  delta.r[1:(p/2)]<-1
  L.dr<-delta.r
  L.drc<-1-L.dr
  
  
  
  
  D<-list(c())
  L<-list(c())
  Lc<-list(c())
  
  
  for (r in 1:p){
    D[[r]]<-matrix(0,K,p)
    L[[r]]<-matrix(0,K,p)
    Lc[[r]]<-matrix(0,K,p)
    
  }
  
  
  
  
  
  DX<-list()
  
  for (x in 1:X){
    
    DX[[x]]<-D
    
  }
  
  for (x in 1:X){
    
    for (r in 1:p){
      
      DX[[x]][[r]][1,r]<-1
      
    }
    
  }
  
  LX<-list()
  
  for (x in 1:X){
    
    LX[[x]]<-L
    
  }
  
  LcX<-list()
  
  for (x in 1:X){
    
    LcX[[x]]<-Lc
    
  }
  
  L.trueX<-list()
  
  #for (x in 1:X){
  
  #  L.trueX[[x]]<-L.true
  
  #}
  
  L.trueX[[1]]<-G0
  L.trueX[[2]]<-G1
  L.trueX[[3]]<-G2
  L.trueX[[4]]<-G3
  #L.trueX[[5]]<-G4
  #L.trueX[[6]]<-G5
  #L.trueX[[7]]<-G6
  #L.trueX[[8]]<-G7
  #L.trueX[[9]]<-G8
  #L.trueX[[10]]<-G9
  
  
  
  AX<-list()
  RESX<-list()
  
  for (x in 1:X) {
    
    AX[[x]]<-matrix(0,p,p)
    RESX[[x]]<-matrix(0,K,2)
  } 
  
  
  
  
  OMEGA.R<-c()
  
  
  
  
  for (k in 2:K){
    
    thetaK[[k]]<-thetaK[[k-1]]
    epsilonK[[k]]<-epsilonK[[k-1]]
    nuK[[k]]<-nuK[[k-1]]
    
    
    for (r in 1:p){
      
      for (x in 1:X) {
        
        data<-dataX[[x]]   
        L.dr<-LX[[x]][[r]][k-1,]
        L.drc<-LcX[[x]][[r]][k-1,]
        delta.r<-DX[[x]][[r]][k-1,]
        omega.r<-pdelta.r(k,x,r,nuK[[k]],thetaK[[k]],DX)
        
        OMEGA.R[k]<-omega.r
        
        for (j in 1:p){ # per ogni parametro
          
          if (delta.r[j]==1){ # se il parametro è attivo
            
            L.drj.p<-rnorm(1,L.dr[j],sigma) # estraggo un valore dalla distribuzione g
            L.drp<-L.dr
            L.drp[j]<-L.drj.p
            acc <- min(0, dnorm(L.dr[j],L.drp[j]+(sigma/2)*grad.h.r(data,delta.r,L.drp,L.drc,r)[j],sigma,log=TRUE) - dnorm(L.drp[j],L.dr[j]+(sigma/2)*grad.h.r(data,delta.r,L.dr,L.drc,r)[j],sigma, log=TRUE)  +
                         lpost.r(data,omega.r,gamma,rho,delta.r,L.drp,L.drc,r) - lpost.r(data,omega.r,gamma,rho,delta.r,L.dr,L.drc,r) ) 
            
            # + sigma/2)*grad.h.r(data,delta.r,L.drp,L.drc,r)[j]
            # +(sigma/2)*grad.h.r(data,delta.r,L.dr,L.drc,r)[j]
            
            
            if (acc > log(runif(1,0,1))){ # accettazione/rifiuto
              L.dr[j]<- L.drj.p
            }
          }
        }
        
        if (sum(delta.r)!=p){
          
          ############################# AGGIORNAMENTO PARAMETRI INATTIVI ####################################
          
          L.drcp<-mvrnorm(1,rep(0,sum(1-delta.r)),gamma*diag(sum(1-delta.r)))
          L.drc[which((1-delta.r)==1)]<-L.drcp
          
          ############################# VETTORE SELEZIONATORE ##############################################
        }
        
        for (j in 1:p){ # per ogni parametro
          if (j!=r){
            delta.rp<-delta.r
            delta.rp[j]<-1-delta.r[j] # switcho il parametro selezionatore del parametro j
            
            DX[[x]][[r]][k,]<-delta.rp
            
            omega.rp<-pdelta.r(k,x,r,nuK[[k]],thetaK[[k]],DX)
            
            DX[[x]][[r]][k,]<-delta.r
            
            
            b <- lpost.r(data,omega.rp,gamma,rho,delta.rp,L.dr,L.drc,r) - lpost.r(data,omega.r,gamma,rho,delta.r,L.dr,L.drc,r) 
            tau <- min(0,b)
            # calcolo la probabilità di accettare lo switch
            if (tau > log(runif(1,0,1))){ # se estraggo 1 allora switcho il parametro altrimenti no
              delta.r[j]<- 1-delta.r[j]
            }
          }
        }
        # SALVATAGGIO
        DX[[x]][[r]][k,]<-delta.r
        LX[[x]][[r]][k,]<-diag(delta.r)%*%L.dr
        LcX[[x]][[r]][k,]<-diag(1-delta.r)%*%L.drc
        
      } # x cycle
      
    } # r cycle
    
    ############   
    
    for (x in 1:X) {
      
      for (i in 1:p){
        
        L.testXBAL[[P]][[k]][[x]][i,]<-DX[[x]][[i]][k,]
        
      }
      
    }  
    
    ############  
    
    
    print(c(k,1,P))
    
    
    
    # THETA UPDATING
    
    Resp <- c(0L,1L)
    comb <- do.call(expand.grid,lapply(1:X,function(x)Resp)) # All posible states:
    comb<-as.matrix(comb)
    
    
    
    for (u in 1:X){ # coppie indici X
      
      for (h in 1:X){
        
        if (h != u){ # per ogni possibile coppia di livelli di X
          
          
          
          
          if (epsilonK[[k]][u,h]==0) {
            
            
            eps<-1
            the<-rgamma(1,alpha,chi)
            
            Eps<-epsilonK[[k]]
            The<-thetaK[[k]]
            
            Eps[u,h]<-eps
            
            The[u,h]<-the
            
            
            
            Cost<-c()
            Costp<-c()
            
            
            lp.pr.epsilon<-log(dbinom(epsilonK[[k]][u,h],1,w))
            lp.pr.theta<-log((1-epsilonK[[k]][u,h])*de0+epsilonK[[k]][u,h]*dgamma(thetaK[[k]][u,h],shape=alpha,scale=chi))
            lq.km<-log(dgamma(thetaK[[k]][u,h],shape=alpha,scale=chi))
            
            
            lp.pr.epsilonp<-log(dbinom(eps,1,w))
            lp.pr.thetap<-log((1-eps)*de0+eps*dgamma(the,shape=alpha,scale=chi))
            lq.kmp<-log(dgamma(the,shape=alpha,scale=chi))
            
            prod.ij<- matrix(0,p,p)  
            prod.ijp<- matrix(0,p,p)  
            
            
            # produttoria sugli ij
            
            for (i in 1:p){ # coppie indici variabili
              
              for (j in 1:p){
                
                if (j != i){                      
                  
                  ker<-  exp(2*epsilonK[[k]][u,h]*L.testXBAL[[P]][[k]][[u]][i,j]*L.testXBAL[[P]][[k]][[h]][i,j])
                  
                  kerp<- exp(2*eps*L.testXBAL[[P]][[k]][[u]][i,j]*L.testXBAL[[P]][[k]][[h]][i,j])
                  
                  
                  for (l in 1:(2^X)){
                    
                    Cost[l]<-exp(nuK[[k]][i,j]*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%thetaK[[k]]%*%comb[l,])
                    
                    Costp[l]<-exp(nuK[[k]][i,j]*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%The%*%comb[l,])
                    
                  }
                  
                  Cost<-sum(Cost)
                  Costp<-sum(Costp)
                  
                  
                  prod.ij[i,j]<-  log(ker / Cost) 
                  prod.ijp[i,j]<- log(kerp / Costp) 
                  
                  
                }
                
              }
              
            }
            
            
            prod.ij<-sum(prod.ij)
            
            lp.post<- prod.ij + lp.pr.theta + lp.pr.epsilon
            
            prod.ijp<-sum(prod.ijp)
            
            lp.postp<- prod.ijp + lp.pr.thetap + lp.pr.epsilonp
            
            
            
            rrr<- lp.postp + lq.km - lp.post
            
            acc <- min(0,rrr)
            
            
            if ( acc > log(runif(1,0,1))){
              
              epsilonK[[k]][u,h]<-eps
              
              thetaK[[k]][u,h]<-the
              
            }
            
            
            
          } else { # epsilon = 1
            
            
            eps<-0
            the<-0
            
            Eps<-epsilonK[[k]]
            The<-thetaK[[k]]
            
            Eps[u,h]<-eps
            
            The[u,h]<-the
            
            
            
            Cost<-c()
            Costp<-c()
            
            
            lp.pr.epsilon<-log(dbinom(epsilonK[[k]][u,h],1,w))
            lp.pr.theta<-log((1-epsilonK[[k]][u,h])*de0+epsilonK[[k]][u,h]*dgamma(thetaK[[k]][u,h],shape=alpha,scale=chi))
            lq.km<-log(dgamma(thetaK[[k]][u,h],shape=alpha,scale=chi))
            
            
            lp.pr.epsilonp<-log(dbinom(eps,1,w))
            lp.pr.thetap<-log((1-eps)*de0+eps*dgamma(the,shape=alpha,scale=chi))
            lq.kmp<-log(dgamma(the,shape=alpha,scale=chi))
            
            prod.ij<- matrix(0,p,p)  
            prod.ijp<- matrix(0,p,p)  
            
            
            # produttoria sugli ij
            
            for (i in 1:p){ # coppie indici variabili
              
              for (j in 1:p){
                
                if (j != i){
                  
                  ker<-  exp(2*epsilonK[[k]][u,h]*L.testXBAL[[P]][[k]][[u]][i,j]*L.testXBAL[[P]][[k]][[h]][i,j])
                  
                  kerp<- exp(2*eps*L.testXBAL[[P]][[k]][[u]][i,j]*L.testXBAL[[P]][[k]][[h]][i,j])
                  
                  
                  for (l in 1:(2^X)){
                    
                    Cost[l]<-exp(nuK[[k]][i,j]*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%thetaK[[k]]%*%comb[l,])
                    
                    Costp[l]<-exp(nuK[[k]][i,j]*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%The%*%comb[l,])
                    
                  }
                  
                  Cost<-sum(Cost)
                  Costp<-sum(Costp)
                  
                  
                  prod.ij[i,j]<-  log(ker / Cost) 
                  prod.ijp[i,j]<- log(kerp / Costp) 
                  
                  
                }
                
              }
              
            }
            
            
            prod.ij<-sum(prod.ij)
            
            lp.post<- prod.ij + lp.pr.theta + lp.pr.epsilon
            
            prod.ijp<-sum(prod.ijp)
            
            lp.postp<- prod.ijp + lp.pr.thetap + lp.pr.epsilonp
            
            
            
            rrr<- lp.postp + lq.km - lp.post
            
            acc <- min(0,rrr)
            
            
            if ( acc > log(runif(1,0,1))){
              
              epsilonK[[k]][u,h]<-eps
              
              thetaK[[k]][u,h]<-the
              
            }
            
          }
          
          
          
          if (epsilonK[[k]][u,h]==1){ #  se epsilon=1 propongo nuovo valore di theta
            
            
            the<-rgamma(1,alpha,chi)
            
            The<-thetaK[[k]]
            
            The[u,h]<-the
            
            
            
            Cost<-c()
            Costp<-c()
            
            
            lp.pr.epsilon<-log(dbinom(epsilonK[[k]][u,h],1,w))
            lp.pr.theta<-log((1-epsilonK[[k]][u,h])*de0+epsilonK[[k]][u,h]*dgamma(thetaK[[k]][u,h],shape=alpha,scale=chi))
            lq.km<-log(dgamma(thetaK[[k]][u,h],shape=alpha,scale=chi))
            
            
            lp.pr.epsilonp<-log(dbinom(epsilonK[[k]][u,h],1,w))
            lp.pr.thetap<-log((1-epsilonK[[k]][u,h])*de0+epsilonK[[k]][u,h]*dgamma(the,shape=alpha,scale=chi))
            lq.kmp<-log(dgamma(the,shape=alpha,scale=chi))
            
            prod.ij<- matrix(0,p,p)  
            prod.ijp<- matrix(0,p,p)  
            
            
            # produttoria sugli ij
            
            for (i in 1:p){ # coppie indici variabili
              
              for (j in 1:p){
                
                if (j != i){
                  
                  ker<-  exp(2*epsilonK[[k]][u,h]*L.testXBAL[[P]][[k]][[u]][i,j]*L.testXBAL[[P]][[k]][[h]][i,j])
                  
                  kerp<- exp(2*epsilonK[[k]][u,h]*L.testXBAL[[P]][[k]][[u]][i,j]*L.testXBAL[[P]][[k]][[h]][i,j])
                  
                  
                  for (l in 1:(2^X)){
                    
                    Cost[l]<-exp(nuK[[k]][i,j]*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%thetaK[[k]]%*%comb[l,])
                    
                    Costp[l]<-exp(nuK[[k]][i,j]*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%The%*%comb[l,])
                    
                  }
                  
                  Cost<-sum(Cost)
                  Costp<-sum(Costp)
                  
                  
                  prod.ij[i,j]<-  log(ker / Cost) 
                  prod.ijp[i,j]<- log(kerp / Costp) 
                  
                  
                }
                
              }
              
            }
            
            
            prod.ij<-sum(prod.ij)
            
            lp.post<- prod.ij + lp.pr.theta + lp.pr.epsilon
            
            prod.ijp<-sum(prod.ijp)
            
            lp.postp<- prod.ijp + lp.pr.thetap + lp.pr.epsilonp
            
            
            
            rrr<- lp.postp + lq.km  - lp.post - lq.kmp
            
            acc <- min(0,rrr)
            
            
            if ( acc > log(runif(1,0,1))){
              
              thetaK[[k]][u,h]<-the
            }
            
          }
          
          
          
          
        } 
      }
    } 
    
    
    
    
    
    ############# UPDATING NUij
    
    
    nup<- rbeta(1,aap,bbp)
    
    nup<-log(nup/(1-nup))
    
    # produttoria sugli ij
    
    for (i in 1:p){ # coppie indici variabili
      
      for (j in 1:p){
        
        if (j != i){
          
          deltaij<-c()
          
          for(t in 1:X){
            
            deltaij[t]<-L.testXBAL[[P]][[k]][[t]][i,j]
            
          }
          
          
          for (l in 1:(2^X)){
            
            Cost[l]<-exp(nuK[[k]][i,j]*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%thetaK[[k]]%*%comb[l,])
            
            Costp[l]<-exp(nup*t(rep(1,X))%*%comb[l,]+t(comb[l,])%*%thetaK[[k]]%*%comb[l,])
            
          }
          
          Cost<-sum(Cost)
          Costp<-sum(Costp)
          
          
          pnu.ij<- exp(aap*nuK[[k]][i,j])/((1+exp(nuK[[k]][i,j]))^(aa+bb)) *  Cost^-1 * exp(nuK[[k]][i,j]*t(rep(1,X))%*%deltaij)
          pnu.ijp<- exp(aap*nup)/((1+exp(nup))^(aap+bbp)) * Costp^-1 * exp(nup*t(rep(1,X))%*%deltaij)
          
          q.nuij <- beta(aa,bb)^-1 * exp(aa*nuK[[k]][i,j])/((1+exp(nuK[[k]][i,j]))^(aa+bb))
          q.nuijp<- beta(aap,bbp)^-1 * exp(aap*nup)/((1+exp(nup))^(aap+bbp))
          
          
          r<-log(pnu.ijp)+log(q.nuij)-log(pnu.ij)-log(q.nuijp)
          
          acc <- min(0,r)
          
          
          if ( acc > log(runif(1,0,1))){
            
            nuK[[k]][i,j]<-nup
            
          }
        }
        
      }
      
    }
    
    
    
  }# K cycle
  
  
  
  
  ###############################################################
  
  
  ######### BAyesian approx no link
  
  
  print("BA"); print(P)
  
  
  ### Bayesian log-linear model selection for multiple graphs ###
  
  library(MASS)
  
  logl.r<- function(data,delta.r,L.dr,L.drc,r){ # quasi-likelihood nodo r-esimo
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    datacn<-data
    datacn[,r]<-1
    logli.ri <- diag(data[,r])%*%(data %*% L.dr) - log(1+exp(datacn %*% L.dr))
    logli.r<-sum(logli.ri)
    return(logli.r)
  }
  
  
  grad.h.r<- function(data,delta.r,L.dr,L.drc,r){ # gradiente funzione h nodo r-esimo
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc) 
    grad<-rep(0,p)
    datacn<-data
    datacn[,r]<-1
    datacn1<-datacn
    datacn1[,which(delta.r==0)]<-0
    datacn1<-t(datacn1)
    data1<-data
    data1[,which(delta.r==0)]<-0
    data1<-diag(data[,r])%*%data1
    grad<- apply(data1,2,sum) - datacn1%*%(diag(as.vector(exp(datacn %*% L.dr)))%*%as.vector(( 1/ (1+exp(datacn %*% L.dr)) )))
    grad<-grad-(L.dr/rho)-(L.drc/gamma) 
    return(grad)
  }
  
  
  h.r<-function(data,rho,gamma,delta.r,L.dr,L.drc,r){ # funzione h_r
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    h<-logl.r(data,delta.r,L.dr,L.drc,r)-(sum(L.dr^2)/(2*rho)) - (sum(L.drc^2)/(2*gamma))
    return(h)
  }
  
  
  lpost.r<-function(data,gamma,rho,delta.r,L.dr,L.drc,r){ # log quasi-posterior nodo r
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    omega.r<-(q^(sum(delta.r)))*((1-q)^(p-sum(delta.r))) # calcolo la probabilità del vettore selezionatore generato
    lpost<- log(omega.r) + log( (sqrt(gamma/rho))^sum(delta.r) ) + logl.r(data,delta.r,L.dr,L.drc,r)- ( sum(L.dr^2)/(2*rho) ) -
      ( sum(L.drc^2)/(2*gamma) )
    return(lpost)
  }
  
  
  
  G.r <- function(data,delta.r,L.dr,L.drc,r){ # funzione G del gradiente troncato h_r
    L.dr<-diag(delta.r)%*%(L.dr+L.drc)
    L.drc<-diag(1-delta.r)%*%(L.dr+L.drc)
    c<- 1
    G <- ( c/( max(c,sqrt(sum(grad.h.r(data,delta.r,L.dr,L.drc,r)^2))) ) ) * grad.h.r(data,delta.r,L.dr,L.drc,r) # gradiente troncato
    return(G)
  }
  
  p<-10
  q<-0.35
  gamma<-0.5
  rho<-2
  sigma<-2 # costante step size?
  D.r<-matrix(0,K,p)
  La.dr<-matrix(0,K,p)
  La.drc<-matrix(0,K,p)
  delta.r<-rep(0,p)
  delta.r[1:(p/2)]<-1
  L.dr<-delta.r
  L.drc<-1-L.dr
  
  
  D<-list(c())
  L<-list(c())
  Lc<-list(c())
  
  
  for (r in 1:p){
    D[[r]]<-matrix(0,K,p)
    L[[r]]<-matrix(0,K,p)
    Lc[[r]]<-matrix(0,K,p)
    
  }
  
  
  DX<-list()
  
  for (x in 1:X){
    
    DX[[x]]<-D
    
  }
  
  for (x in 1:X){
    
    for (r in 1:p){
      
      DX[[x]][[r]][1,r]<-1
      
    }
    
  }
  
  LX<-list()
  
  for (x in 1:X){
    
    LX[[x]]<-L
    
  }
  
  LcX<-list()
  
  for (x in 1:X){
    
    LcX[[x]]<-Lc
    
  }
  
  
  for (k in 2:K){
    
    for (r in 1:p){
      
      for (x in 1:X) {
        
        data<-dataX[[x]]   
        L.dr<-LX[[x]][[r]][k-1,]
        L.drc<-LcX[[x]][[r]][k-1,]
        delta.r<-DX[[x]][[r]][k-1,]
        
        for (j in 1:p){ # per ogni parametro
          
          if (delta.r[j]==1){ # se il parametro è attivo
            
            L.drj.p<-rnorm(1,L.dr[j],sigma) # estraggo un valore dalla distribuzione g
            L.drp<-L.dr
            L.drp[j]<-L.drj.p
            acc <- min(0, dnorm(L.dr[j],L.drp[j]+(sigma/2)*grad.h.r(data,delta.r,L.drp,L.drc,r)[j],sigma,log=TRUE) - dnorm(L.drp[j],L.dr[j]+(sigma/2)*grad.h.r(data,delta.r,L.dr,L.drc,r)[j],sigma, log=TRUE)  +
                         lpost.r(data,gamma,rho,delta.r,L.drp,L.drc,r) - lpost.r(data,gamma,rho,delta.r,L.dr,L.drc,r) ) 
            
            # + sigma/2)*grad.h.r(data,delta.r,L.drp,L.drc,r)[j]
            # +(sigma/2)*grad.h.r(data,delta.r,L.dr,L.drc,r)[j]
            
            
            if (acc > log(runif(1,0,1))){ # accettazione/rifiuto
              L.dr[j]<- L.drj.p
            }
          }
        }
        
        
        if (sum(delta.r)!=p){
          
          ############################# AGGIORNAMENTO PARAMETRI INATTIVI ####################################
          L.drcp<-mvrnorm(1,rep(0,sum(1-delta.r)),gamma*diag(sum(1-delta.r)))
          L.drc[which((1-delta.r)==1)]<-L.drcp
          ############################# VETTORE SELEZIONATORE ##############################################
        }
        
        
        
        ############################# VETTORE SELEZIONATORE ##############################################
        for (j in 1:p){ # per ogni parametro
          if (j!=r){
            delta.rp<-delta.r
            delta.rp[j]<-1-delta.r[j] # switcho il parametro selezionatore del parametro j
            b<- log( (q/(1-q))^(sum(delta.rp)) ) + log( (gamma/rho)^( (sum(delta.rp))/2 ) ) + h.r(data,rho,gamma,delta.rp,L.dr,L.drc,r) -
              log( (q/(1-q))^(sum(delta.r))  ) - log( (gamma/rho)^( (sum(delta.r))/2 ) )  - h.r(data,rho,gamma,delta.r,L.dr,L.drc,r)
            tau <- min(0,b)
            # calcolo la probabilità di accettare lo switch
            if (tau > log(runif(1,0,1))){ # se estraggo 1 allora switcho il parametro altrimenti no
              delta.r[j]<- 1-delta.r[j]
            }
          }
        }
        # SALVATAGGIO
        DX[[x]][[r]][k,]<-delta.r
        LX[[x]][[r]][k,]<-diag(delta.r)%*%L.dr
        LcX[[x]][[r]][k,]<-diag(1-delta.r)%*%L.drc
        
      } # x cycle
      
    } # r cycle
    
    ############   
    
    for (x in 1:X) {
      
      for (i in 1:p){
        
        L.testXBA[[P]][[k]][[x]][i,]<-DX[[x]][[i]][k,]
        
      }
      
    } 
    
    ############  
    
    
    print(c(k,1,P))
    
  } # k cycle
  
} #  P CYCLE


















RBEL<-matrix(0,10,2)

aucBEL<-c()

FP<-c()
FN<<-c()

sumbal<-list()

for(x in 1:X){
  
  sumbal[[x]]<-matrix(0,p,p)
  
  
}



for(x in 1:X){
  
  for(k in 20001:25000){
    
    sumbal[[x]]<-sumbal[[x]]+L.testXBAL[[P]][[k]][[x]]
    
    
    
  }
  
}


MCCBEL<-c()


for (P in 1:10){
  
  MCCBEL[P]<- ((9-RBEL[P,1])*(36-RBEL[P,2])-RBEL[P,2]*RBEL[P,1])/sqrt( ((9-RBEL[P,1])+RBEL[P,2]) * ((9-RBEL[P,1])+RBEL[P,1]) * ((36-RBEL[P,2])+RBEL[P,2]) * ((36-RBEL[P,2])+RBEL[P,1]) )
  
}





RBE<-matrix(0,4,2)

aucBE<-c()

FP<-c()
FN<-c()

for(P in 1:4){
  
  for(x in 1:X){
    
    
    sssum<-apply(PREDBE[[x]][[P]][2000:2500,],2,sum)/501
    
    predd<-rep(0,45)
    
    predd[which(sssum>=0.5)]<-1
    
    
    FP[x]<-length(which(predd-MX.test[[x]]==1))
    FN[x]<-length(which(predd-MX.test[[x]]==-1))
    
    
    rr<-roc(predd,MX.test[[x]])
    
    aucBE[x]<-rr$auc
    
  }
  
  AUCBE[P]<-mean(aucBE)
  RBE[P,]<-c(mean(FN),mean(FP))
}



MCCBE<-c()


for (P in 1:4){
  
  MCCBE[P]<- ((9-RBE[P,1])*(36-RBE[P,2])-RBE[P,2]*RBE[P,1])/sqrt( ((9-RBE[P,1])+RBE[P,2]) * ((9-RBE[P,1])+RBE[P,1]) * ((36-RBE[P,2])+RBE[P,2]) * ((36-RBE[P,2])+RBE[P,1]) )
  
}





RBAL<-matrix(0,1,2)

aucBAL<-c()

FN<-c()
FP<-c()

for(P in 1:1){
  
  for(x in 1:X){
    
    
    sssum<-apply(PREDBAL[[x]][[P]][20000:25000,],2,sum)/5001
    
    predd<-rep(0,45)
    
    predd[which(sssum>0.5)]<-1
    
    
    FP[x]<-length(which(predd-MX.test[[x]]==1))
    FN[x]<-length(which(predd-MX.test[[x]]==-1))
    
    
    rr<-roc(predd,MX.test[[x]])
    
    aucBAL[x]<-rr$auc
    
  }
  
  AUCBAL[P]<-mean(aucBAL)
  
  RBAL[P,]<-c(mean(FN),mean(FP))
}







RBA<-matrix(0,4,2)

aucBA<-c()

for(P in 1:1){
  
  for(x in 1:X){
    
    
    sssum<-apply(PREDBA[[x]][[P]][20000:25000,],2,sum)/5001
    
    predd<-rep(0,45)
    
    predd[which(sssum>0.5)]<-1
    
    FP[x]<-length(which(predd-MX.test[[x]]==1))
    FN[x]<-length(which(predd-MX.test[[x]]==-1))
    
    
    rr<-roc(predd,MX.test[[x]])
    
    aucBA[x]<-rr$auc
    
  }
  
  AUCBA[P]<-mean(aucBA)
  
  RBA[P,]<-c(mean(FN),mean(FP))
}



MCCBAL<-c()


for (P in 1:2){
  
  MCCBAL[P]<- ((9-RBAL[P,1])*(36-RBAL[P,2])-RBAL[P,2]*RBAL[P,1])/sqrt( ((9-RBAL[P,1])+RBAL[P,2]) * ((9-RBAL[P,1])+RBAL[P,1]) * ((36-RBAL[P,2])+RBAL[P,2]) * ((36-RBAL[P,2])+RBAL[P,1]) )
  
}


MCCBA<-c()


for (P in 1:2){
  
  MCCBA[P]<- ((9-RBA[P,1])*(36-RBA[P,2])-RBA[P,2]*RBA[P,1])/sqrt( ((9-RBA[P,1])+RBA[P,2]) * ((9-RBA[P,1])+RBA[P,1]) * ((36-RBA[P,2])+RBA[P,2]) * ((36-RBA[P,2])+RBA[P,1]) )
  
}






boxplot(MCCSL,MCCDSSL,MCCBA,MCCBAL,ylim=c(0.5,1),ylab="MCC",las=2,names=c("SL","DSSL","BA","BAL"),main="(A)")

+boxplot(AUCSL,AUCDSSL,AUCBA,AUCBAL,ylim=c(0.5,1),ylab="AUC",las=2,names=c("SL","DSSL","BA","BAL"),main="(A)")



boxplot(RBE[1:10,1],RBEL[1:10,1],ylim=c(0,6),ylab="FN",las=2,names=c("BE","BEL"),main="(A)")

boxplot(RBE[1:10,2],RBEL[1:4,2],ylim=c(0,6),ylab="FP",las=2,names=c("BE","BEL"),main="(A)")



boxplot(RBA[1:4,1],RBAL[1:4,1],ylim=c(0,6),ylab="FN",las=2,names=c("BA","BAL"),main="(A)")

boxplot(RBA[1:4,2],RBAL[1:4,2],ylim=c(0,5),ylab="FP",las=2,names=c("BA","BAL"),main="(A)")


apply(RBA,2,mean)
apply(RBAL,2,mean)


apply(RBE[1:6,],2,mean)
apply(RBEL[1:6,],2,mean)

apply(RSL,2,mean)



ssssum<-matrix(0,4,4)

for (k in 20000:25000){
  
  ssssum1<-matrix(0,4,4)
  
  ssssum1[which(thetaK[[k]]!=0)]<-1
  
  ssssum<-ssssum+ssssum1
  
}

ssssum<-ssssum/5001

round(ssssum)


boxplot((9-RSL[1:10,1])/9,(9-RDSSL[1:10,1])/9,(9-RBA[1:10,1])/9,(9-RBAL[1:10,1])/9,(9-RBE[1:10,1])/9,(9-RBEL[1:10,1])/9,ylim=c(0.5,1),ylab="TPR",las=2,names=c("SL","DSSL","BA","BAL","BE","BEL"),main="(A)")




boxplot((RSL[1:10,2])/36,(RDSSL[1:10,2])/36,(RBA[1:10,2])/36,(RBAL[1:10,2])/36,(RBE[1:10,2])/36,(RBEL[1:10,2])/36,ylim=c(0,0.2),ylab="FPR",las=2,names=c("SL","DSSL","BA","BAL","BE","BEL"),main="(A)")



RBAL<-matrix(0,2,2)


for(P in 1:1){
  
  sumbal<-list()
  
  for(x in 1:X){
    
    sumbal[[x]]<-matrix(0,p,p)
    
    
  }
  
  
  
  for(x in 1:X){
    
    for(k in 13001:15000){
      
      sumbal[[x]]<-sumbal[[x]]+L.testXBAL[[P]][[k]][[x]]
      
      
      
    }
    
  }
  
  
  for(x in 1:X){
    
    
    sumbal[[x]]<-sumbal[[x]]/2000
    
    
    
  }
  
  
  
  
  mod<-list()
  
  for(x in 1:X){
    
    mod[[x]]<-rep(0,45)
    
  }
  
  for(x in 1:X){
    
    mod[[x]]<-(as.vector(t(sumbal[[x]][upper.tri(sumbal[[x]])]))+as.vector(t(sumbal[[x]])[upper.tri(sumbal[[x]])]))/2
    
    pos<-which(mod[[x]]>0.5)
    
    mod[[x]]<-rep(0,45)
    
    mod[[x]][pos]<-1
    
    for(x in 1:X){
      
      
      sssum<-apply(PREDBA[[x]][[P]][20000:25000,],2,sum)/5001
      
      predd<-rep(0,45)
      
      predd[which(sssum>0.5)]<-1
      
      FP[x]<-length(which(predd-MX.test[[x]]==1))
      FN[x]<-length(which(predd-MX.test[[x]]==-1))
      
      
      rr<-roc(predd,MX.test[[x]])
      
      aucBA[x]<-rr$auc
      
    }
    
    AUCBA[P]<-mean(aucBA)
    
  }
  
  FP<-c()
  FN<-c()
  
  for(x in 1:X){
    
    
    FP[x]<-length(which(mod[[x]]-MX.test[[x]]==1))
    FN[x]<-length(which(mod[[x]]-MX.test[[x]]==-1))
    
  }
  
  
  RBAL[P,]<-c(mean(FN),mean(FP))
  
}


RBA<-matrix(0,2,2)


for(P in 1:2){
  
  sumba<-list()
  
  for(x in 1:X){
    
    sumba[[x]]<-matrix(0,p,p)
    
    
  }
  
  
  
  for(x in 1:X){
    
    for(k in 20001:25000){
      
      sumba[[x]]<-sumba[[x]]+L.testXBA[[P]][[k]][[x]]
      
      
      
    }
    
  }
  
  
  for(x in 1:X){
    
    
    sumba[[x]]<-sumba[[x]]/5000
    
    
    
  }
  
  
  
  
  mod<-list()
  
  for(x in 1:X){
    
    mod[[x]]<-rep(0,45)
    
  }
  
  for(x in 1:X){
    
    mod[[x]]<-(as.vector(t(sumba[[x]][upper.tri(sumba[[x]])]))+as.vector(t(sumba[[x]])[upper.tri(sumba[[x]])]))/2
    
    pos<-which(mod[[x]]>0.5)
    
    mod[[x]]<-rep(0,45)
    
    mod[[x]][pos]<-1
    
    rr<-roc(mod[[x]],MX.test[[x]])
    
    aucBA[x]<-rr$auc
    
  }
  
  AUCBA[P]<-mean(aucBA)
  
  FP<-c()
  FN<-c()
  
  for(x in 1:X){
    
    
    FP[x]<-length(which(mod[[x]]-MX.test[[x]]==1))
    FN[x]<-length(which(mod[[x]]-MX.test[[x]]==-1))
    
  }
  
  
  RBA[P,]<-c(mean(FN),mean(FP))
  
}


















ssssum<-matrix(0,4,4)

for (k in 40001:50000){
  
  ssssum1<-thetaK[[k]]
  
  #ssssum1[which(thetaK[[k]]!=0)]<-1
  
  ssssum<-ssssum+ssssum1
  
}

ssssum<-ssssum/10000

round(ssssum)






ssssum<-matrix(0,4,4)

for (k in 5001:6000){
  
  ssssum1<-matrix(0,4,4)
  
  ssssum1[which(thetaK[[k]]!=0)]<-1
  
  ssssum<-ssssum+ssssum1
  
}

ssssum<-ssssum/1000

round(ssssum)




sumba<-list()

for(x in 1:X){
  
  sumba[[x]]<-matrix(0,p,p)
  
  
}



for(x in 1:X){
  
  for(k in 20001:25000){
    
    sumba[[x]]<-sumba[[x]]+L.testXBA[[P]][[k]][[x]]
    
    
    
  }
  
}


for(x in 1:X){
  
  
  sumba[[x]]<-sumba[[x]]/5000
  
  
  
}

}


mod<-list()

for(x in 1:X){
  
  mod[[x]]<-rep(0,45)
  
}

for(x in 1:X){
  
  mod[[x]]<-(as.vector(t(sumba[[x]][upper.tri(sumba[[x]])]))+as.vector(t(sumba[[x]])[upper.tri(sumba[[x]])]))/2
  
  pos<-which(mod[[x]]>0.5)
  
  mod[[x]]<-rep(0,45)
  
  mod[[x]][pos]<-1
  
}

FP<-c()
FN<-c()

for(x in 1:X){
  
  
  FP[x]<-length(which(mod[[x]]-MX.test[[x]]==1))
  FN[x]<-length(which(mod[[x]]-MX.test[[x]]==-1))
  
}




boxplot(0.79,0.98,0.82,0.87,0.88,0.91,ylim=c(0.5,1),ylab="MCC",las=2,names=c("SL","DSSL","BA","BAL","BE","BEL"),main="(A)")





ssssum<-matrix(0,4,4)

for (k in 40001:50000){
  
  ssssum1<-matrix(0,4,4)
  
  ssssum1[which(thetaK[[k]]!=0)]<-1
  
  ssssum<-ssssum+ssssum1
  
}

ssssum<-ssssum/10000

round(ssssum)




ssssum<-matrix(0,4,4)

for (k in 13001:15000){
  
  ssssum1<-matrix(0,4,4)
  
  ssssum1[which(thetaK[[k]]!=0)]<-1
  
  ssssum<-ssssum+ssssum1
  
}

ssssum<-ssssum/2000

round(ssssum)









################### OK


MCCSL<-c()


for (P in 1:6){
  
  MCCSL[P]<- ((9-RSL[P,1])*(36-RSL[P,2])-RSL[P,2]*RSL[P,1])/sqrt( ((9-RSL[P,1])+RSL[P,2]) * ((9-RSL[P,1])+RSL[P,1]) * ((36-RSL[P,2])+RSL[P,2]) * ((36-RSL[P,2])+RSL[P,1]) )
  
}

MCCDSSL<-c()


for (P in 1:6){
  
  MCCDSSL[P]<- ((9-RDSSL[P,1])*(36-RDSSL[P,2])-RDSSL[P,2]*RDSSL[P,1])/sqrt( ((9-RDSSL[P,1])+RDSSL[P,2]) * ((9-RDSSL[P,1])+RDSSL[P,1]) * ((36-RDSSL[P,2])+RDSSL[P,2]) * ((36-RDSSL[P,2])+RDSSL[P,1]) )
  
}


RBAL<-matrix(0,10,2)


for(P in 1:6){
  
  sumbal<-list()
  
  for(x in 1:X){
    
    sumbal[[x]]<-matrix(0,p,p)
    
    
  }
  
  
  
  for(x in 1:X){
    
    for(k in 40001:50000){
      
      sumbal[[x]]<-sumbal[[x]]+L.testXBAL[[P]][[k]][[x]]
      
      
      
    }
    
  }
  
  
  for(x in 1:X){
    
    
    sumbal[[x]]<-sumbal[[x]]/10000
    
    
    
  }
  
  
  
  
  mod<-list()
  
  for(x in 1:X){
    
    mod[[x]]<-rep(0,45)
    
  }
  
  for(x in 1:X){
    
    mod[[x]]<-(as.vector(t(sumbal[[x]][upper.tri(sumbal[[x]])]))+as.vector(t(sumbal[[x]])[upper.tri(sumbal[[x]])]))/2
    
    pos<-which(mod[[x]]>0.5)
    
    mod[[x]]<-rep(0,45)
    
    mod[[x]][pos]<-1
    
    rr<-roc(mod[[x]],MX.test[[x]])
    
    aucBAL[x]<-rr$auc
    
  }
  
  AUCBAL[P]<-mean(aucBAL)
  
  
  FP<-c()
  FN<-c()
  
  for(x in 1:X){
    
    
    FP[x]<-length(which(mod[[x]]-MX.test[[x]]==1))
    FN[x]<-length(which(mod[[x]]-MX.test[[x]]==-1))
    
  }
  
  
  RBAL[P,]<-c(mean(FN),mean(FP))
  
}






RBA<-matrix(0,10,2)


for(P in 1:6){
  
  sumba<-list()
  
  for(x in 1:X){
    
    sumba[[x]]<-matrix(0,p,p)
    
    
  }
  
  
  
  for(x in 1:X){
    
    for(k in 40001:50000){
      
      sumba[[x]]<-sumba[[x]]+L.testXBA[[P]][[k]][[x]]
      
      
      
    }
    
  }
  
  
  for(x in 1:X){
    
    
    sumba[[x]]<-sumba[[x]]/10000
    
    
    
  }
  
  
  
  
  mod<-list()
  
  for(x in 1:X){
    
    mod[[x]]<-rep(0,45)
    
  }
  
  for(x in 1:X){
    
    mod[[x]]<-(as.vector(t(sumba[[x]][upper.tri(sumba[[x]])]))+as.vector(t(sumba[[x]])[upper.tri(sumba[[x]])]))/2
    
    pos<-which(mod[[x]]>0.5)
    
    mod[[x]]<-rep(0,45)
    
    mod[[x]][pos]<-1
    
    rr<-roc(mod[[x]],MX.test[[x]])
    
    aucBA[x]<-rr$auc
    
  }
  
  AUCBA[P]<-mean(aucBA)
  
  FP<-c()
  FN<-c()
  
  for(x in 1:X){
    
    
    FP[x]<-length(which(mod[[x]]-MX.test[[x]]==1))
    FN[x]<-length(which(mod[[x]]-MX.test[[x]]==-1))
    
  }
  
  
  RBA[P,]<-c(mean(FN),mean(FP))
  
}



MCCBAL<-c()


for (P in 1:5){
  
  MCCBAL[P]<- ((9-RBAL[P,1])*(36-RBAL[P,2])-RBAL[P,2]*RBAL[P,1])/sqrt( ((9-RBAL[P,1])+RBAL[P,2]) * ((9-RBAL[P,1])+RBAL[P,1]) * ((36-RBAL[P,2])+RBAL[P,2]) * ((36-RBAL[P,2])+RBAL[P,1]) )
  
}


MCCBA<-c()


for (P in 1:5){
  
  MCCBA[P]<- ((9-RBA[P,1])*(36-RBA[P,2])-RBA[P,2]*RBA[P,1])/sqrt( ((9-RBA[P,1])+RBA[P,2]) * ((9-RBA[P,1])+RBA[P,1]) * ((36-RBA[P,2])+RBA[P,2]) * ((36-RBA[P,2])+RBA[P,1]) )
  
}



boxplot(MCCSL,MCCDSSL,MCCBA,MCCBAL,ylim=c(0.5,1),ylab="MCC",las=2,names=c("SL","DSSL","MCCBA","MCCBAL"),main="(A)")

boxplot(AUCSL,AUCDSSL,AUCBA,AUCBAL,ylim=c(0.5,1),ylab="MCC",las=2,names=c("SL","DSSL","MCCBA","MCCBAL"),main="(A)")


mean(MCCSL)
mean(MCCDSSL)
mean(MCCBA)
mean(MCCBAL)

apply(RBA[1:6,],2,mean)
apply(RBAL[1:6,],2,mean)


mean(AUCSL[1:6])
mean(AUCDSSL[1:6])
mean(AUCBA[1:6])
mean(AUCBAL[1:6])