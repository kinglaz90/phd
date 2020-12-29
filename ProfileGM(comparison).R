
library(readxl)

data<-read_excel("C:\\Users\\Andrea\\Desktop\\aml-rppa.xls")

data<-as.data.frame(data)

data<-cbind(data[,-1],data[,1])

data$`data[, 1]`<-as.factor(data$`data[, 1]`)

summary(data)


# PRECISION MATRICES LEUKEMIA

TEST<-list(c())

TEST[[1]]<-rep(0,18)


TEST[[2]]<-diag(18)

TEST[[2]][1,c(8,11,12,15)]<-1
TEST[[2]][2,c(3,9,13)]<-1
TEST[[2]][4,10]<-1
TEST[[2]][5,c(13,18)]<-1
TEST[[2]][6,c(10,15,16)]<-1
TEST[[2]][7,15]<-1
TEST[[2]][10,c(12,15)]<-1
TEST[[2]][15,16]<-1


TEST[[3]]<-diag(18)

TEST[[3]][1,c(8,11,12)]<-1
TEST[[3]][2,c(3,9,11)]<-1
TEST[[3]][4,c(9,12)]<-1
TEST[[3]][5,13]<-1
TEST[[3]][6,c(15,16)]<-1
TEST[[3]][7,c(11,14,15)]<-1
TEST[[3]][9,17]<-1
TEST[[3]][10,c(11,12,18)]<-1
TEST[[3]][11,14]<-1
TEST[[3]][12,14]<-1
TEST[[3]][15,16]<-1



TEST[[4]]<-diag(18)

TEST[[4]][1,c(3,8,11,12,15)]<-1
TEST[[4]][2,c(3,9)]<-1
TEST[[4]][4,12]<-1
TEST[[4]][5,c(7,13,18)]<-1
TEST[[4]][6,c(9,15,16)]<-1
TEST[[4]][7,c(9,11,15,18)]<-1
TEST[[4]][10,c(12,15,18)]<-1
TEST[[4]][11,c(17,18)]<-1
TEST[[4]][12,16]<-1
TEST[[4]][14,18]<-1
TEST[[4]][15,16]<-1



TEST[[5]]<-diag(18)

TEST[[5]][1,c(8,10,11,14,15)]<-1
TEST[[5]][2,c(9,14)]<-1
TEST[[5]][3,13]<-1
TEST[[5]][4,c(6,10)]<-1
TEST[[5]][5,13]<-1
TEST[[5]][6,9]<-1
TEST[[5]][7,c(14,15)]<-1
TEST[[5]][10,c(12,15)]<-1
TEST[[5]][11,c(14,16,17)]<-1
TEST[[5]][15,16]<-1






# COMPARISON OF THE SELECTED MODELS



ACC.e<-c()
TPR.e<-c()
TNR.e<-c()
sparss<-c()
adjjj<-matrix(0,100,18)


MODEL<-list(c())

MODEL[[1]]<-rep(0,18)
MODEL[[2]]<-diag(18)
MODEL[[3]]<-diag(18)
MODEL[[4]]<-diag(18)
MODEL[[5]]<-diag(18)


k=4 

p=18

for (i in 1:100){

PROVA<-ProfileGM(data=data,n=c(17,34,68,59),type="bidirected",strategy = "AND")

adjjj[i,]<-as.vector(PROVA$adj[[1]])

for (b in 1:(k+1)){
  
MODEL[[b]]<-MODEL[[b]]+PROVA$adj[[b]]

}

spars<-0

print(i)

for(t in 2:k){
  
  for(l in 1:p){
    
    for(j in 1:p){
      
      if (j>l){
        
        if (PROVA$adj[[t]][l,j]!=0){
          
          spars<-spars+1  

        } 
      }
    }
  }
}

}

for (i in 1:(k+1)){
  
  MODEL[[i]]<-MODEL[[i]]/100
  
  MODEL[[i]]<-round(MODEL[[i]])
  
}

MODEL.U2<-MODEL

MODEL.U2

spars<-1-(spars/(k*p*(p-1)/2))

sparss<-c(sparss,spars)


f<-check(TEST,PROVA$adj)

ACC.e<-c(ACC.e,f$ACC.e)
TPR.e<-c(TPR.e,f$TPR.e)
TNR.e<-c(TNR.e,f$TNR.e)




RES2<-c(round(mean(ACC.e),2),round(mean(TPR.e),2),round(mean(TNR.e),2))

RES2



# TRY TO COMPARE "TOTAL", "PROFILE", "NO" ASSOCIATIONS

  TEST.It<-diag(18)
  TEST.Ip<-diag(18)
  TEST.D<-diag(18)
  
  TEST.It2<-diag(18)
  TEST.Ip2<-diag(18)
  TEST.D2<-diag(18)
  
  

for(l in 1:p){
  
  for(j in 1:p){
    
    c<-0
    
    if(j>l){
    
    for(i in 2:(k+1)){
        
        c<-c+MODEL.U[[i]][l,j]
      
    }
        
        if(c==0){
          
             TEST.It2[l,j]<-1
          
        } else if (c==4){
             
          
             TEST.D2[l,j]<-1
            
          
        } else if (c>0 & c<4) {
          
             TEST.Ip2[l,j]<-1
          
          
        }
      }
    }
}
  
  
  
  
  
  
  
  for(l in 1:p){
    
    for(j in 1:p){
      
      c<-0
      
      if(j>l){
        
        for(i in 2:(k+1)){
          
          c<-c+TEST[[i]][l,j]
          
        }
        
        if(c==0){
          
          TEST.It[l,j]<-1
          
        } else if (c==4){
          
          
          TEST.D[l,j]<-1
          
          
        } else if (c>0 & c<4) {
          
          TEST.Ip[l,j]<-1
          
          
        }
      }
    }
  }
  
  
  
  
  
  
  
  


  TEST.It
  TEST.Ip
  TEST.D

TEST.It+ TEST.Ip+TEST.D

TEST.It2
TEST.Ip2
TEST.D2

TEST.It2+ TEST.Ip2+TEST.D2


ItC<-0
It<-0
IpC<-0
Ip<-0
DC<-0
D<-0


for(l in 1:p){
  
  for(j in 1:p){
    
    if (j>l){
      
      
      if (TEST.It[l,j]==1 & TEST.It2[l,j]==1){
        
        ItC<-ItC+1
        It<-It+1
        
      } else if (TEST.It[l,j]==1 & TEST.It2[l,j]==0){
        
        It<-It+1
        
      }
        
      
      
        
      if (TEST.Ip[l,j]==1 & TEST.Ip2[l,j]==1){
        
        IpC<-IpC+1
        Ip<-Ip+1
        
      } else if (TEST.Ip[l,j]==1 & TEST.Ip2[l,j]==0){
        
        Ip<-Ip+1
        
      }
      
      
      
      
      if (TEST.D[l,j]==1 & TEST.D2[l,j]==1){
        
        DC<-DC+1
        D<-D+1
        
      } else if (TEST.D[l,j]==1 & TEST.D2[l,j]==0){
        
        D<-D+1
        
      }
      
      
      
        
      }
      
  }
  
  P.ItC<- ItC/It
  P.IpC<- IpC/Ip
  P.DC<- DC/D
  }
  
c(P.ItC,P.IpC,P.DC)


library(igraph)

graph_from_adjacency_matrix(adjmatrix=MODEL.U[[2]], mode = "undirected",diag = TRUE, add.colnames = NULL, add.rownames = NA)

plot(graph_from_adjacency_matrix(adjmatrix=MODEL.U[[2]], mode = "undirected",diag = FALSE, add.colnames = NULL, add.rownames = NA)
)




