library(tidyverse)
library(knitr)
setwd("D:\\R resource")
Usertable <- read.csv("User_Table.csv")
Gamelog <- read.csv("Game_Log.csv")
str(Gamelog)
kable(Gamelog[1:10,])
kable(Usertable[1:10,])

Gametable <- Gamelog%>%
  group_by(User_Id)%>%
  summarise( 
    Min_Aft=mean(Min_Aft),
    Min_Eve=mean(Min_Eve),
    Min_Mid=mean(Min_Mid),
    Buy_Coin=mean(Buy_Coin),
    Buy_Dia=mean(Buy_Dia),
    Buy_Car=mean(Buy_Car)
    )%>%
  inner_join(Usertable,by='User_Id')
Gametable%>%summary()
#�ث�
Gametable <- Gametable[,2:7]%>%
  mutate(
    Aft = (Min_Aft - min(Min_Aft)) / (max(Min_Aft)-min(Min_Aft)),
    Eve = (Min_Eve - min(Min_Eve)) / (max(Min_Eve)-min(Min_Eve)),
    Mid = (Min_Mid - min(Min_Mid)) / (max(Min_Mid)-min(Min_Mid)),
    Coin = (Buy_Coin - min(Buy_Coin)) / (max(Buy_Coin)-min(Buy_Coin)),
    Dia = (Buy_Dia - min(Buy_Dia)) / (max(Buy_Dia)-min(Buy_Dia)),
    Car = (Buy_Car - min(Buy_Car)) / (max(Buy_Car)-min(Buy_Car))
  ) %>% cbind(
    Gametable[,c(8,9)]
  )
kable(Gametable[1:5,])    
#���O�ܼ�
Dummytable <- model.matrix(~Identity+ Telecom,Gametable)
kable(Dummytable[1:10,])
Dummytable[,-1]
#���X
Gametable <- cbind(
  Gametable[,-c(1:6,13,14)],
  Dummytable[,-1]
)
kable(Gametable[1:5,])
#��ı��
library(reshape2)
Cormatrix <- Gametable %>% cor() %>% melt()
kable(Cormatrix[1:5,])

ggplot(Cormatrix)+
  
  geom_tile(aes(Var1,Var2,fill=value),colour="white")+
  
  scale_fill_gradient2(low="firebrick4",high = "steelblue")+
  
  guides(fill=guide_legend(title="Correlation"))+
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust=1),
        axis.title = element_blank())
#���h�����s���R 
set.seed(500)
Distance <- dist(Gametable,method = 'euclidean')
hclust(Distance,method="complete")%>% plot()
#��ƫؼҤ��R

set.seed(500)#kmeans�t��k�O�o�]�H���ؤl

K <- kmeans(Gametable,3)

Clusterresult <- cbind(
  Gametable,
  K$cluster
)

colnames(Clusterresult)[ncol(Clusterresult)] <- 'Cluster'
table(Clusterresult$Cluster)  
  
ClusterResultForPlot <- Clusterresult %>%
  gather( key = Continuous_Variable,
          value = Normalized_Value,
          - c(IdentityNovice, IdentityVeteran, Telecomother, Cluster))

ClusterResultForPlot$Continuous_Variable <- ClusterResultForPlot$Continuous_Variable %>% factor( levels = c('Mid','Aft','Eve','Coin','Dia','Car'))

ggplot( data = ClusterResultForPlot) + 
  geom_boxplot( aes( x = Continuous_Variable,
                     y = Normalized_Value),
                size = 0.7) +
  facet_wrap( ~ Cluster)
#�אּ4�s

set.seed(500)
k <- kmeans(Gametable,4)

ClusterResult <- cbind(
  Gametable,
  k$cluster)  
colnames(ClusterResult)[ncol(ClusterResult)] <-"cluster"  
table(ClusterResult$cluster)


ClusterResultForPlot <- ClusterResult %>% 
  gather(key = Continuous_Variable,
         value = Normalized_Value,
         -c(IdentityNovice, IdentityVeteran, Telecomother, cluster))

ClusterResultForPlot$Continuous_Variable <- ClusterResultForPlot$Continuous_Variable %>% factor(levels = c('Aft','Eve','Mid','Coin','Dia','Car'))

ggplot( data = ClusterResultForPlot) + 
  geom_boxplot( aes( x = Continuous_Variable,
                     y = Normalized_Value),
                size = 0.7) +
  facet_wrap( ~ cluster)







