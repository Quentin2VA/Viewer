#RDA function proto N2 - ENVISTRESS
#QD20200812
------------------------------------------------------------------------------------------------------
  
  library(ggplot2)

Rda.ggplot<- function(X,PC1=1,PC2=2,site_lab=TRUE, point_shape=16, col="black"){
  attach(X)
  smry <- summary(X)
  df1  <- data.frame(smry$sites[,PC1:PC2])       # PC1 and PC2
  df2  <- data.frame(smry$species[,PC1:PC2])     # loadings for PC1 and PC2
  df3  <- data.frame(X$CCA$biplot[,PC1:PC2])
  header <- colnames(df1)
  
  if(site_lab==TRUE){
    rda.plot <<- ggplot(df1, aes(x=df1[,1], y=df1[,2])) + 
      theme_classic()+
      geom_point(aes(label=row.names(df1)),color=col,fill=col,alpha=0.7,size=0.7,shape=point_shape)+
      geom_text(aes(label=rownames(df1)),size=0.1) +
      geom_hline(yintercept=0, linetype="dotted") +
      geom_vline(xintercept=0, linetype="dotted") +
      scale_color_brewer(palette = 2)+
      coord_fixed()+
      ylab(label = header[2])+xlab(label = header[1])+labs(caption = NULL)
  }else{
    rda.plot <<- ggplot(df1, aes(x=df1[,1], y=df1[,2])) + 
      theme_classic()+
      geom_point(aes(label=row.names(df1)),color=col,fill=col,alpha=0.7,size=0.7,shape=point_shape)+
      geom_hline(yintercept=0, linetype="dotted") +
      geom_vline(xintercept=0, linetype="dotted") +
      scale_color_brewer(palette = 2)+
      coord_fixed()+
      ylab(label = header[2])+xlab(label = header[1])+labs(caption = NULL)
  }
  
  rda.biplot <- rda.plot +
    geom_segment(data=df2, aes(x=0, xend=df2[,1], y=0, yend=df2[,2]), 
                 color="red", arrow=arrow(length=unit(0.01,"npc"))) +
    geom_segment(data=df3, aes(x=0, xend=df3[,1], y=0, yend=df3[,2]), 
                 color="blue", arrow=arrow(length=unit(0.01,"npc"))) +
    geom_text(data=df2, 
              aes(x=df2[,1],y=df2[,2],label=rownames(df2),
                  hjust=0.5*(1-sign(df2[,1])),vjust=0.5*(1-sign(df2[,2]))), 
              color="red", size=4)+
    geom_text(data=df3, 
              aes(x=df3[,1],y=df3[,2],label=rownames(df3),
                  hjust=0.5*(1-sign(df3[,1])),vjust=0.5*(1-sign(df3[,2]))), 
              color="blue", size=4)
  
  print(rda.biplot)
  detach(X)
}

# # # Summary # #----

#X = YOur RDA object
# PC 1 and 2 : Pincip Component selection 
# site_lab: Display site names 
# point_shape: try from 1 to 21 to change point shape 
# col: color attributeds to sites can be a factor or a Rcolor (e.g "red") or any type for ggplot2

# Example of uses :

#library(vegan)

#my.rda <- rda(sp~env)

#Rda.ggplot(my.rda,PC1 = 1,PC2 = 2,site_lab = FALSE,point_shape = 5, col="#C91ABA")

#Rda.ggplot(my.rda,PC1 = 1,PC2 = 2,site_lab = FALSE,point_shape = 3, col="#red")

### NOT RUN

 # ENJOY ;-)
#P@racels:Q2VA.git.hub
