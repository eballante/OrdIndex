
new.index<-function(pred_prob,test,m) {
  n<-dim(pred_prob)[1]
  pred_prob<-unname(pred_prob) #per provare
  ID<-1:n
  pred_class <- apply(pred_prob, 1, which.max)
  pred_class_fac <-as.factor(pred_class)
 ind<-cbind(1:n,pred_class)
 prob_estclass<-pred_prob[ind]
  df<-data.frame(ID,prob_estclass,pred_class_fac,pred_class)
  ord_obs<- df %>% 
            arrange(pred_class_fac,desc(prob_estclass)) 

  y_real<-as.numeric(test$y[ord_obs$ID])
  x_graph<-seq(0,1,length.out=n)
  fun<-stepfun(x_graph,c(0,y_real))
  y=fun(x_graph)
  
  lenclass_ind<-c(1,1+which(diff(sort(pred_class))!=0))
  lenclass_ind[m+1]<-n
  lenclass<-lenclass_ind/n
  lenclass[1]=0
  lenclass[m+1]<-1
  f.goal<-stepfun(x_graph,c(0,sort(as.numeric(test$y)-1)))
  y.goal=f.goal(x_graph)
  #plot(x_graph,y.goal)
  fun_mod<-data.frame(x_graph,y)
  h<-c(1,1:m)
  class_point<-data.frame(lenclass,h)
  graph<-ggplot(fun_mod,aes(x=x_graph,y=y))+ geom_step()+geom_point(class_point,mapping=aes(x=lenclass,y=h))
  fun_int<-function (x) abs(fun(x)-f.goal(x))
  int<-rep(NA,m)
  w<-rep(NA,m)
  for (i in 1:m) {
    int[i]<-integral(fun_int,lenclass[i],lenclass[i+1])
    err<-min(which(y_real[lenclass_ind[i]:(lenclass_ind[i+1]-1)]!=ord_obs$pred_class_fac[lenclass_ind[i]:(lenclass_ind[i+1]-1)]))/n
    err<-ifelse(is.finite(err),lenclass[i]+err,lenclass[i+1])
    w[i]<-(lenclass[i+1]-err)/(lenclass[i+1]-lenclass[i])
  }
index<-sum(int*w)
obj<-list("index"=index, "graph"=graph)
return(obj)
}
