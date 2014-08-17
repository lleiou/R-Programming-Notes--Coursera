corr <- function(directory, threshold=0) {
  
  k<-0
  x<-0
  cor<-1:332
  
  for(i in 1:332){
    
    if (i<10){
      id_new[i]<-paste(directory,"/00",i,".csv",sep="")
    }
    
   else if (9<i&i<100){
      id_new[i]<-paste(directory,"/0",i,".csv",sep="")
    }
    
    else if (i>99){
      id_new[i]<-paste(directory,"/",i,".csv",sep="")
    }
  }

  for(i in 1:332){    
    if(nrow(read.csv(id_new[i])[complete.cases(read.csv(id_new[i])),])>threshold)
    {
      k=k+1
    sulfate<-0
    nitrate<-0
    x<-read.csv(id_new[i])[complete.cases(read.csv(id_new[i])),]
    sulfate<-x[,"sulfate"]
    nitrate<-x[,"nitrate"]
    cor[k]<-cor(sulfate,nitrate)
    }
    #sulfate要加上引号！！
    
  } 

  if(k>0){
  new<-1:k
  for(i in 1:k){
    new[i]<-cor[i]
  }
  }
  else {new<-NULL}#根据测试用例, threshold=5000的时候, 我们summary得到一个Null...（当然, 我觉得, 如果程序编的
  #比较好的话,是不用单独考虑这个问题的...)
  
  options(digits = 6)
  new

}



#本程序存在的重大问题！

#执行效率非常低, 在等待结果期间, 建议做一点别的事情...

#感想:终于把这个assignment全都做完了, 很不容易, 看出来了自己的思维不严谨的缺点了...

#另,虽然是通过了十个测试用例，但我清楚, 这些程序易读性很差, 而且执行效率非常低,很多地方写的很不简洁, 值得提高！！
