complete <- function(directory, id = 1:332) {
  
  id_length<-length(id)
  num<-id
  id_new<-1:332
  id_new<-id_new-id_new
  k=1
  
  for(i in id){
    
    if (i<10){
      id_new[i]<-paste(directory,"/00",i,".csv",sep="")
    }
    
    if (9<i&i<100){
      id_new[i]<-paste(directory,"/0",i,".csv",sep="")
    }
    
    if (i>99){
      id_new[i]<-paste(directory,"/",i,".csv",sep="")
    }
    
    
    read.csv(id_new[i])[complete.cases(read.csv(id_new[i])),]
        num[k]=nrow(read.csv(id_new[i])[complete.cases(read.csv(id_new[i])),])
      k=k+1
  #sulfate要加上引号！！
  
  } 
 
  
  data.frame(id=id,nobs=num)
  

  
}

#问题一览

#1.每使用一个新的向量, 一定要先在前面定义好

#2.读取文件时，要加上.csv后缀

#3.路径要加上引号

#4.错误: 意外的'{' in: 有时候, 出现这种错误不一定是{的问题, 也可能是[或者(的问题, 因为在R中, 有时如果在一个)
旁边再打一个), 可能会打不出来...所以写程序的时候要看着屏幕

#5.if语句的判断条件里面不能出现NA, 只能是T或者F

#6.由于for循环里面的i不一定是从1开始的, 而是从id开始的, 所以num的指标不要使用i了, 再定义一个从i开始的指标变量.

#7.complete.cases(data.frame)返回的是所有不含NA指标的列指标组成的向量.


