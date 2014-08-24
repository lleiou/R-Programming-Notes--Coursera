rankhospital <- function (state,outcome,num="best") {
  
  #先判断state名字是否正确
  
  letter<-read.csv("hospital-data.csv")
  letter<-letter[,"State"]
  letter<-unique(letter)
  
  length<-length(letter)
  
  a<-0
  
  for(i in 1:length){
    if(letter[i]==state){
      a<-a+1
    }
  }
  
  
  if(a==0){
    stop("invalid state")
  }
  #名字正确之后, 先生成所查看州的数据集
  
  else {
    
 
    
    data<-read.csv("outcome-of-care-measures.csv")
    
    data<-subset(data,State==state) #仅保留所调查州的数据
    
    if(num =="best"){
    ##########################################
    ##下面根据死因来进行subset
    if(outcome=="heart attack"){
      data<-data[,c(2,7,11)]
      data<-subset(data,data[,3]!="Not Available")
      
      data[, 3] <- as.numeric(as.character(data[, 3]))
      
      
      data_new<-data[order(data[,3],data[,1],decreasing=F),]#按照字母顺序排序
      
      
      as.character(data_new[1,1])
    }
    
    else if(outcome=="heart failure"){
      data<-data[,c(2,7,17)]
      data<-subset(data,data[,3]!="Not Available")
      
      data[, 3] <- as.numeric(as.character(data[, 3]))
      
      
      data_new<-data[order(data[,3],data[,1],decreasing=F),]#按照字母顺序排序
      
      as.character(data_new[1,1])
    }
    
    else if(outcome=="pneumonia"){
      data<-data[,c(2,7,23)]
      
      
      data<-subset(data,data[,3]!="Not Available")
      
      data[, 3] <- as.numeric(as.character(data[, 3]))
      
      
      data_new<-data[order(data[,3],data[,1],decreasing=F),]#按照字母顺序排序
      
      
      as.character(data_new[1,1])
    }
    
    else  {
      stop("invalid outcome")
    }
    
    ##########################################
    #获得只有所需数据的data.frame
    }
    
    else if(num=="worst"){
      ##########################################
      ##下面根据死因来进行subset
      if(outcome=="heart attack"){
        data<-data[,c(2,7,11)]
        data<-subset(data,data[,3]!="Not Available")
        
        data[, 3] <- as.numeric(as.character(data[, 3]))
        
        
        data_new<-data[order(data[,3],data[,1],decreasing=T),]#按照字母顺序排序
        
        
        as.character(data_new[1,1])
      }
      
      else if(outcome=="heart failure"){
        data<-data[,c(2,7,17)]
        data<-subset(data,data[,3]!="Not Available")
        
        data[, 3] <- as.numeric(as.character(data[, 3]))
        
        
        data_new<-data[order(data[,3],data[,1],decreasing=T),]#按照字母顺序排序
        
        as.character(data_new[1,1])
      }
      
      else if(outcome=="pneumonia"){
        data<-data[,c(2,7,23)]
        
        
        data<-subset(data,data[,3]!="Not Available")
        
        data[, 3] <- as.numeric(as.character(data[, 3]))
        
        
        data_new<-data[order(data[,3],data[,1],decreasing=T),]#按照字母顺序排序
        
        
        as.character(data_new[1,1])
      }
      
      else  {
        stop("invalid outcome")
      }
      
      ##########################################
      #获得只有所需数据的data.frame
    }
    
    else {
      ##########################################
      ##下面根据死因来进行subset
      num<-as.numeric(num)
      
      if(outcome=="heart attack"){
        data<-data[,c(2,7,11)]
        data<-subset(data,data[,3]!="Not Available")
        
        data[, 3] <- as.numeric(as.character(data[, 3]))
        
        
        data_new<-data[order(data[,3],data[,1],decreasing=F),]#按照字母顺序排序
        
        
        as.character(data_new[num,1])
      }
      
      else if(outcome=="heart failure"){
        data<-data[,c(2,7,17)]
        data<-subset(data,data[,3]!="Not Available")
        
        data[, 3] <- as.numeric(as.character(data[, 3]))
        
        
        data_new<-data[order(data[,3],data[,1],decreasing=F),]#按照字母顺序排序
        
        as.character(data_new[num,1])
      }
      
      else if(outcome=="pneumonia"){
        data<-data[,c(2,7,23)]
        
        
        data<-subset(data,data[,3]!="Not Available")
        
        data[, 3] <- as.numeric(as.character(data[, 3]))
        
        
        data_new<-data[order(data[,3],data[,1],decreasing=F),]#按照字母顺序排序
        
        
        as.character(data_new[num,1])
      }
      
      else  {
        stop("invalid outcome")
      }
      
      ##########################################
    }
    
  }
  
  
}



