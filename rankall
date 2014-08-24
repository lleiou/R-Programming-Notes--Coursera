rankall<-function(outcome,num="best"){
  
  letter<-read.csv("hospital-data.csv",stringsAsFactors=FALSE)
  letter<-letter[,"State"]
  letter<-unique(letter)
  letter<-letter[order(letter)]#对州的名称按照字母排序
  length<-length(letter)
  
  #创建最终输出的数据框
  data_frame<-data.frame(hospital=rep(0,length),state=rep(0,length))
  
  data<-read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE)
                     
                         if(outcome=="heart attack"){
                           data<-data[,c(2,7,11)]
                           data<-subset(data,data[,3]!="Not Available")
                           
                           data[, 3] <- as.numeric(as.character(data[, 3]))
                         }
                         
                          else if(outcome=="heart failure"){
                           data<-data[,c(2,7,17)]
                           data<-subset(data,data[,3]!="Not Available")
                           
                           data[, 3] <- as.numeric(as.character(data[, 3]))
                          }
                         
                         else if(outcome=="pneumonia"){
                           data<-data[,c(2,7,23)]
                           
                           
                           data<-subset(data,data[,3]!="Not Available")
                           
                           data[, 3] <- as.numeric(as.character(data[, 3]))
                         }
                         else  {
                           stop("invalid outcome")
                         }
                         
                          
  for(i in 1:length){
    state_num<-letter[i]#临时存储第i个州的名称
   data_s<-subset(data,data[,2]==state_num) #选取split之后, 第i个州对应的data.frame
      if(num=="best"){
     data_new<-data_s[order(data_s[,3],data_s[,1],decreasing=F),]
     data_frame[i,1]<-data_new[1,1]
     data_frame[i,2]<-data_new[1,2]
     }
   
   else if(num=="worst"){
     data_new<-data_s[order(data_s[,3],data_s[,1],decreasing=T),]
     data_frame[i,1]<-data_new[1,1]
     data_frame[i,2]<-data_new[1,2]
     
   }
  else {
    data_new<-data_s[order(data_s[,3],data_s[,1],decreasing=F),]
    data_frame[i,1]<-data_new[num,1]
    data_frame[i,2]<-data_s[1,2]
  }
  }
                         data_frame
    
  
}
