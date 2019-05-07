subjectt<-function(sub){
  require(readxl)
  att=0
  data=read_excel(file="H:/III Yr II Sem Y15 BATCH.xlsx",1)
  newdata=data.frame(data)
  c=nrow(newdata)
  total=na.omit(newdata[sub])
  data=na.omit(newdata[sub])
  stud=c("att","total")
  stud=as.data.frame(stud)
  total=nrow(total)-1
  yy=data.frame(c())
  for(z in 1:42){
    att=0
    for(j in 1:126) 
    {
      ida=na.omit(newdata[j,sub])
      ida=strsplit(as.character(newdata[j,sub]),split=",")
      x=as.numeric(unlist(ida))
      x=x[-is.na(x)]
      for(i in x) {
        ss=gsub("\\s","",i)
        if(length(na.omit(ss))==0) {
          next
        }
        else if(i==z) {
          att=att+1
        }
      } 
    }
    att=total-att
    yy=rbind.data.frame(yy,c(z,att,total))
    total=nrow(na.omit(newdata[sub]))-1
  }
  colnames(yy)=c("ROLLNO","ATT","TOTAL")
  return(yy)
}
IDA=subjectt("IDA")
ICS=subjectt("ICS")
EP1=subjectt("EP1")
WN=subjectt("WN")
BI=subjectt("BI")
SE=subjectt("SE")
studentatt=data.frame(IDA,ICS,EP1,WN,BI,SE)
conducted=data.frame(c())
for(i in 1:42)
{
  conducted=rbind.data.frame(conducted,c(sum(studentatt[i,2],studentatt[i,5],studentatt[i,8],studentatt[i,11],studentatt[i,14],studentatt[i,17])))
}
colnames(conducted)=c("ATTENDED")
tt=data.frame(c())
for(i in 1:42)
{
  tt=rbind.data.frame(tt,c(sum(studentatt[i,3],studentatt[i,6],studentatt[i,9],studentatt[i,12],studentatt[i,15],studentatt[i,18])))
}
colnames(tt)=c("total")
per=data.frame(c())
for(i in 1:42)
{
  per=rbind.data.frame(per,((conducted[i,]/tt[i,])*100))
}
colnames(per)=c("PERCENTAGE")
studentatt=data.frame(IDA,ICS,EP1,WN,BI,SE,per)
colnames(studentatt)=c("ROLLNO","IDA_ATT","IDA_CON","ROLLNO","ICS_ATT","ICS_CON","ROLLNO","EP1_ATT","EP1_CON","ROLLNO","WN_ATT","WN_CON","ROLLNO","BI_ATT","BI_CON","ROLLNO","SE_ATT","SE_CON","PERCENTAGE")
cbind.data.frame(marks,per)
