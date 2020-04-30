### 2020/04/30 Keonwoo Park

### 데이터 구조론
## Insertion Sort

Insertion_Sort <- function(d, decreasing=FALSE){
  size_d <- length(d)
  for(i1 in 2:size_d){
    temp_value = d[i1]
    j = i1-1
    if(decreasing==FALSE){
      while(j>=1 && d[j] > temp_value){
        d[j+1] = d[j]
        j = j-1
      }
    }else{
      while(j>=1 && d[j] < temp_value){
        d[j+1] = d[j]
        j= j-1
      }
    }
    d[j+1] = temp_value
  }
  return(d)
}

a = sample(1:10)
a
Insertion_Sort(a)

# 알고리즘 속도 측정
set.seed(1234)
test_data<-data.frame(d1=sample(1:1000),
                      d2=1:1000,
                      d3=1000:1,
                      d4=c(1:500,sample(501:1000)),
                      d5=c(sample(1:500),c(501:1000))
)
head(test_data)



n=5
Simulation_Results <- data.frame(d1=rep(0,n),
                                 d2=rep(0,n),
                                 d3=rep(0,n),
                                 d4=rep(0,n),
                                 d5=rep(0,n))
for (i1 in 1:n){
  for (i2 in 1:5){
    T1<-Sys.time()
    Insertion_Sort(test_data[,i2])
    T2<-Sys.time()
    T3=T2-T1
    Simulation_Results[i1,i2]=as.numeric(difftime(T2,T1,units="secs"))
    
  }
}
Simulation_Results
