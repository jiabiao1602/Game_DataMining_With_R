# 自定义score_function( )实现指标打分模型
# 渠道得分函数
channel_score <- function(data,amount=T){
  
  # 进行指标的波动性打分  
  library(reshape)
  data <- cast(data,渠道名称~自然周)
  # 利用apply函数分渠道求出当前周与上周的差值
  x <- t(apply(data[,-1],1,diff))
  # 利用as.data.frame函将x转换成数据框形式
  x <- as.data.frame(x,row.names = as.character(data[,1]))
  # 利用colnames函数对x列名重新赋值
  colnames(x) <-  colnames(data[3:ncol(data)])
  # 找出最近四周的最大值
  # 自定义函数mystat求最近四周的最大值
  mystat <- function(x){
    m <- rep(0,(ncol(data)-1))
    for(i in 1:(ncol(data)-1)){
      if(i <=3){
        m[i] <- max(x[1:i])
      } else {
        m[i] <- max(x[(i-4):i])
      }
    }
    return(m)
  }
  # 利用apply函按分渠道求最近四周最大值
  y <- t(apply(data[,-1],1,mystat))
  # 利用as.data.frame函数将y转换成数据框形式
  y <- as.data.frame(y,row.names = as.character(data[,1]))
  # 利用colnames函数对y列名重新赋值
  colnames(y) <-  colnames(data[2:ncol(data)])
  # 计算波动变化得分
  reliability_score <- 5*round(x/y[,-1],3)
  reliability_score
  if(amount) {
    # 进行指标的量级打分
    # 利用colSums函数进行按列求和
    x <- colSums(data[,-1])
    x <- as.data.frame(matrix(rep(x,nrow(data)),nrow = nrow(data),byrow = T))
    amount_score <- 5*round(data[,-1]/x,3)
    rownames(amount_score) <- rownames(reliability_score)
    amount_score
    
    # 计算指标得分
    # 利用cumsum函数进行对波动变化值进行累积求和
    rs_cumsum <- apply(reliability_score,1,cumsum)
    rs_cumsum <- as.data.frame(t(rs_cumsum))
    score <- 10+rs_cumsum+amount_score[,-1] #起始分+波动变化得分+量级得分
    score[,colnames(data)[2]] <- 10
    score <- score[,c(ncol(score),1:ncol(score)-1)] # 改变列的顺序
  } else {
    # 计算指标的指标得分
    # 利用cumsum函数进行对波动变化值进行累积求和
    rs_cumsum <- apply(reliability_score,1,cumsum)
    rs_cumsum <- as.data.frame(t(rs_cumsum))
    score <- 10+rs_cumsum #起始分+波动变化得分
    score[,colnames(data)[2]] <- 10
    score <- score[,c(ncol(score),1:ncol(score)-1)] # 改变列的顺序
  }
  return(round(score,2))
}
