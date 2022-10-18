# 导入相关包  ，得先下载相关包，下载方式为 instllpackages('statnet')

library(statnet)
library(network)
library(ergm)
library(tergm)
library(xlsx)
library(btergm)
library(texreg)
Edge<-read.xlsx("C:/Users/Jack/Desktop/黑龙江省绿色技术产学研协同创新网络形成机理/整个模型/最终数据.xlsx",sheetName="edge",header=T,row.names=1)  #读取矩阵数据。文件导入地址可鼠标右键进属性复制
Node<-read.xlsx("C:/Users/Jack/Desktop/黑龙江省绿色技术产学研协同创新网络形成机理/整个模型/最终数据.xlsx",sheetName="node",header=T)  # 读取节点数据
 # Node[,c(2:6)] <- as.numeric(unlist(Node[,c(2:6)]))
Network <- as.matrix(Edge)#导入T网络3
Network <- as.network(Network,directed=F)  # 数据网络化
Network %v% "Type1" <- Node[,2]
Network %v% "Type2" <- Node[,3]
Network %v% "Type3" <- Node[,4]
Network %v% "Area" <- Node[,5]
Network %v% "PatentNum" <- Node[,6]   #给网络附上节点属性

plot(Network) # 绘图

#几个模型，cotrol是模型运行参数，gwdegree(0, fixed=TRUE)也是运行参数  具体可用help(gwdegree)查看详细用法

model1 <- ergm(Network ~ edges + gwdegree(0.5, fixed=TRUE) + gwesp(0.5, fixed=TRUE),
               control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=5000
                                    ,MCMC.interval=1000, seed = 567, parallel=14,parallel.type='PSOCK'), 
               eval.loglik = T, verbose = T) 
summary(model1)


model2 <- ergm(Network ~ edges  + gwdegree(0.5, fixed=TRUE) + gwesp(0.5, fixed=TRUE) + nodecov('PatentNum') ,
               control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=5000,MCMLE.density.guard = 10000
                                    ,MCMC.interval=10000, seed = 567, parallel=14,parallel.type='PSOCK'), 
               eval.loglik = T, verbose = T)
summary(model2)


model3 <- ergm(Network ~ edges + gwesp(0.5, fixed=TRUE) + gwdegree(0.5, fixed=TRUE)
               + nodematch('Type1',diff=T) + nodematch('Type2',diff=T) + nodematch('Type3',diff=T) + nodematch('Area') + nodecov('PatentNum'),
               control=control.ergm(MCMC.samplesize=10000, MCMC.burnin=5000
                                    ,MCMC.interval=10000, seed = 567, parallel=14,parallel.type='PSOCK'), 
               eval.loglik = T, verbose = T)
summary(model3)
  # 绘制模拟图
library(texreg)
screenreg(list(model1,model2,model3))

gof.1 <- gof(model3)
plot(gof.1)
par( mfrow = c(1,1))
     
set.seed( 2 )
randomg <- rgraph( 320, 1, tprob = .006, mode = "graph" )
randomnet <- as.network( randomg, directed = FALSE )

summary( randomnet )

nullrandom <- ergm( randomnet ~edges )
gof_nullrandom <- gof( nullrandom, GOF =
                         ~degree+distance+espartners+dspartners, seed = 567 )

lhdforesp <- ergm(Network ~edges )
gof_lhdforesp <- gof( lhdforesp, GOF =
                        ~degree+distance+espartners+dspartners, seed = 567 )

dev.off( )

par( mfrow = c( 3,2 ),mai = c( .8,.8,.2,.2 ) )

hist( degree( Network,gmode = "graph" ), breaks=max(degree(Network))/2, main
      = "", xlab = "Degree ( 协同创新网络)",xlim = range( 0:15 ),ylim = range( 0:400
      ) )

hist( degree( randomnet,gmode = "graph" ), breaks=max(degree(randomnet))/2, main = "", xlab = "Degree ( ’随机网络‘
)",xlim = range( 0:15 ),ylim = range( 0:400 ) )

barplot( as.matrix(Edge),gof_lhdforesp$obs.dspart[2:5], main = "", ylab = "Frequency", xlab = "DSP ( 协同创新网络 )", xlim = range( 0:4 ), ylim = range( 0:15000 ) )

barplot( as.matrix(Edge),gof_nullrandom$obs.dspart[2:5], main = "", xlab = "DSP (
random network )", xlim = range( 0:5 ), ylim = range( 0:15000 ) )

barplot( gof_lhdforesp$obs.espart[2:5], main = "", ylab = "Frequency",xlab = "ESP ( LHD )", xlim = range( 0:5 ),ylim = range(
  0:800 ) )

barplot( barplot(as.matrix(df)),gof_nullrandom$obs.espart[2:5], main = "", xlab = "ESP (
random network )", xlim = range( 0:5 ),ylim = range( 0:800 ) )   