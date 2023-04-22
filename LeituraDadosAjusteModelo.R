require(readxl)

dados = read_excel("PoliticalInstitutionsData.xlsx",sheet=6,skip=0)[199:7289,c(1,2,4,6:10)]
dados = data.frame(dados)

dados2 = dados

paises = which(table(dados2$Code)==36)

regioes = rep(NA,l=dim(dados2)[1])

regioes[which(dados2$Region=="East Asia & Pacific")] = 1
regioes[which(dados2$Region=="Europe & Central Asia")] = 2
regioes[which(dados2$Region=="Latin America & Caribbean")] = 3
regioes[which(dados2$Region=="Middle East & North Africa")] = 4
regioes[which(dados2$Region=="North America")] = 5
regioes[which(dados2$Region=="South Asia")] = 6
regioes[which(dados2$Region=="Sub-Saharan Africa")] = 7
dados2$Region = regioes


aux3 = c()
for(i in 1:length(paises))
{
  aux3 = rbind(aux3,which(dados2$Code==names(paises)[i]))
}

dados3 = dados2[aux3,]

table(dados3$Code)
table(dados3$Year)
table(dados3$Region)

J = length(unique(dados3$Code))
T = length(unique(dados3$Year))
K = 4
R = length(unique(dados3$Region))

y = array(NA,dim=c(J,K,T))
codigo = c()
nome = c()
reg = c()

for(j in 1:J)
{
  for(k in 3:6)
  {
    y[j,k-2,] = as.numeric(dados3[which(dados3$Code==unique(dados3$Code)[j]),k+1])
  }  
  codigo[j] = unique(dados3$Code)[j]
  nome[j] = unique(dados3$Country..standardized.names.)[j]
  reg[j] = dados3$Region[j]
}

valor = 4

quant = c()
for(j in 1:J)
{
  quant = c(quant,length(which(is.na(y[j,1,])==TRUE)))
}
y = y[-which(quant>valor),,]
codigo = codigo[-which(quant>valor)]
nome = nome[-which(quant>valor)]
reg = reg[-which(quant>valor)]
J = dim(y)[1]

quant = c()
for(j in 1:J)
{
  quant = c(quant,length(which(is.na(y[j,2,])==TRUE)))
}
y = y[-which(quant>valor),,]
codigo = codigo[-which(quant>valor)]
nome = nome[-which(quant>valor)]
reg = reg[-which(quant>valor)]
J = dim(y)[1]

quant = c()
for(j in 1:J)
{
  quant = c(quant,length(which(is.na(y[j,3,])==TRUE)))
}
y = y[-which(quant>valor),,]
codigo = codigo[-which(quant>valor)]
nome = nome[-which(quant>valor)]
reg = reg[-which(quant>valor)]
J = dim(y)[1]


quant = c()
for(j in 1:J)
{
  quant = c(quant,length(which(is.na(y[j,4,])==TRUE)))
}
y = y[-which(quant>valor),,]
codigo = codigo[-which(quant>valor)]
nome = nome[-which(quant>valor)]
reg = reg[-which(quant>valor)]
J = dim(y)[1]

dim(y)

y2 = y
for(j in 1:J)
{
  for(k in 1:K)
  {
    for(t in 1:T)
    {
      if ((is.na(y[j,k,t])==FALSE)&(y[j,k,t]==0)){y2[j,k,t]=y[j,k,t]+0.05}
      if ((is.na(y[j,k,t])==FALSE)&(y[j,k,t]==1)){y2[j,k,t]=y[j,k,t]-0.05}
    }
  }
}

ytransf = log(y2/(1-y2))

require(rjags)

K = dim(ytransf)[2]
T = dim(ytransf)[3] 
J = dim(ytransf)[1]

p.real = 1

jags <- jags.model("MultilevelFactorModel.bug",data=list("y"=ytransf,"P"= p.real, "K" = K, "T" = T,"J" = J))

update(jags,50000)


amostra <- jags.samples(jags,c("beta","sigma2","f","betaW","fW","alpha","y","Gama","GamaW","Lambda","LambdaW"),600000, thin = 300)
