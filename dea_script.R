install.packages("Benchmarking")
install.packages("dplyr")
install.packages("xtable")
install.packages("knitr")
library(Benchmarking)
library(dplyr)
library(xtable)
library(knitr)

#Importando a base de dados
hospitais <- read.csv2("hospitais.csv")

#Setando inputs e outputs
inp <- as.matrix(hospitais[,2:3])
out <- hospitais[,4]

#Plotar da fronteira de eficiência 
dea.plot.frontier(inp, out, RTS = "drs", txt=hospitais$Hospital)

#Resultado
result_dea <- dea(inp, out)
summary(result_dea)
eff(result_dea)

result_table <- cbind(hospitais, round(eff(result_dea), 4))
names(result_table)[5]<- "Eficiência"

result_table


print(xtable(result_table, type = "latex"), file = "filename2.tex")


dea_folga <- dea(inp,out,SLACK=TRUE)
dea_folga_table <- data.frame(dea_folga$sx,dea_folga$sy)
names(dea_folga_table) <- c("Folga Insumo 1", "Folga Insumo 2", "Folga Output")
print(xtable(dea_folga_table, type = "latex"), file = "filename3.tex")


dist <- dist(hospitais)

hc <-  hclust(dist)
plot(hc)
