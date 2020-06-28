library(bnlearn)
library(visNetwork)
library(ggplot2)
plot.network <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}


df <- read.csv('G:\\m.tech 2ndsem\\pgm\\New folder\\Support Devices_cut_df.csv')
str(df)
df <- japply( df,which(sapply(df, class)=="integer"), as.numeric )
df <- data.frame(df)
df <- sapply(df,as.factor)
df <- data.frame(df)

res <- hc(df)
str(res)

model <- bn.fit(res,data = df)
plot.network(res)
summary(model)

str()
df$Support.Devices
test <- df[, !names(df) %in% c('Support.Devices')]
pred <- predict(model,node='Support.Devices',data = test)
confusion<-table(df$Support.Devices,pred)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
saveRDS(res, "G:\\m.tech 2ndsem\\pgm\\Support.Devices.rds")


super_model <- readRDS("G:\\m.tech 2ndsem\\pgm\\final_model1.rds")
print(super_model)
super_model$Age
