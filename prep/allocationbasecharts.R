# library(ggplot2)
# library(highcharter)
# 
# dfp <- data.frame(A=c("total"), A1=10, A2=15, A3=5)
# 
# dfp2 <- data.frame(Allocation=c("FL", "AL", "MS", "LA","TX"),
#                    State=c(10,15,5, 35, 35),
#                    col=c("#fb9a99"))
# 
# 
 colors2=c("#fb9a99","#33a02c","#b2df8a","#1f78b4","#a6cee3")
# 
# highchart() %>%
#   hc_title(text= paste("Percent" , "allocation", sep=" ")) %>%
#   hc_subtitle(text="put in selected option here") %>% 
#   hc_chart(type = "column") %>% 
#   hc_xAxis(categories = dfp2$Allocation) %>% 
#   hc_add_series(dfp2$State, 
#                 name = "states", 
#                 showInLegend = FALSE) %>% 
#   hc_plotOptions(
#     column = list(
#       colorByPoint = TRUE,
#       colors=colors2
#     ))
# 
# 
# dfp3 <- dfp2 
# dfp4 <- dfp3[nrow(dfp3),]
# 
# 
# library(ggplot2)


########### ggplot test
 colors2=c("#fb9a99","#33a02c","#b2df8a","#1f78b4","#a6cee3")
gp <- data.frame(Allocation=c("Total Allocation", "Private"),
                 FL=c(36.36, 36.19),
                 AL=c(16.58, 14.96),
                 Options=c(1:nrow(gp)))

library(reshape2)
gp.long <- melt(gp[,2:4], id.vars="Options")
gp.long

p <- ggplot(gp.long,aes(x=variable,y=value,fill=factor(Options)))+
  geom_bar(stat="identity",position="dodge")
p2 <- p + scale_fill_discrete(name='test') +
  xlab("Percent allocation")

+
  scale_fill_discrete(name="",
                      breaks=c(1, 2),
                      labels=c("Male", "Female"))+
  xlab("Beverage")+ylab("Mean Percentage")

