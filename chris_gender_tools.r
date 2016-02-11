# Gender and Tools
# 80 men vs 32 women (and 1 "doesn't matter"), so counts must be normalized by gender
if(!"reshape2" %in% installed.packages()){
    install.packages("reshape2")}
library(reshape2)


Tools <- aggregate(data_clean_average[c("tools.c/c++","tools.dropbox",
"tools.excel","tools.ggplot2","tools.github","tools.drive","tools.latex",
"tools.lattice","tools.matlab","tools.python","tools.regular.expressions",
"tools.rstudio","tools.shell","tools.spss","tools.sql","tools.stata",
"tools.sweave/knitr","tools.html.css.jsm", "tools.xml"
)],by=list(data_clean_average$pronoun),FUN=sum,na.rm=FALSE)

names(Tools) = c("Group.1",tools.titles)

male <- function(x){x/80}
female <- function(x){x/32}
Tools.n <- Tools
Tools.n[2,c(2:length(Tools[2,]))] <- 
  apply(Tools[2,c(2:length(Tools[2,]))],MARGIN=1,FUN=male)
Tools.n[3,c(2:length(Tools[3,]))] <- 
  apply(Tools[3,c(2:length(Tools[3,]))],MARGIN=1,FUN=female)
Tools.m <- melt(Tools.n,id.vars='Group.1')
ggplot(Tools.m, aes(Group.1, value)) +
geom_bar(aes(fill = variable), position = "dodge", stat="identity") + ggtitle('Tool Use by Gender')
