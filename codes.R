library(xlsx)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)
library(igraph)
library(googleVis)
library(lattice)
library(tree)
library(sqldf)
library(GGally)
library(Rmisc)
library(reshape2)

#Data Cleaning and Preliminary Work
# Load the data
data <- read.xlsx("Survey+Response.xlsx", sheetName="Form Responses 1")

# Function that returns a list of the names of the blank columns
FindBlankCols <- function(x) {
  blank_cols <- rep(0,ncol(x))
  for (k in 1:ncol(x)) {
    name = colnames(x)[k]
    if (sum(is.na(x[, name])) == nrow(x))
    {
      blank_cols[k] = name
    }
  }
  blank_cols = blank_cols[blank_cols != 0]
  return(blank_cols)
}


# We drop all the blank columns
blank_cols <- FindBlankCols(data)
data_clean <- data[, ! names(data) %in% blank_cols, drop = F]

# Rename the remaining columns
data_clean <- data_clean %>%
  rename(waitlist = Are.you.on.the.waiting.list.) %>%
  rename(tools = Experiences.with.tools) %>%
  rename(Rdata_manipulation_modeling = Programming.and.Analytical.Experiences..R..data.manipulation.and.modeling.) %>%
  rename(pronoun = What.is.your.preferred.gender.pronoun.) %>%
  rename(text_editor = What.code.text.editor.do.you.use.most.) %>%
  rename(Rgraphic_basics = 
           Programming.and.Analytical.Experiences..R..graphic.basics..base..lattice..grid.etc....) %>%
  rename(Radvanced = Programming.and.Analytical.Experiences..R..advanced..multivariate.data.analysis..e.g..spatiotemporal.data..visualization.and.modeling..) %>%
  rename(RMarkdown = Programming.and.Analytical.Experiences..Reproducible.documentation.with.R..e.g..R.Markdown..) %>%
  rename(Matlab = Programming.and.Analytical.Experiences..Matlab..data.manipulation..analysis..visualization.and.modeling.) %>%  
  rename(Github = Programming.and.Analytical.Experiences..Github.)


#See what's misclassified
#unique(data_clean$Program)

#convert some misclassified label to unified ones
#Ms in ds, MSDS, Data Science to IDSE (master) 
#QMSS (master) to QMSS
data_clean$Program[data_clean$Program == "MSDS"] = "IDSE (master)"
data_clean$Program[data_clean$Program == "Ms in ds"] = "IDSE (master)"
data_clean$Program[data_clean$Program == "Data Science"] = "IDSE (master)"
data_clean$Program[data_clean$Program == "QMSS (master)"] = "QMSS"


#Add Dummy Variables for text_editor Column
data_clean$text_editor <- tolower(data_clean$text_editor)

ed.titles <- c('sublime','ipython','wrangler', 'atom', 'notepad', 'rstudio', 'webstorm', 
               'pycharm', 'eclipse', 'jupyter', 'stata', 'python', 'textmate', 'vi.vim', 'xcode')

ed.regular <- c('subl', 'ipy', 'wrang', 'atom', 'notep', 'rstud', 'storm', 'charm', 'eclips', 
                'jupyt', 'stata', '^pyth','textmate', 'vi', 'xcode')

for(i in 1:length(ed.titles)){
  column.name <- paste('editor', ed.titles[i], sep = '.')
  data_clean <- cbind(data_clean, col = rep(0, nrow(data_clean)))
  colnames(data_clean)[dim(data_clean)[2]] = column.name
  data_clean[,dim(data_clean)[2]][grep(ed.regular[i], data_clean$text_editor)] <- 1
}

#Add Dummy variables for tools Column
data_clean$tools <- tolower(as.character(data_clean$tools))
commas <- strsplit(data_clean$tools, ',')

list.tools <- c()
for( i in 1:length(data_clean$tools)){
  list.tools <- c(list.tools, commas[[i]])
}
list.tools <- sort(unique(trimws(list.tools)))


tools.regular <- c("c/c","dropbox","excel","ggplot2","github","drive",
                   "latex","lattice","matlab","python","regula","rstudio",
                   "shell","spss","sql","stata","sweave/knitr",
                   "web", "xml")

tools.titles <- c("c/c++","dropbox","excel","ggplot2","github","drive",
                  "latex","lattice","matlab","python","regular.expressions","rstudio",
                  "shell","spss","sql","stata","sweave/knitr",
                  "html.css.jsm", "xml")

for(i in 1:length(tools.titles)){
  column.name <- paste('tools', tools.titles[i], sep = '.')
  data_clean <- cbind(data_clean, col = rep(0, nrow(data_clean)))
  colnames(data_clean)[dim(data_clean)[2]] = column.name
  data_clean[,dim(data_clean)[2]][grep(tools.regular[i], data_clean$tools)] <- 1
}

for(i in 1:length(data_clean$tools)){
  if('r' %in% trimws(commas[[i]])){
    if(data_clean$tools.rstudio[i] == 1){
      a_a = 0
    }
    else{
      data_clean$tools.rstudio[i] = 1
    }
  }
}

#Convert experience level to number
data_clean[,c(4,7:11)] <- sapply(data_clean[,c(4,7:11)],as.character) 
data_clean[data_clean == "None"] = 1
data_clean[data_clean == "A little"] = 2
data_clean[data_clean == "Confident"] = 3
data_clean[data_clean == "Expert"] = 4

#Add average confidence level for each person
data_clean_average = data_clean %>%
  mutate(average_confidence = (as.numeric(Rdata_manipulation_modeling) + 
                                 as.numeric(Rgraphic_basics) + as.numeric(Radvanced)  + as.numeric(RMarkdown) + 
                                 as.numeric(Matlab) + as.numeric(Github))/6)


#Figure A1
#Number of students from each program
Programs = as.vector(data_clean$Program)
#animalFactor <- as.factor(animals)
ggp = ggplot(data.frame(Programs),aes(x=Programs),main="a") +  ggtitle('Students from Each Program') + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# bar graph of counts
ggp + geom_bar(fill="lightgreen")

#Figure A2
#Text Editor Use
data.editor <- data_clean[,grep('editor', colnames(data_clean))][-1]
editors.names = substring(colnames(data.editor), first = 8, last = 1000)
sum.editor <- data.frame(editors = editors.names, count = colSums(data.editor))
sum.editor.2 <- subset(sum.editor, count > 1)

edit.bar <- ggplot(data = sum.editor.2, aes(x = editors, y = count)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

bar.col = c('darkblue', 'darkblue','darkblue','darkblue','darkblue','orange',
            'darkblue','darkblue','darkblue')  

edit.bar <- edit.bar + geom_bar(stat = 'identity', fill = bar.col, width = 1, 
                                position = 'dodge', colour = 'white') + ggtitle('Students per Text Editor') + 
  theme(plot.title = element_text(size = rel(1.2)), axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), text = element_text(size = 15)) + 
  annotate("text", x = 6, y = 53, label = "54% use Rstudio", colour= 'white') + 
  geom_text(aes(label=count), vjust = -0.5, colour = 'black')

edit.bar


#Figure A3
# Text Editor and Average Experience
#Weak
weak <- data_clean_average %>% 
  filter(average_confidence < 1.5) %>%
  dplyr::select(editor.sublime, editor.rstudio, editor.notepad, editor.vi.vim) 

weak <- colSums(weak)/sum(weak)
weak <- weak[weak != 0]
weak <- data.frame(weak)

#Strong
strong <- data_clean_average %>% 
  filter(average_confidence > 2.5) %>%
  dplyr::select(editor.sublime, editor.rstudio, editor.notepad, editor.vi.vim) 

strong <- colSums(strong)/sum(strong)
strong <- strong[strong != 0]
strong <- data.frame(strong)

#Medium
medium <- data_clean_average %>% 
  filter(average_confidence <= 2.5, average_confidence >= 1.5) %>%
  dplyr::select(editor.sublime, editor.rstudio, editor.notepad, editor.vi.vim) 

medium <- colSums(medium)/sum(medium)
medium <- medium[medium != 0]
medium <- data.frame(medium)

# Creation of the labels (editors)
lbls <- rownames(weak)
for (k in 1:length(lbls)) {
  var = lbls[k]
  lbls[k] = unlist(strsplit(var, 'editor.'))[2]
}

count <- t(cbind(weak$weak, medium$medium, strong$strong))
colnames(count) <- lbls
rownames(count) <- c("weak", "medium", "strong")

barplot(count, main="Students' Average Experience Level per Text Editor",
        xlab="text editor", ylab="% of people for a given (level, text editor)",
        col=c("green", "blue", "red"),
        legend = c('weak (confidence<1.5)', 
                   'medium (1.5<confidence<2.5)',
                   'strong (confidence>2.5)'
        ), ylim=c(0, 0.8), beside=TRUE)


#Figure A4
#Students per tool
library(RColorBrewer)
data.tools <- data_clean[,grep('tool', colnames(data_clean))][-1]
tools.names <- substring(colnames(data.tools), first = 7, last = 1000)
sum.tools <- data.frame(tools = tools.names, count = colSums(data.tools))
sum.tools.ord <- transform(sum.tools, tools = reorder(tools, count))

tool.bar <- ggplot(data = sum.tools.ord, aes(x = tools, y = count))
cols <- colorRampPalette(brewer.pal(9,'Blues'))(dim(sum.tools)[1])


tool.bar <- tool.bar + geom_bar(stat = 'identity', fill = cols, width = 1, 
                                position = 'dodge', colour = 'lightblue') +   ggtitle('Students per Tool') + 
  theme(plot.title = element_text(size = rel(1.2)), axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), text = element_text(size = 15)) + coord_flip() +
  geom_text(aes(label=count), hjust = -.5, colour = 'red', size = 4.5) + 
  scale_fill_brewer()

tool.bar


#Figure A5
#Experience Level with Tools
skills <- c('Rgraphic_basics', 'Radvanced', 'RMarkdown', 'Github')
data.skills <- data_clean[,skills]
data.skills[data.skills == 1] <- 'None'
data.skills[data.skills == 2] <- "A little"
data.skills[data.skills == 3] <- "Confident"
data.skills[data.skills == 4] <- "Expert"


datm <- melt(cbind(data.skills, ind = rownames(data.skills)), id.vars = c('ind'))

par(mar = c(5, 4, 4, 10) + 0.1)

barplot.skills = barplot(t(table(datm[,-1])), main='Experience with Tools',
                         col = c('darkblue', 'green', 'red', 'orange'))
par(xpd = TRUE)

legend("topright", inset=c(-0.2,0), legend=c("A little","Confident", 'Expert',
                                             'None'), fill = c('darkblue', 'green', 'red', 'orange'))

#Figure A6
#Average Experience in Tools per Program
data_clean$Radvanced = as.numeric(data_clean$Radvanced)
data_clean$Rgraphic_basics = as.numeric(data_clean$Rgraphic_basics)
data_clean$RMarkdown = as.numeric(data_clean$RMarkdown)
data_clean$Matlab = as.numeric(data_clean$Matlab)
data_clean$Github = as.numeric(data_clean$Github)

mean.skills = aggregate(.~Program, FUN=mean, data = data_clean[, c('Program', 'Radvanced',
                                                                   'Rgraphic_basics','RMarkdown', 'Matlab', 'Github')])

y <- melt(mean.skills)

p <- ggplot(y, aes(x=Program,y=variable))
p + geom_tile(aes(fill=value), color = 'grey') + 
  scale_fill_gradient(low="white", high="#0072B2") + xlab("Skills") + ylab("Programs") + 
  ggtitle('Average Experience per Program') +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.2, vjust = 0.5, size = 10),
        plot.title = element_text(size = rel(1.4)), axis.text.y = element_text(size=10))


#Figure A7
#Experience Levels per Tools
# First Sankey diagram
require(igraph)
require(googleVis)

g <- graph.tree(9, children = 4)
g <- add_edges(g, c(3,6,3,7,3,8,3,9,4,6,4,7,4,8,4,9,5,6,5,7,5,8,5,9))

# Preparation of the weights
weights_1 <- data_clean_average %>%
  filter(tools.rstudio == 1) %>%
  dplyr::select(Rdata_manipulation_modeling, Rgraphic_basics) %>%
  group_by(Rdata_manipulation_modeling) %>%
  summarise(count = n())

weights_2 <- data_clean_average %>%
  filter(tools.rstudio == 1) %>%
  dplyr::select(Rdata_manipulation_modeling, Rgraphic_basics) %>%
  group_by(Rdata_manipulation_modeling, Rgraphic_basics) %>%
  summarise(count = n())

weights <- seq(20)
answers <- weights_1$Rdata_manipulation_modeling

for (k in seq(4)) {
  if (!is.element(toString(k), answers)){
    weights[k] = 0
  }
}


for (node in weights_1$Rdata_manipulation_modeling) {
  weights[strtoi(node)] = weights_1[weights_1$Rdata_manipulation_modeling == node,]$count
}

#Preparation of  weights_2
weights_2 <- data.frame(weights_2)
answers <- weights_2$Rdata_manipulation_modeling

for (i in seq(4)) {
  for (j in seq(4)) {
    if (dim(filter(weights_2, Rdata_manipulation_modeling==toString(i), Rgraphic_basics==toString(j)))[1] == 0) {
      #Add the row
      weights_2 = rbind(weights_2, c(toString(i), toString(j), '0'))
    }
  }
}

weights_2 <- arrange(weights_2, Rdata_manipulation_modeling, Rgraphic_basics)

weights[5:length(weights)] = weights_2$count
weights <- strtoi(weights)

E(g)$weight = weights
edgelist <- get.data.frame(g) 
colnames(edgelist) <- c("source","target","value")
VAR <- c('r','None1', 'a little1', 'confident1', 'expert1')
edgelist$source <- VAR[edgelist$source]
TARGET <- c('coline','None1', 'a little1', 'confident1', 'expert1', 'None2', 'a little2', 'confident2', 'expert2')
edgelist$target <- TARGET[edgelist$target]

plot(
  gvisSankey(edgelist, from="source", 
             to="target", weight="value",
             options=list(
               sankey="{link: {colorMode: 'gradient', colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f','#cab2d6', '#ffff99', '#1f78b4', '#33a02c'] },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )






#### Second Sankey diagram

g <- graph.tree(9, children = 4)
g <- add_edges(g, c(3,6,3,7,3,8,3,9,4,6,4,7,4,8,4,9,5,6,5,7,5,8,5,9))

# Preparation of the weights
weights_1 <- data_clean_average %>%
  filter(tools.rstudio == 1) %>%
  dplyr::select(Radvanced, Github) %>%
  group_by(Radvanced) %>%
  summarise(count = n())

weights_2 <- data_clean_average %>%
  filter(tools.rstudio == 1) %>%
  dplyr::select(Radvanced, Github) %>%
  group_by(Radvanced, Github) %>%
  summarise(count = n())

weights <- seq(20)
answers <- weights_1$Radvanced

for (k in seq(4)) {
  if (!is.element(toString(k), answers)){
    weights[k] = 0
  }
}


for (node in weights_1$Radvanced) {
  weights[strtoi(node)] = weights_1[weights_1$Radvanced == node,]$count
}

#Preparation of weights_2
weights_2 <- data.frame(weights_2)
answers <- weights_2$Radvanced

for (i in seq(4)) {
  for (j in seq(4)) {
    if (dim(filter(weights_2, Radvanced==toString(i), Github==toString(j)))[1] == 0) {
      #Add the row
      weights_2 = rbind(weights_2, c(toString(i), toString(j), '0'))
    }
  }
}

weights_2 <- arrange(weights_2, Radvanced, Github)

weights[5:length(weights)] = weights_2$count
weights <- strtoi(weights)


E(g)$weight = weights
edgelist <- get.data.frame(g) 
colnames(edgelist) <- c("source","target","value")
VAR <- c('r','None1', 'a little1', 'confident1', 'expert1')
edgelist$source <- VAR[edgelist$source]
TARGET <- c('coline','None1', 'a little1', 'confident1', 'expert1', 'None2', 'a little2', 'confident2', 'expert2')
edgelist$target <- TARGET[edgelist$target]


plot(
  gvisSankey(edgelist, from="source", 
             to="target", weight="value",
             options=list(
               sankey="{link: {colorMode: 'gradient', colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f','#cab2d6', '#ffff99', '#1f78b4', '#33a02c'] },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )

#Decision tree for Predicting Gender
#explore the possibility that people's confidence/expertise level in each programming 
#and analytical tool could be differentiated by gender
data_clean = tbl_df(data_clean)
#rename certain column names in the form that can be read by the tree() function 
#since it doesn't recognize "+" or "/"
names(data_clean)[16] = "editor.notepadpp"
names(data_clean)[27] = "tools.c.or.cpp"
names(data_clean)[43] = "tools.sweave.or.knitr"

data_clean_binary_pronoun = data_clean[-which(data_clean$pronoun=="doesn't matter" | 
                                                is.na(data_clean$pronoun) == TRUE),]
data_clean_without_gender = data_clean[which(data_clean$pronoun=="doesn't matter" | 
                                               is.na(data_clean$pronoun) == TRUE),]

#build tree classifier for gender classification of two unknown cases
#data_clean is original data, data_clean_binary_pronoun is our training data
#data_clean_without_gender is our test data, i.e. we need to predict their gender
data_clean_training = data_clean_binary_pronoun[,-c(3,6)]
data_clean_test = data_clean_without_gender[,-c(3,6)]
#we use sql-ish package in r to change the gender indicators to initials f and m, 
#which would make the resulting tree look less crowded

data_clean_training=sqldf("select * ,
                          CASE WHEN pronoun=='he/him' THEN 'm'
                          WHEN pronoun=='she/her' THEN 'f'
                          END pronoun from data_clean_training")
data_clean_training = data_clean_training[,-4]
data_clean_training$pronoun = as.factor(data_clean_training$pronoun)
tree.train=tree(pronoun~.,data_clean_training)

plot(tree.train)
text(tree.train, pretty=0)

1-sum(data_clean_training$pronoun=='m')/nrow(data_clean_training)
#we notice the misclassification error rate of this single tree before any pruning or 
#tuning is about 17.0%; this is already better than theerror rate of 28.6% when we uniformly
#choose the gender to be male, which gives us reasons to continue to use tree classifier. 
#But this is only the training error; we need to determine tree with the lowest test error. 
#Thus we randomly select about 80% of observations (90 people) to be our training data 
#and the rest to be test data.
set.seed(1)
train=sample(1:nrow(data_clean_training),90)
tree.test=data_clean_training[-train,]
pronoun.test=data_clean_training$pronoun[-train]
tree.train.subset=tree(pronoun~.,data_clean_training,subset=train)
tree.test.pred=predict(tree.train.subset,tree.test,type="class")


#table(tree.test.pred,pronoun.test)
#the table tells us that out of 22 test cases, 3 females are misclassified as males,
#and 5 males are misclassified as females.
1-sum(tree.test.pred==pronoun.test)/length(tree.test.pred)
#the test error rate is about 36.4%

#we now apply cross-validation to determine the optimal level of tree complexity
set.seed(8)
cv.tree.train=cv.tree(tree.train.subset, FUN=prune.misclass, K=10)
#cv.tree.train
par(mfrow=c(1,1))
plot(cv.tree.train$size,cv.tree.train$dev,type="b",
     xlab="Tree Size",ylab="10-fold CV Total Error",
     ylim=c(0.99*min(cv.tree.train$dev),1.1*max(cv.tree.train$dev)))
text(cv.tree.train$size,cv.tree.train$dev, labels=cv.tree.train$size,cex=0.8,pos=3)
#we observe from the plot that the 10-fold cross-validation error rates with 6 
#terminal nodes are lowest, and lower than that of the original tree with 
#12 terminal nodes. The y-axis denotes the number of total misclassification cases among
#all 10 folds. We thus pick 6 to be the number of terminal nodes and prune the tree, see if 
#the result is better:
prune.tree.train=prune.misclass(tree.train.subset,best=6)
tree.test.pred=predict(prune.tree.train,tree.test,type="class")


#table(tree.test.pred,pronoun.test)
#the table tells us that out of 22 test cases, 3 females are misclassified as males,
#and 3 males are misclassified as females.
1-sum(tree.test.pred==pronoun.test)/length(tree.test.pred)
#the test error rate is about 27.3%

#Therefore we decide the number of terminal nodes to be 6, and fit the entire dataset 
#with 112 cases to this single tree model, and use the resulting tree to predict the
#gender of two people.
prune.original.tree=prune.misclass(tree.train,best=6)
plot(prune.original.tree)
text(prune.original.tree, pretty=0)
#prediction:
test.pred=predict(prune.original.tree,data_clean_test,type="class")

#which means that the one who wrote "doesn't matter" is a male student, and the 
#one who didn't write anything is a female student.

#we can now update our data set:
data_clean[which(data_clean$pronoun=="doesn't matter"),5] = "he/him"
data_clean[which(is.na(data_clean$pronoun) == TRUE),5] = "she/her"


#Figure B1
#Number of Males and Females from Each Program
ggplot(data_clean, aes(x = Program, fill = pronoun)) + 
  geom_bar(position = "dodge") + ggtitle('Males and Females Per Program') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Figure B2
# Tools vs. Gender
# 80 men vs 32 women (and 1 "doesn't matter"), so counts must be normalized by gender
Tools <- aggregate(data_clean[c("tools.c.or.cpp","tools.dropbox",
                                "tools.excel","tools.ggplot2","tools.github","tools.drive","tools.latex",
                                "tools.lattice","tools.matlab","tools.python","tools.regular.expressions",
                                "tools.rstudio","tools.shell","tools.spss","tools.sql","tools.stata",
                                "tools.sweave.or.knitr","tools.html.css.jsm", "tools.xml"
)],by=list(data_clean$pronoun),FUN=sum,na.rm=FALSE)

names(Tools) = c("Group.1",tools.titles)

male <- function(x){x/81}
female <- function(x){x/33}
Tools.n <- Tools
Tools.n[1,c(2:length(Tools[1,]))] <- 
  apply(Tools[1,c(2:length(Tools[1,]))],MARGIN=1,FUN=male)
Tools.n[2,c(2:length(Tools[2,]))] <- 
  apply(Tools[2,c(2:length(Tools[2,]))],MARGIN=1,FUN=female)
Tools.m <- melt(Tools.n,id.vars='Group.1')
ggplot(Tools.m, aes(Group.1, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + ggtitle('Tool Use by Gender')


#Figure B3
#Number of Tools vs. Gender
data_clean_average1 = data_clean_average[data_clean_average$pronoun!="doesn't matter",]
data_clean_average1 = data_clean_average1[-97,]
number_tools=apply(data_clean_average1[,c(27:45)],1,sum)
data_clean_average1 = cbind(data_clean_average1,number_tools)
qplot(factor(pronoun), number_tools, data = data_clean_average1, geom = "boxplot")


#Figure B4
#Experience Level vs. Gender
qplot(factor(pronoun), average_confidence, data = data_clean_average1, geom = "boxplot")



#Figure B5
data_summarize_grouped_by_gender=data_clean_binary_pronoun %>%
  group_by(pronoun) %>%
  select(Rdata_manipulation_modeling,Rgraphic_basics,Radvanced,RMarkdown,
         Matlab,Github) %>%
  summarise(
    avg_Rdata_manipulation_modeling = mean(as.numeric(Rdata_manipulation_modeling)),
    avg_Rgraphic_basics = mean(as.numeric(Rgraphic_basics)),
    avg_Radvanced = mean(as.numeric(Radvanced)),
    avg_RMarkdown = mean(as.numeric(RMarkdown)),
    avg_Matlab = mean(as.numeric(Matlab)),
    avg_Github = mean(as.numeric(Github))
  )

#Average Experience per Tool by Gender
data_averagescore_grouped <- melt(data_summarize_grouped_by_gender)
ggplot(data_averagescore_grouped, aes(y = variable, x = value, shape = pronoun, color = pronoun))+geom_point(size = 7, alpha = 0.7) + 
  ggtitle("Average Experience per Tool by Gender")
            
#Figure B6
# Breakdown of Experience by Gender:  R Data Manipulation and Modeling, R Graphics, R Advanced, R #Markdown, Matlab, and Github
data_gender_indicated = data_clean %>%
  group_by(pronoun) %>%
  select(Rdata_manipulation_modeling,Rgraphic_basics,Radvanced,RMarkdown,
         Matlab,Github) %>%
  mutate(Rdata_manipulation_modeling=as.numeric(Rdata_manipulation_modeling),
         Rgraphic_basics = as.numeric(Rgraphic_basics),
         Radvanced = as.numeric(Radvanced),
         RMarkdown = as.numeric(RMarkdown),
         Matlab = as.numeric(Matlab),
         Github = as.numeric(Github))

Rdata_manipulation_modeling_level_proportion = data_gender_indicated %>%
  select(Rdata_manipulation_modeling) %>%
  group_by(pronoun, Rdata_manipulation_modeling) %>%
  summarise(count=n()) %>%
  mutate(proportion=count/sum(count))

Rgraphic_basics_level_proportion = data_gender_indicated %>%
  select(Rgraphic_basics) %>%
  group_by(pronoun, Rgraphic_basics) %>%
  summarise(count=n()) %>%
  mutate(proportion=count/sum(count))

Radvanced_level_proportion = data_gender_indicated %>%
  select(Radvanced) %>%
  group_by(pronoun, Radvanced) %>%
  summarise(count=n()) %>%
  mutate(proportion=count/sum(count))

RMarkdown_level_proportion = data_gender_indicated %>%
  select(RMarkdown) %>%
  group_by(pronoun, RMarkdown) %>%
  summarise(count=n()) %>%
  mutate(proportion=count/sum(count))

Matlab_level_proportion = data_gender_indicated %>%
  select(Matlab) %>%
  group_by(pronoun, Matlab) %>%
  summarise(count=n()) %>%
  mutate(proportion=count/sum(count))

Github_level_proportion = data_gender_indicated %>%
  select(Github) %>%
  group_by(pronoun, Github) %>%
  summarise(count=n()) %>%
  mutate(proportion=count/sum(count))

pie.tool.exp.1 = ggplot(data=Rdata_manipulation_modeling_level_proportion, 
                        aes(x=factor(Rdata_manipulation_modeling),
                            y=proportion,
                            fill = factor(Rdata_manipulation_modeling))) + 
  geom_bar(width = 1, stat="identity") + 
  facet_grid(facets=. ~ pronoun) + coord_polar() + xlab('') + 
  ylab('Rdata Manipulation & Modeling')+
  guides(fill=guide_legend(title="Tool Experience Level"))

pie.tool.exp.2 = ggplot(data=Rgraphic_basics_level_proportion, 
                        aes(x=factor(Rgraphic_basics),
                            y=proportion,
                            fill = factor(Rgraphic_basics))) + 
  geom_bar(width = 1, stat="identity") + 
  facet_grid(facets=. ~ pronoun) + coord_polar() + xlab('') + 
  ylab('Rgraphic Basics')+
  guides(fill=guide_legend(title="Tool Experience Level"))

pie.tool.exp.3 = ggplot(data=Radvanced_level_proportion, 
                        aes(x=factor(Radvanced),
                            y=proportion,
                            fill = factor(Radvanced))) + 
  geom_bar(width = 1, stat="identity") + 
  facet_grid(facets=. ~ pronoun) + coord_polar() + xlab('') + 
  ylab('Radvanced') +
  guides(fill=guide_legend(title="Tool Experience Level"))

pie.tool.exp.4 = ggplot(data=RMarkdown_level_proportion, 
                        aes(x=factor(RMarkdown),
                            y=proportion,
                            fill = factor(RMarkdown))) + 
  geom_bar(width = 1, stat="identity") + 
  facet_grid(facets=. ~ pronoun) + coord_polar() + xlab('') + 
  ylab('RMarkdown') +
  guides(fill=guide_legend(title="Tool Experience Level"))

pie.tool.exp.5 = ggplot(data=Matlab_level_proportion, 
                        aes(x=factor(Matlab),
                            y=proportion,
                            fill = factor(Matlab))) + 
  geom_bar(width = 1, stat="identity") + 
  facet_grid(facets=. ~ pronoun) + coord_polar() + xlab('') + 
  ylab('Matlab') +
  guides(fill=guide_legend(title="Tool Experience Level"))

pie.tool.exp.6 = ggplot(data=Github_level_proportion, 
                        aes(x=factor(Github),
                            y=proportion,
                            fill = factor(Github))) + 
  geom_bar(width = 1, stat="identity") + 
  facet_grid(facets=. ~ pronoun) + coord_polar() + xlab('') + 
  ylab('Github') +
  guides(fill=guide_legend(title="Tool Experience Level"))

pie.tool.exp.1
pie.tool.exp.2
pie.tool.exp.3
pie.tool.exp.4
pie.tool.exp.5
pie.tool.exp.6


#Figure C1
#Comparison plot of overall confidence level and number of tools a person being able to use efficiently
#add new column of total number of tools each person using 
tooltotal <- rowSums(data_clean[12:45])
data_clean <- cbind(data_clean, tooltotal)
experience <- data_clean$Rdata_manipulation_modeling
#level of confident vs. number of tools  
ggplot(data_clean, aes(x = tooltotal, y = experience, col = pronoun))+geom_jitter() + 
  geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) + ggtitle("Average Experience vs. Number of Tools Known")


#Figure C2
#Experience Level in Individual Tools vs. Total Number of Tools
p1<-ggplot(data_clean, aes(x = tooltotal, y = Github, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p2<-ggplot(data_clean, aes(x = tooltotal, y = Matlab, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p3<-ggplot(data_clean, aes(x = tooltotal, y = RMarkdown, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p4<-ggplot(data_clean, aes(x = tooltotal, y = Radvanced, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p5<-ggplot(data_clean, aes(x = tooltotal, y = Rgraphic_basics, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
multiplot(p1,p2,p3,p4,p5, cols =2)


#Figure C3
#Experience Level in Individual Tools vs. Overall Average Experience
p6<-ggplot(data_clean, aes(y = experience, x = Github, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p7<-ggplot(data_clean, aes(y = experience, x = Matlab, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p8<-ggplot(data_clean, aes(y = experience, x = RMarkdown, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p9<-ggplot(data_clean, aes(y = experience, x = Radvanced, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p10<-ggplot(data_clean, aes(y = experience, x = Rgraphic_basics, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE)+geom_line()
multiplot(p6,p7,p8,p9,p10, cols =2)


#Figure C4
#The pairwise comparison between all six tools with correlation coefficients 
levelcf <- data.frame(sapply(data_clean[7:11], as.numeric))
levelcf
for (i in 1:ncol(levelcf)){
  levelcf[,i] = rnorm(nrow(levelcf), 0, 0.05)+levelcf[,i]
} 
levelcf <- cbind(levelcf, data_clean[5])
#discrete and hard to understand, by jittering add small noise 
ggpairs(data=levelcf, title="tools confident level comparison")



