library(dplyr)
library(ggplot2)

# Load the data
data <- read.csv("Survey+Response.csv", header = TRUE)
data = data[,-1]
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
  rename(Rgraphic_basics = Programming.and.Analytical.Experiences..R..graphic.basics..base..lattice..grid.etc....) %>%
  rename(Radvanced = Programming.and.Analytical.Experiences..R..advanced..multivariate.data.analysis..e.g..spatiotemporal.data..visualization.and.modeling..) %>%
  rename(RMarkdown = Programming.and.Analytical.Experiences..Reproducible.documentation.with.R..e.g..R.Markdown..) %>%
  rename(Matlab = Programming.and.Analytical.Experiences..Matlab..data.manipulation..analysis..visualization.and.modeling.) %>%  
  rename(Github = Programming.and.Analytical.Experiences..Github.)


#See what's misclassified
unique(data_clean$Program)

#convert some misclassified label to unified ones
#Ms in ds, MSDS, Data Science to IDSE (master) 
#QMSS (master) to QMSS
data_clean$Program[data_clean$Program == "MSDS"] = "IDSE (master)"
data_clean$Program[data_clean$Program == "Ms in ds"] = "IDSE (master)"
data_clean$Program[data_clean$Program == "Data Science"] = "IDSE (master)"
data_clean$Program[data_clean$Program == "QMSS (master)"] = "QMSS"
#See how many there are each category
Programs = as.vector(data_clean$Program)
#animalFactor <- as.factor(animals)
ggp = ggplot(data.frame(Programs),aes(x=Programs),main="a")
# counts
ggp + geom_bar(fill="lightgreen")
# proportion
ggp + geom_bar(fill="lightblue",aes(y=..count../sum(..count..))) + ylab("Proportion of Students in each Department")

#pi chart
# 3D Exploded Pie Chart
library(plotrix)
slices = as.vector(table(Programs))
lbls = as.vector(rownames(table(Programs)))
require(graphics)
pie3D(slices,labels=lbls,explode=0.2,radius=1, labelcex=0.7,
      main="Pie Chart of Programs", col = rainbow(length(slices)),labelcol=rainbow(length(slices)))

#waitlist and program
wl = as.vector(data_clean$waitlist)

slices1 = as.vector(table(wl))
lbls1 = as.vector(rownames(table(wl)))
#how many wl (yes,no)
pie3D(slices1,labels=lbls1,explode=0.3,radius=1, labelcex=0.7,
      main="Pie Chart of waitlist", col = rainbow(length(slices)),labelcol=rainbow(length(slices)))
#program propotion of ppl no wl
no_wl_data = data_clean[data_clean$waitlist=="No",]
no_wl_program = as.vector(no_wl_data$Program)
slices2 = as.vector(table(no_wl_program))
lbls2 = as.vector(rownames(table(no_wl_program)))
pie3D(slices2,labels=lbls2,explode=0.3,radius=1, labelcex=0.5,
      main="Pie Chart of off waitlist by program", col = rainbow(length(slices)),labelcol=rainbow(length(slices)))

#program propotion of ppl yes wl
yes_wl_data = data_clean[data_clean$waitlist=="Yes",]
yes_wl_program = as.vector(yes_wl_data$Program)
slices3 = as.vector(table(yes_wl_program))
lbls3 = as.vector(rownames(table(yes_wl_program)))
pie3D(slices3,labels=lbls3,explode=0.3,radius=1, labelcex=0.7,
      main="Pie Chart of on waitlist by program", col = rainbow(length(slices)),labelcol=rainbow(length(slices)))


#Juan
#Add Dummy Variables for text_editor Column
data_clean$text_editor <- tolower(data_clean$text_editor)

ed.titles <- c('sublime','ipython','wrangler', 'atom', 'notepad++', 'rstudio', 'webstorm', 
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
list.tools

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

#implementing part of Yanjin's idea, converting confidence level to number
data_clean[,c(4,7:11)] <- sapply(data_clean[,c(4,7:11)],as.character) 
data_clean[data_clean == "None"] = 1
data_clean[data_clean == "A little"] = 2
data_clean[data_clean == "Confident"] = 3
data_clean[data_clean == "Expert"] = 4

#added average confidence level for each person
data_clean_average = data_clean %>%
  mutate(average_confidence = (as.numeric(Rdata_manipulation_modeling) + 
    as.numeric(Rgraphic_basics) + as.numeric(Radvanced)  + as.numeric(RMarkdown) + 
    as.numeric(Matlab) + as.numeric(Github))/6)


#EJ's code

#explore the possibility that people's confidence/expertise level in each programming 
#and analytical tool could be differentiated by gender
#Notice besides the one indicating his/her gender doesn't matter, there's one person 
#doesn't right anything at all. So I use a single decision tree to determine their gender
#and then conduct the exploratory data analysis based on gender.
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
library(tree)
#data_clean is original data, data_clean_binary_pronoun is our training data
#data_clean_without_gender is our test data, i.e. we need to predict their gender
data_clean_training = data_clean_binary_pronoun[,-c(3,6)]
data_clean_test = data_clean_without_gender[,-c(3,6)]
#we use sql-ish package in r to change the gender indicators to initials f and m, 
#which would make the resulting tree look less crowded
library(sqldf)
data_clean_training=sqldf("select * ,
                          CASE WHEN pronoun=='he/him' THEN 'm'
                          WHEN pronoun=='she/her' THEN 'f'
                          END pronoun from data_clean_training")
data_clean_training = data_clean_training[,-4]
data_clean_training$pronoun = as.factor(data_clean_training$pronoun)
tree.train=tree(pronoun~.,data_clean_training)
summary(tree.train)
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
table(tree.test.pred,pronoun.test)
#the table tells us that out of 22 test cases, 3 females are misclassified as males,
#and 5 males are misclassified as females.
1-sum(tree.test.pred==pronoun.test)/length(tree.test.pred)
#the test error rate is about 36.4%

#we now apply cross-validation to determine the optimal level of tree complexity
set.seed(8)
cv.tree.train=cv.tree(tree.train.subset, FUN=prune.misclass, K=10)
cv.tree.train
summary(tree.train.subset)
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
table(tree.test.pred,pronoun.test)
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
test.pred
#which means that the one who wrote "doesn't matter" is a male student, and the 
#one who didn't write anything is a female student.

#we can now update our data set:
data_clean[which(data_clean$pronoun=="doesn't matter"),5] = "he/him"
data_clean[which(is.na(data_clean$pronoun) == TRUE),5] = "she/her"


#now begin our exploratory data analysis of experience in programming and 
#analytical tools differentiated by gender
data_grouped_by_gender=data_clean %>%
  select(pronoun,Rdata_manipulation_modeling,Rgraphic_basics,Radvanced,RMarkdown,
         Matlab,Github) %>%
  group_by(pronoun) %>%
  summarise(
    avg_Rdata_manipulation_modeling = round(mean(as.numeric(Rdata_manipulation_modeling)),2),
    avg_Rgraphic_basics = round(mean(as.numeric(Rgraphic_basics)),2),
    avg_Radvanced = round(mean(as.numeric(Radvanced)),2),
    avg_RMarkdown = round(mean(as.numeric(RMarkdown)),2),
    avg_Matlab = round(mean(as.numeric(Matlab)),2),
    avg_Github = round(mean(as.numeric(Github)),2)
  )

avg_list=c("avg_Rdata_manipulation_modeling","avg_Rgraphic_basics",
           "avg_Radvanced","avg_RMarkdown","avg_Matlab","avg_Github")
tool_name_list=c("Rdata Manipulation & Modeling","Rgraphic Basics","Radvanced",
                 "RMarkdown","Matlab","Github")
library(grid)
library(gridExtra)

plotting1 = ggplot(data_grouped_by_gender, 
                  aes(x=factor(pronoun), y=factor(get(avg_list[1])), fill = factor(pronoun))) + 
  geom_bar(position = "dodge", stat="identity") + 
  labs(x="Gender",y=tool_name_list[1]) + 
  theme(legend.position = "none")
plotting2 = ggplot(data_grouped_by_gender, 
                  aes(x=factor(pronoun), y=factor(get(avg_list[2])), fill = factor(pronoun))) + 
  geom_bar(position = "dodge", stat="identity") + 
  labs(x="Gender",y=tool_name_list[2]) + 
  theme(legend.position = "none")
plotting3 = ggplot(data_grouped_by_gender, 
                   aes(x=factor(pronoun), y=factor(get(avg_list[3])), fill = factor(pronoun))) + 
  geom_bar(position = "dodge", stat="identity") + 
  labs(x="Gender",y=tool_name_list[3]) + 
  theme(legend.position = "none")
plotting4 = ggplot(data_grouped_by_gender, 
                   aes(x=factor(pronoun), y=factor(get(avg_list[4])), fill = factor(pronoun))) + 
  geom_bar(position = "dodge", stat="identity") + 
  labs(x="Gender",y=tool_name_list[4]) + 
  theme(legend.position = "none")
plotting5 = ggplot(data_grouped_by_gender, 
                   aes(x=factor(pronoun), y=factor(get(avg_list[5])), fill = factor(pronoun))) + 
  geom_bar(position = "dodge", stat="identity") + 
  labs(x="Gender",y=tool_name_list[5]) + 
  theme(legend.position = "none")
plotting6 = ggplot(data_grouped_by_gender, 
                   aes(x=factor(pronoun), y=factor(get(avg_list[6])), fill = factor(pronoun))) + 
  geom_bar(position = "dodge", stat="identity") + 
  labs(x="Gender",y=tool_name_list[6]) + 
  theme(legend.position = "none")
grid.arrange(plotting1,plotting2,plotting3,plotting4,plotting5,plotting6,nrow=3,ncol=2,
             bottom=textGrob("Average Experience Level for Programming and Analytical Tools",gp=gpar(fontsize=12,font=3,col=1)))

#really don't understand why the following chunk of code doesn't work;
#it keeps displaying the same graph for all six plots, which is the last one with 
#Github vs. gender. It works on Tuesday....
'''
plots <- list()# new empty list
for (i in (1:6)) {
  plotting=ggplot(data_grouped_by_gender, 
                aes(x=factor(pronoun), y=factor(get(avg_list[i])), fill = factor(pronoun))) + 
           geom_bar(position = "dodge", stat="identity") + 
           labs(x="Gender",y=tool_name_list[i]) + 
           theme(legend.position = "none")
  plots[[j]] <- plotting
}
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],nrow=3,ncol=2,
             bottom=textGrob("Average Experience Level for Programming and Analytical Tools",gp=gpar(fontsize=12,font=3,col=1)))
'''

#exploration of tool experience proportion of each level (from 1-4)
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

Rdata_manipulation_modeling_level_proportion
Rgraphic_basics_level_proportion
Radvanced_level_proportion
RMarkdown_level_proportion
Matlab_level_proportion
Github_level_proportion

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


#the 5-by-3 plot with pairwise tool experience comparison
tools.list = c("Rdata_manipulation_modeling", "Rgraphic_basics","Radvanced","RMarkdown",
               "Matlab","Github")
pairewise.plots <- list()
k = 0
for (i in (1:5)) {
  for (j in ((i+1):6)) {
    k=k+1
    gp=ggplot(data_clean, aes(x=tools.list[i], y=tools.list[j], col = pronoun)) + 
      geom_jitter() + labs(x=tool_name_list[i],y=tool_name_list[j]) + 
      theme(axis.text=element_blank(),legend.position = "none")
    pairewise.plots[[k]] <- gp
  }
}
library(Rmisc)
multiplot(plotlist = pairewise.plots, cols = 3)


