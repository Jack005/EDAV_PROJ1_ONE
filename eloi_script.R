library(xlsx)
library(dplyr)
require(ggplot2)

setwd("/Users/eloimorlaas/Documents/Columbia/Spring_2016/EDAV/Proj1_the_class")
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
blank_cols = FindBlankCols(data)
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
  

# Quick summary plots
require(scales)

#Repartition of the programs
ggplot(data_clean, aes(x = as.factor(Program))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Repartition of the programs", y = "Percent", x = "Program")


# Waitlist
qplot(data_clean$waitlist, main=c('In the waitlist ?')) #With the count

ggplot(data_clean, aes(x = as.factor(waitlist))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Repartition of the programs", y = "Percent", x = "In the waitlist ?")
  

#Juan
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



# Histograms of the text editors in function of the avg confidence
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

barplot(count, main="Text editor distribution according to the skills level",
        xlab="text editor", ylab="% of people for a given (level, text editor)",
        col=c("green", "blue", "red"),
        legend = c('weak (confidence<1.5)', 
                   'medium (1.5<confidence<2.5)',
                   'strong (confidence>2.5)'
                   ), ylim=c(0, 0.8), beside=TRUE)



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

#Idem for weights_2
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
VAR <- c('r','very weak1', 'weak1', 'medium1', 'strong1')
edgelist$source <- VAR[edgelist$source]
TARGET <- c('coline','very weak1', 'weak1', 'medium1', 'strong1', 'very weak2', 'weak2', 'medium2', 'strong2')
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

#Idem for weights_2
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
VAR <- c('r','very weak1', 'weak1', 'medium1', 'strong1')
edgelist$source <- VAR[edgelist$source]
TARGET <- c('coline','very weak1', 'weak1', 'medium1', 'strong1', 'very weak2', 'weak2', 'medium2', 'strong2')
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

