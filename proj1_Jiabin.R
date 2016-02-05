library(xlsx)
library(dplyr)
library(ggplot2)

# Load the data
data <- read.xlsx("~/Desktop/W4701/Survey+Response.xlsx", sheetName="Form Responses 1")

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
ggp + geom_bar(fill="lightblue",aes(y=..count../sum(..count..)))


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


