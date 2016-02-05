library(xlsx)
library(dplyr)
require(ggplot2)

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
  
