#Yanjin:
#Average confidence level comparison based on the dimension of genders
install.packages("reshape2")
library(reshape2)
data_averagescore_grouped <- melt(data_summarize_grouped_by_gender)
ggplot(data_averagescore_grouped, aes(y = variable, x = value, shape = pronoun, color = pronoun))+geom_point(size = 7, alpha = 0.7)



#The pairwise comparison between all six tools with correlation coefficients 
install.packages("GGally")
require(GGally)
levelcf <- data.frame(sapply(data_clean[7:11], as.numeric))
levelcf
for (i in 1:ncol(levelcf)){
  levelcf[,i] = rnorm(nrow(levelcf), 0, 0.05)+levelcf[,i]
} 
levelcf <- cbind(levelcf, data_clean[5])
#discrete and hard to understand, by jittering add small noise 
ggpairs(data=levelcf, title="tools confident level comparison")
#color/transparent/size of points 



#Comparison plot of overall confidence level and number of tools a person being able to use efficiently
install.packages("Rmisc")
library(Rmisc)
#add new column of total number of tools each person using 
tooltotal <- rowSums(data_clean[12:45])
data_clean <- cbind(data_clean, tooltotal)
confidence <- data_clean$Rdata_manipulation_modeling
#level of confident vs. number of tools  
ggplot(data_clean, aes(x = tooltotal, y = confidence, col = pronoun))+geom_jitter() + geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 



#Pairwise comparison plots between confident level on each tool and overall level of confidence 
#number of tools vs. confident level on each tools 
p1<-ggplot(data_clean, aes(x = tooltotal, y = Github, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p2<-ggplot(data_clean, aes(x = tooltotal, y = Matlab, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p3<-ggplot(data_clean, aes(x = tooltotal, y = RMarkdown, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p4<-ggplot(data_clean, aes(x = tooltotal, y = Radvanced, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p5<-ggplot(data_clean, aes(x = tooltotal, y = Rgraphic_basics, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
multiplot(p1,p2,p3,p4,p5, cols =2)



#Pairwise comparison plots between confident level on each tool and total number of tools a person being able to use
#total confident level vs. confident level on each tools 
p6<-ggplot(data_clean, aes(y = confidence, x = Github, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p7<-ggplot(data_clean, aes(y = confidence, x = Matlab, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p8<-ggplot(data_clean, aes(y = confidence, x = RMarkdown, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
p9<-ggplot(data_clean, aes(y = confidence, x = Radvanced, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE) 
ggplot(data_clean, aes(y = confidence, x = Rgraphic_basics, color = pronoun))+geom_jitter()+geom_smooth(linetype = 1, aes(group =1), method = "lm", se = FALSE)+geom_line()
multiplot(p6,p7,p8,p9,p10, cols =2)

