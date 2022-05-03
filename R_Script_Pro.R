install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("caret")
install.packages("glmnet")
install.packages("hexbin")
install.packages("leaps")



library("ggplot2")
library("dplyr")
library("stringr")
library("caret")
library("glmnet")
library("hexbin")
library("car")
library("leaps")

##stepward and backward regression 
##need to be driven by common sense 

##research question 
##How does years of experience of developers in the U.S. influence the amount of their salaries? 
dataset <- read.csv('Datasets/survey_results_public.csv')
summary(dataset)

#check datatypes
str(dataset)


#check how many null values exits in each of the columns. 
#there are some null values in the CompTotal. Rows with null in the variable should be dropped. 
#other columns also include null values. 
null_counts <- sapply(dataset, (function(x) sum(is.na(x))))
null_counts


#get respondents who are developers by prfession 
dataset <-subset(dataset, MainBranch=='I am a developer by profession' & Country=='United States of America')


#I will start cleaning this dataset first.


#Drop definitely unnecessary variables 
dataset <- subset(dataset, select=-c(SurveyLength,SurveyEase,UK_Country))

#change datatypes of columns into appropriate ones
dataset$ResponseId <- as.character(dataset$ResponseId)

#About YearsCode variable 
#I think it is okay to drop rows that contain less than 1 year and more than 50 years 
#they account about only 1.2% of all observations 
unique(dataset$YearsCode)
dataset <- dataset[!dataset$YearsCode %in% c("More than 50 years","Less than 1 year"),]
dataset$YearsCode <- as.numeric(dataset$YearsCode)


#About YearsCodePro
#drop less than 1 year 
unique(dataset$YearsCodePro)
#around 3% of resupondents have less than 1 year professional experience 
sum(dataset$YearsCodePro =="Less than 1 year",na.rm = TRUE) /length(dataset$YearsCodePro)
#drop rows 
dataset <- dataset[!dataset$YearsCodePro == "Less than 1 year",]
dataset$YearsCodePro <- as.numeric(dataset$YearsCodePro)


#threre are columns that list technologies that a respondent wants to work with
#Tools that they want to work with are not good indicator of salary amount 
#Simply because salary is not determined based on tools they want to work with. 
#On the other hand, it might be affected by # of tools that they currently work with. 
# This is because more tools people can use, more skilled they are typically. 

#drop such columns 
wanto_columns <- grepl(pattern = "WantToWorkWith", x=names(dataset))
dataset <- dataset[,!wanto_columns]

#Moreover, there are some columns about stack overflow. 
#the usage of the site is not likely to be ralated with salary amount 
#thus, drop them.
#also drop columns about communities 
dataset<- subset(dataset,select=-c(NEWSOSites,SOVisitFreq,SOAccount,SOPartFreq,SOComm))
dataset<- subset(dataset,select=-c(NEWOtherComms))
dataset<- subset(dataset, select=-c(NEWStuck))

#drop Opsys column 
#kinds of operating system used do not significantly affect the salary 
dataset <- subset(dataset, select=-c(OpSys))


#Convert worked with columns into number of tools that they can use. 
#i do not think that being able to use certain tools significantly increase the salary. 
#However, the number of tools people can use can be related to the compensation because 
#I guess that more tools people can use, more experienced they tend to be. 
#define a function to calculate the number of elements in the list 

##dumy variables, 
calculate_elements <- function(elements)
{
    if(any(is.na(elements))){
      return (0)
    }
    else{
      return (length(elements))
    }
    
}

#apply transformation on all of the columns 

columns <- names(dataset)[grep(pattern = "HaveWorkedWith",x=names(dataset))]


for(col_name in columns)
{
  dataset[,col_name] <- rapply(strsplit(dataset[,col_name],";"),calculate_elements)

}


#drop rows that contain at least one NA value. 
dataset <- na.omit(dataset)


#drop the original learn code column 
dataset <- subset(dataset, select=-c(LearnCode))

#let's apply same thing to devtypes as well 
#first get all dev types availabel
all_types =c()
for (types in dataset$DevType){
  for(type in strsplit(types,";")){
    for(content in type){
      if(!(as.character(content) %in% all_types)){
        all_types <-append(all_types,as.character(content))
    }
    }
  }
}

#create an empty dataframe 
dev_types <- setNames(data.frame(matrix(0,ncol = length(all_types), nrow = nrow(dataset))), all_types)
dataset<- cbind(dataset,dev_types)
#for learn code columns
for(id in dataset$ResponseId){
  job_desc <- dataset[dataset$ResponseId==id,"DevType"]
  description <- strsplit(job_desc,";")
  for(outer_list in description)
  {
    for(inner_list in outer_list)
    {
      if (inner_list == "Other (please specify):")
      {
        dataset[dataset$ResponseId==id,"Other (please specify):"] <- 1
      } else{
        dataset[dataset$ResponseId==id,inner_list] <- 1
      }
    }
  }
}


colnames(dataset) <- gsub(" ","_",colnames(dataset))
colnames(dataset) <- gsub(",","",colnames(dataset))
colnames(dataset) <- gsub("-","_",colnames(dataset))
colnames(dataset) <- gsub("/","",colnames(dataset))
dataset$Other_online_resources <- dataset$`Other_online_resources_(ex:_videos_blogs_etc)`
dataset$Other_devtypes <- dataset$`Other_(please_specify):`
dataset$Senior_excecutives <- dataset$`Senior_Executive_(C_Suite_VP_etc.)`

dataset <- subset(dataset, select=-c(`Other_online_resources_(ex:_videos_blogs_etc)`,`Other_(please_specify):`,`Senior_Executive_(C_Suite_VP_etc.)`))


#drop the original devtype column 
dataset <- subset(dataset, select=-c(DevType))

#there is a sexuality column 
#I do not think that what sexual preference siginificantly affects the amount of salary 
# thus, I will drop the column. 
#this needs attention(transgender)
dataset <- subset(dataset,select =-c(Sexuality))


#look at gender variable 
#it looks like that some people do not reveal their biological gender entirely
#sine it is likely that there could be a gap between men and women
#this column should be preserved. 
#there are some options one of which I can take. 
#keep only women and men
#keep everything as dummy variables 

#let's see the distribution of the unique values. Especially, I would like to see 
#how much of respondents do not entirely show their biological gender

#it looks like that about 90% of respondents are men. 
#the second biggest percentage is women. 
#thus, in this data set, the proportion of transgender is pretty small 
#Let's keep only men and women for this dataset. 
dataset %>%group_by(Gender) %>%summarise(cnt = n())%>%mutate(percentage=(cnt/sum(cnt))*100)


dataset<- dataset[(dataset$Gender=="Man") | (dataset$Gender=="Woman"),]
dataset


unique(dataset$Ethnicity)

#this Ethnicity categorical column is also very imbalanced. Mosto of values are white. Proportions of other unique values are spread out 
#the problem is that the distinction is too detailed. Moreover, some of respondents have more than one Ethnicity even though there is a multiracial selection......
#Since I don't know how to deal with multiple entries of ethnicity for one respondent and most of respondents have one ethnicity type, 
# i will pick only rows that contain one value for this variable. 
#white or non-white if i do not want to reduce sample 
unique(dataset$Ethnicity)
dataset %>%group_by(Ethnicity) %>%summarise(cnt = n())%>%mutate(percentage=(cnt/sum(cnt))*100)%>%arrange(desc(percentage))%>%head(20)
dataset %>%group_by(Ethnicity) %>%summarise(cnt = n())%>%mutate(percentage=(cnt/sum(cnt))*100)%>%arrange(desc(percentage))%>%tail(20)
dataset <- dataset[(rapply(strsplit(dataset$Ethnicity,";"),length)==1),]
unique(dataset$Ethnicity)



##about acceability 
##this variable is too detailed as well. 
## Moreover, since they have jobs, their disability is not so severe that they cannot work as developers. 
## However, the disability might affect efficiency of their work..... 
## If I make dummy variable for each of disability, it will be too detailed and sd will be very wide. 
# so, let's make this column into binary that indicates whether people have disability or not. 
unique(dataset$Accessibility)
dataset %>%group_by(Accessibility) %>%summarise(cnt = n())%>%mutate(percentage=(cnt/sum(cnt))*100)

dataset[dataset$Accessibility=="None of the above","Accessibility"] <- 0
dataset[dataset$Accessibility!=0,"Accessibility"] <- 1



#about mentalhealth
#Let's apply same logic to this variable as one used for accesability
unique(dataset$MentalHealth)
dataset %>%group_by(MentalHealth) %>%summarise(cnt = n())%>%mutate(percentage=(cnt/sum(cnt))*100)%>%arrange(desc(percentage))%>%head(20)
dataset[dataset$MentalHealth=="None of the above","MentalHealth"] <- 0
dataset[dataset$MentalHealth!=0,"MentalHealth"] <- 1



#let's look at currency
#a very few respondents get paid in non-us dollars....
#Since I selected only developers in the united states, this does not sound intuitive. 
#Given that the number is a very few and the strangeness, I guess they are wrong entries. 
#let's drop the rows that contain non-us currency 
unique(dataset$Currency)
dataset %>%group_by(Currency) %>%summarise(cnt = n())%>%mutate(percentage=(cnt/sum(cnt))*100)%>%arrange(desc(percentage))
dataset<- dataset[dataset$Currency == "USD\tUnited States dollar",]


unique(dataset$Trans)
dataset <- dataset[(dataset$Trans=="No"|dataset$Trans=="Yes"),]




#dependent variable 
#will use ConvertedCompYearly 
#so, let's drop currency, comptotal, and frequency 
dataset <- subset(dataset,select=-c(Currency, CompTotal, CompFreq))

#we can also drop MainBracnch and Country
dataset <- subset(dataset,select=-c(MainBranch,Country))

dataset <- subset(dataset,select=-c(ResponseId))



#Let's do some feature selection......
#selections of features will be done using two methods 

#let's study the dependent variable first.
#looks like it is skewed a lot 
# we might want to drop some outliers to fit a regression model. 
#let's deal with this before we train the model
summary(dataset$ConvertedCompYearly)

#see the relationship between the dependent variable and the years of professional experience 
#for higher compensation, there might be a linear relationship. 
#let's dig it deeper/ 
ggplot(dataset)+
  geom_point(aes(x=YearsCodePro,y=ConvertedCompYearly))


high_comp <-dataset[dataset$ConvertedCompYearly>=2.5e+06,]
low_comp <-dataset[dataset$ConvertedCompYearly<2.5e+06,]

#let's see plots and correlatino coefficient 
ggplot(high_comp)+
  geom_point(aes(x=YearsCode,y=ConvertedCompYearly))
cor(high_comp$YearsCode,high_comp$ConvertedCompYearly)

#
ggplot(low_comp)+
  geom_point(aes(x=YearsCode,y=ConvertedCompYearly))
cor(low_comp$YearsCode,low_comp$ConvertedCompYearly)

#Let's see the majority of income 
#I guess there is some non linear relationship.... like log curve?
majority_part <-dataset[dataset$ConvertedCompYearly<500000,]
ggplot(majority_part,aes(x=YearsCodePro,y=ConvertedCompYearly))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(majority_part)+
  geom_point(aes(x=YearsCode,y=ConvertedCompYearly))

#apply log transformation 
ggplot(majority_part)+
  geom_point(aes(x=log(YearsCodePro),y=log(ConvertedCompYearly)))

#apply sqrt transformation
ggplot(majority_part)+
  geom_point(aes(x=sqrt(YearsCodePro),y=sqrt(ConvertedCompYearly)))


cor(log(majority_part$YearsCodePro),log(majority_part$ConvertedCompYearly))
cor(sqrt(majority_part$YearsCodePro),sqrt(majority_part$ConvertedCompYearly))


#Given these plots, I think that it would be better to focus on the majority of data point because there 
#is likely a patter in the data.
# we need to determine a threshold 
#let's plot a box plot 
ggplot(dataset,aes(x=factor(0),y=ConvertedCompYearly))+
  geom_boxplot()+
  ylim(0,500000)

summary(dataset$ConvertedCompYearly)
#okay let's get values within the whiskers. 
#meaning all values below (75th percentile - 25th percentile)*1.5 + 75th percentile 
upper_limit <-(173000-95000)*1.5 + 173000
lower_limit <-(173000-95000)*1.5- 95000

#plot the scatter plots for YearsCodePro and YearsCode
#and fit a regression model
# looks like there is a non linear relationship. 
# this need to be considered in creating a model. 
dataset <- dataset[dataset$ConvertedCompYearly<=upper_limit & dataset$ConvertedCompYearly>=lower_limit ,]

ggplot(dataset,aes(x=YearsCode,y=ConvertedCompYearly))+
  geom_point()+
  geom_smooth(method="lm")



ggplot(dataset,aes(x=log(YearsCodePro),y=ConvertedCompYearly))+
  geom_point()+
  geom_smooth()


ggplot(dataset,aes(x=log(YearsCode),y=log(ConvertedCompYearly)))+
  geom_point()+
  geom_smooth()


#btw,now mean and median of the yearly compensation are not very different. 
summary(dataset$ConvertedCompYearly)

# let's plot data for each vairable to see a relationship between it with the dependent first 
# When comparing median yearly income, there is a stark difference between part time and full time 
ggplot(data = dataset, mapping = aes(x = Employment, y =ConvertedCompYearly))+
  geom_boxplot(horizontal=TRUE)+coord_flip()

#by states 
#Several states have very high median compensation 
#The lowest state is Puerto Rico....It is still a part of the U.S., but the economic situation is should be totally different from the U.S.
#I might need to drop it....
ggplot(data = dataset, mapping = aes(x = US_State, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()


#education level 
#associate degree and something else have less yearly compensation. 
#then till professional degree, there is not a difference. 
#then, the compensation goes up as the level of defree goes up. 
#I don't know if the difference obseved here is significant or not...
ggplot(data = dataset, mapping = aes(x = EdLevel, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()


#age 1st code
#It doesn't not seem to be true that the earlier you start coding, the higher the compensation is.... 
#it is kinda skeptic that people who start coding at 45-54 years actually have the highest median income
ggplot(data = dataset, mapping = aes(x = Age1stCode, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()


dat


ggplot(data=dataset, mapping=aes(x=reorder(US_State,mean(ConvertedCompYearly)),y=ConvertedCompYearly))+
  geom_bar(stat="identity")+coord_flip()


#organization size 
#there are some differences across various organization sizes. Maybe significant different. 
ggplot(data = dataset, mapping = aes(x = OrgSize, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()

#age
#okay...two things to mention here. 
#first, there are some professional developers under 18 years old. I don't feel this is correct...At least there are some outliers ithink. 
#let's drop them from our consideration. 
ggplot(data = dataset, mapping = aes(x = Age, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()
#4 persons are under 18 
sum(dataset$Age=="Under 18 years old")
#drop them 
dataset <- dataset[dataset$Age!="Under 18 years old",]

#gender 
#there is a difference, but not very significant difference
ggplot(data = dataset,aes(x = Gender, y=ConvertedCompYearly))+
  geom_boxplot()+coord_flip()
#trans 
#not a very significant difference beween them 
ggplot(data = dataset, mapping = aes(x =Trans, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()

#Ethinicity 
#not big differences 
ggplot(data = dataset, mapping = aes(x = Ethnicity, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()

#almost same. 
#we probably do not need this variable in our model
ggplot(data = dataset, mapping = aes(x = Accessibility, y =ConvertedCompYearly))+
  geom_boxplot()+coord_flip()

#mental health 
#a bit different. we might not need this variable as well. 
ggplot(data = dataset,aes(x=MentalHealth,y=ConvertedCompYearly))+
  geom_boxplot()+coord_flip()


par(mfrow=c(2, 2))
#man variabels come back as significant. 
linear_reg <- lm(ConvertedCompYearly~., data= dataset)
summary(linear_reg)
plot(linear_reg)
#since this selection method could drop dummy variables of a categorical column, I will use this selection method 
# for only numerical columns. 
#dropping more than two dummy variables will mess up the interpretation of the dummy variables. 
dataset_log <- data.frame(dataset)
dataset_sqrt <- data.frame(dataset)

dataset_log$log_yearscode<-log(dataset$YearsCode)
dataset_log$log_yearscodepro <-log(dataset$YearsCodePro)
dataset_sqrt$sqrt_yearscode <- sqrt(dataset$YearsCode)
dataset_sqrt$sqrt_yearscodepro <-sqrt(dataset$YearsCodePro)


#feature selection on numerical columns 
numerical_data <-select_if(dataset, is.numeric)
regfit.bwd <- regsubsets(ConvertedCompYearly~. ,nvmax=45,data=numerical_data,method="backward")
regfit_bwd_summary <- summary(regfit.bwd)
which.max(regfit_bwd_summary$adjr2)
#let's use this numerical columns for our regression model
names(coef(regfit.bwd,30))[-1]


#so we are gonna use the numerical columns found in the feature selectino and categorical columns. 
numerical_selected<-names(coef(regfit.bwd,30))[-1]
categorical <-names(select_if(dataset,is.character))
variables <- c(numerical_selected,categorical)
regression_df <- subset(dataset,select=variables)
regression_df$ConvertedCompYearly <- dataset$ConvertedCompYearly

#create a model 
model <- lm(ConvertedCompYearly~.,data=regression_df)
summary(model)
plot(model)

#drop unsignificant categorical variables
regression_df <-subset(regression_df,select=-c(Age1stCode,Trans,Ethnicity,Accessibility))


model_2nd <- lm(ConvertedCompYearly~.,data=regression_df)
summary(model_2nd)
plot(model_2nd)


#when looking at this plot, probably there are no outliers. 
#what about leverages 
hv<-as.data.frame(hatvalues(model_2nd))
mn<-mean(hatvalues(model_2nd))

hv$warn <- ifelse(hv[, 'hatvalues(model_2nd)']>3*mn, 'x3',
                  ifelse(hv[, 'hatvalues(model_2nd)']>2*mn, 'x3', '-' ))
hv_high <- subset(hv, warn%in%c("x2", "x3"))


regression_df <-regression_df[!rownames(regression_df) %in% rownames(hv_high),]
  
model_3rd <-lm(ConvertedCompYearly~.,data=regression_df)
summary(model_3rd)
plot(model_3rd)


#apply transformation
no_leverage_df_log <- data.frame(regression_df)
no_leverage_df_log$log_dependent <- log(no_leverage_df_log$ConvertedCompYearly)
no_leverage_df_log<-subset(no_leverage_df_log,select=-c(ConvertedCompYearly))


#log transformed regression 
#still hetero......
model_log_onDependent <- lm(log_dependent~., data=no_leverage_df_log)
summary(model_log_onDependent)
plot(model_log_onDependent,id.n=8)
#deal with outliers 
outliers <- c(24286,72257,36239,35279,40452,51283,57057,10152,47199,8438,31183,41394)
#let's drop some binary columns because they have zero 1 entry 
devtypes <- no_leverage_df_log_noOutliers[,colnames(no_leverage_df_log_noOutliers)[12:27]]
lapply(devtypes,FUN = sum)
no_leverage_df_log_noOutliers <- subset(no_leverage_df_log_noOutliers, select=-c(Academic_researcher,Product_manager,Student, Marketing_or_sales_professional, Senior_excecutives))
no_leverage_df_log_noOutliers <- no_leverage_df_log[!rownames(no_leverage_df_log) %in% outliers,]
model_log_onDependent_noOut <- lm(log_dependent~.,data=no_leverage_df_log_noOutliers)
summary
#add log transformation on the YearscodePro as well
no_leverage_df_log_noOutliers$YearsCodePro_log <- log(no_leverage_df_log_noOutliers$YearsCodePro)
both_log <- lm(log_dependent~.-YearsCodePro, data=no_leverage_df_log_noOutliers)
summary(both_log)
plot(both_log)

