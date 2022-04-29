install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")

library("ggplot2")
library("dplyr")
library("stringr")

##research question 
##How does years of experience of developers in the U.S. influence the amount of their salaries? 



dataset <- read.csv('Datasets/survey_results_public.csv')
summary(dataset)

#check datatypes
#probably need to change data types of response ID, years of code, years code pro. 
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


#create binary columns for Learn to code 
#1 indicate whether a person learned coding in that way  
columns_code <- c("Books / Physical media","Coding Bootcamp","Friend or family member","Colleague","Online Courses or Certification",
                  "Online Forum","Other online resources (ex: videos, blogs, etc)","School","Other")


# Create empty data.frame with matrix & setNames functions
learn_codes <- setNames(data.frame(matrix(0,ncol = 9, nrow = nrow(dataset))), columns_code)
dataset<- cbind(dataset,learn_codes)


#for learn code columns
for(id in dataset$ResponseId){
  learning_ways <- dataset[dataset$ResponseId==id,"LearnCode"]
  ways <- strsplit(learning_ways,";")
  for(outer_list in ways)
  {
    for(inner_list in outer_list)
    {
    if (!inner_list %in% columns_code[1:8])
      {
        dataset[dataset$ResponseId==id,"Other"] <- 1
      } else{
        dataset[dataset$ResponseId==id,inner_list] <- 1
      }
    }
  }
}

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

#drop the original devtype column 
dataset <- subset(dataset, select=-c(DevType))

#there is a sexuality column 
#I do not think that what sexual preference siginificantly affects the amount of salary 
# thus, I will drop the column. 
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



#dependent variable 
#will use ConvertedCompYearly 
#so, let's drop currency, comptotal, and frequency 
dataset <- subset(dataset,select=-c(Currency, CompTotal, CompFreq))

#we can also drop MainBracnch and Country
dataset <- subset(dataset,select=-c(MainBranch,Country))

dataset <- subset(dataset,select=-c(ResponseId))



#Let's do some feature selection......
#selections of features will be done using two methods 
#common sense 
#statistical method, such as Ridge Regression 



ggplot(dataset)+
  geom_point(aes(x=,y=US_State))+
  ylim(0,10000000)


str(dataset)







