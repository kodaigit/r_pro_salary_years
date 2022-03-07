

dataset <- read.csv('Datasets/survey_results_public.csv')
summary(dataset)
#get respondents who are developers by prfession 
dataset <-subset(dataset, MainBranch=='I am a developer by profession' & Country=='United States of America')


?sapply
