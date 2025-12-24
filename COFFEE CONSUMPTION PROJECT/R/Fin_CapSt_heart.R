install.packages("tidyverse")
suppressPackageStartupMessages(library(tidyverse))

heart_rw<- read.csv("C://SAIT//DATA406CAPSTONE//DATA//heart.csv")
heart_rw

glimpse(heart_rw)
summary(heart_rw)

#Checking for NAs Duplicate

colSums(is.na(heart_rw))
sum(duplicated(heart_rw))

heart_r8<-heart_rw %>%
  mutate(
    Sex = factor(Sex),
    ChestPainType = factor(ChestPainType),
    FastingBS = factor(FastingBS,levels = c(0,1), labels = c("No", "Yes")),
    RestingECG = factor(RestingECG),
    ExerciseAngina = factor(ExerciseAngina,levels = c("N", "Y"), labels=c("No", "Yes")),
    ST_Slope = factor(ST_Slope),
    HeartDisease = factor(HeartDisease, levels = c(0,1), labels = c("No", "Yes"))
  )
glimpse(heart_r8)

summary(select(heart_r8, Age, RestingBP, Cholesterol, MaxHR, Oldpeak))

#Replacing 0 or less values with NAs

heart_R8 <- heart_r8 %>%
  mutate(
    RestingBP   = if_else(RestingBP <= 0, NA_real_, RestingBP),
    Cholesterol = if_else(Cholesterol <= 0, NA_real_, Cholesterol),
    Oldpeak = if_else(Oldpeak < 0, NA_real_, Oldpeak)
  )

summary(select(heart_R8, RestingBP,Oldpeak, Cholesterol))

## Replacing missing Cholesterol values with median by sex
heart_R8<-heart_R8%>%
  group_by(Sex) %>%
  mutate(Cholesterol=ifelse(is.na(Cholesterol),
                            median(Cholesterol,na.rm=TRUE),
                            Cholesterol))
#Replacing NAs with median for Oldpeak and RestingBP
heart_R8 <- heart_R8 %>%
  mutate(
    RestingBP   = if_else(is.na(RestingBP),
                          median(RestingBP, na.rm = TRUE),
                          RestingBP),
    Oldpeak = if_else(is.na(Oldpeak),
                      median(Oldpeak, na.rm = TRUE),
                      Oldpeak)
  )
summary(select(heart_R8, RestingBP,Oldpeak, Cholesterol))


#Creating Age Group for summary & cross-tabs

heart_R8 <- heart_R8 %>%
  mutate(
    AgeGroup = cut(
      Age,
      breaks = c(0, 34, 49, 64, Inf),
      labels = c("18–34", "35–49", "50–64", "65+"),
      right  = TRUE))

table(heart_R8$AgeGroup, heart_R8$HeartDisease)

num_vars <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")

Heart_R8 <- heart_R8 %>%
  # Standardizing numeric variables to support modeling and comparison
  
  mutate(across(all_of(num_vars), scale, .names = "z_{.col}"))

glimpse(Heart_R8)


write.csv(Heart_R8,"Heart_R8_Cleaned.csv")

getwd()









