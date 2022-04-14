library('shiny')
library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library("DT")
library('dplyr')
library('markdown')
library('zoo')
library('tidyr')
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)


###Cleaning data###
#Reading the dataset
survey <- read.csv("survey.csv")
head(survey)

str(survey)

#summary of data
glimpse(survey)
#1259 rows and 27 columns

# check for missing values
sapply(survey, function(x) sum(is.na(x)))

#missing values are present in state, work_interfere, self_employed and comments

#looking at state the NA value are considered as missing.
survey$state <- survey$state %>% replace_na("NA")

# check for missing values
sapply(survey, function(x) sum(is.na(x)))

#dropping the commments column
survey$comments <- NULL

#getting summary for work_interface and self_employed
summary(survey)

survey$work_interfere <- survey$work_interfere %>% replace_na("NA")
survey$self_employed <- survey$self_employed %>% replace_na("NA")

# check for missing values
sapply(survey, function(x) sum(is.na(x)))

#checking accuracy for data
str(survey)

#accuracy check for categorical variables
#unique value count for no_employees and gender

table(survey['no_employees'])

table(survey['Gender'])

#grouping the gender column
survey$Gender <- as.character(survey$Gender)
survey$Gender <- tolower(survey$Gender)

Female <- c('female', 'cis female', 'f', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail')

Male <- c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 
          'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man','Mle')

Others <- c('others','a little about you','agender','androgyne','queer/she/they', 'non-binary', 'nah', 'enby', 'fluid', 'genderqueer',  
            'guy (-ish) ^_^', 'male leaning androgynous', 'neuter', 'queer','p','all','trans-female', 'trans woman',
            'female (trans)','something kinda male?','ostensibly male, unsure what that really means')


Gender_new <- as.vector(survey$Gender)  

Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Male) "Male" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Female) "Female" else x)
Gender_new <- sapply(as.vector(Gender_new), function(x) if(x %in% Others) "Others" else x)

survey$Gender<- Gender_new


#unique value count for gender
table(survey['Gender'])

#accuracy check for numerical variables
#summary of survey
summary(survey)

#histogram
survey %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#there are outliers in age some values are less than 0 and greater than 100, so we will drop this values.
survey<-survey[!(survey$Age<0 | survey$Age >100),]


#histogram
survey %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#cleaned data
head(survey)
str(survey)

#check for null values
sum(duplicated(survey))



###UI###

shinyUI(dashboardPage(skin="yellow",
                      #Application title
                      dashboardHeader(title = "Mental Health Survey",titleWidth = 600),
                      # dashboard sidebar functions will be inserted here
                      dashboardSidebar(
                        
                        sidebarMenu(
                          menuItem('Map and Overview',tabName='map'),
                          menuItem("For Employee",tabName = "employee"),
                          menuItem("For Employeer",tabName = "employeer")
                          
                        ),
                        sliderInput("age",
                                    label = "Age range",
                                    min =  15,
                                    max = 80,
                                    step = 2,
                                    value = c(15,80),
                                    sep = ""),
                        uiOutput("typeSelectOutput"),
                        radioButtons("gender",
                                     label = "Select Gender:",
                                     choices = c('Female' = 'female', 'Male'='male','Others'= 'others'),
                                     selected = "female"),
                        radioButtons("companysize",
                                     label = "Select Company Size:",
                                     choices = c("1-25" = "6-25",
                                                 "26-100" = "26-100",
                                                 "500-1000" = "500-1000",
                                                 ">1000" = "More than 1000"),
                                     selected = "26-100"),
                        radioButtons("company",
                                     label = "Type of company",
                                     choices =  c("Tech" = "Yes",
                                                  "Other" = "No"
                                     ),
                                     selected = "Yes")
                        
                      ),
                      # functions that must go in the body of the dashboard.
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "employee",
                                  h3("Employee benefit"),
                                  plotOutput("thePlot"),
                                  br(),
                                  h3("how company size and work type affect the mental health?"),
                                  plotOutput("companysize"),
                                  
                          ),
                          tabItem(tabName = "map",
                                  h3("Distribution of participants"),
                                  plotlyOutput("plotworld"),
                                  h3("Number of participants country wise"),
                                  plotOutput('country')
                          ),
                          tabItem(tabName = "employeer",
                                  h3("How many employees are ready to seek help?"),
                                  plotOutput("seekhelp"),
                                  h3("Would you bring up a mental health issue with a potential employer in an interview?"),
                                  plotOutput('mentalhealth')
                          )
                          
                        )
                      )
))





###Server###
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #select state
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput","Select country:",
                sort(unique(survey$Country)),
                multiple = TRUE,
                selected = c("United States"))
    
  })
  
  # function to know about "Does you employeer provide you mental health benefits?"
  output$thePlot <- renderPlot({
    survey$no_employees <- as.factor(survey$no_employees)
    if(input$gender == "female"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        ggplot(aes(x = no_employees, fill = benefits)) + geom_bar(position="fill") +
        xlab("No of employees") + 
        ylab(paste0("benefit proportion for", input$gender)) + ggtitle(paste0("Does you employeer provide you mental health benefits to",input$gender))
        
    }
    else if(input$gender == "male"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male") %>%
        ggplot(aes(x = no_employees, fill = benefits)) + geom_bar(position="fill") +
        xlab("No of employees") + 
        ylab(paste0("benefit proportion for", input$gender)) + ggtitle(paste0("Does you employeer provide you mental health benefits to",input$gender))
      
      
    }
    else{
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others") %>%
        ggplot(aes(x = no_employees, fill = benefits)) + geom_bar(position="fill")  +
        xlab("No of employees") + 
        ylab(paste0("benefit proportion for", input$gender)) + ggtitle(paste0("Does you employeer provide you mental health benefits to",input$gender))
      
      
    }
    plot
    
  })    
  
  
  # function to know about "how company size and work type affect the mental health?" 
  output$companysize <- renderPlot({

    if(input$gender == "female"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        ggplot(aes(x=no_employees,y=treatment, fill=factor(tech_company)), color=factor(vs)) +  
        stat_summary(fun=mean,position=position_dodge(),geom="bar")+
        xlab("No of employees") + 
        ylab(paste0("Treatment proportion for", input$gender)) 
    }
    else if(input$gender == "male"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2])%>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male") %>%
        ggplot(aes(x=no_employees,y=treatment, fill=factor(tech_company)), color=factor(vs)) +  
        stat_summary(fun=mean,position=position_dodge(),geom="bar")+
        xlab("No of employees") + 
        ylab(paste0("Treatment proportion for", input$gender)) 
      
    }
    else{
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others") %>%
        ggplot(aes(x=no_employees,y=treatment, fill=factor(tech_company)), color=factor(vs)) +  
        stat_summary(fun=mean,position=position_dodge(),geom="bar")+
        xlab("No of employees") + 
        ylab(paste0("Treatment proportion for", input$gender)) 
      
    }
    plot
    
  })    
  
  # function to know about "how many people are ready to seek help"
  output$seekhelp <- renderPlot({
    survey$seek_help <- as.factor(survey$seek_help)
    if(input$gender == "female"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        filter(no_employees == input$companysize) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge") +
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to seek help")) 
    }
    else if(input$gender == "male"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2])%>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male") %>%
        filter(no_employees == input$companysize) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to seek help")) 
      
    }
    else{
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others") %>%
        filter(no_employees == input$companysize) %>%
        ggplot(aes(x = no_employees, fill = seek_help)) + geom_bar(position = "dodge")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to seek help")) 
      
    }
    plot
    
  })    
  
  # function to know about "Would you bring up a mental health issue with a potential employer in an interview?"
  output$mentalhealth <- renderPlot({
    if(input$gender == "female"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Female") %>%
        filter(no_employees == input$companysize) %>%
        filter(tech_company ==input$company) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "darkgreen")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to disscuss the mental health concern")) 
    }
    else if(input$gender == "male"){
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Male") %>%
        filter(no_employees == input$companysize) %>%
        filter(tech_company ==input$company) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "darkgreen")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to disscuss the mental health concern")) 
      
    }
    else{
      plot <- survey %>% 
        filter(Age>input$age[1], Age<input$age[2]) %>%
        filter(Country %in% input$typeInput) %>%
        filter(Gender == "Others") %>%
        filter(no_employees == input$companysize) %>%
        filter(tech_company ==input$company) %>%
        ggplot(aes(x=mental_health_consequence)) + geom_bar(fill = "darkgreen")+
        xlab("No of employees") + 
        ylab(paste0("proportion of ", input$gender, " ready to disscuss the mental health concern")) 
      
    }
    plot
    
  })    
  
  output$plotworld <- renderPlotly({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world_modified <- world %>% 
      mutate(my_selection = ifelse(admin %in% survey$Country,1, NA))
   plot <- ggplot(data = world_modified) +
      geom_sf(aes(fill=my_selection)) +
      theme_bw()
   plot
  })
  
  
  output$country <- renderPlot({
    plot <- survey %>% 
      filter(Age>input$age[1], Age<input$age[2]) %>%
      filter(Country %in% input$typeInput) %>%
      ggplot(aes(x = Country)) + geom_bar(fill = "blue")
    plot
  })
})

