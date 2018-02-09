setwd("D:/Append_Data/")


#Loading the required packages.
library(formattable)
library(data.table)
library(readr)
library(shiny)
library(shinydashboard)
library(quanteda, irlba)
library(ggplot2)
#library(e1071)
#library(lattice)
library(zoo)
library(lubridate)
#library(fiftystater)
library(forecast)
library(rvest)
library(tibble)
library(randomForest)
library(tseries)
library(tibble)
#library(maps)
#library(mapproj)
#library(tmap)
#library(maptools)
library(dplyr)
library(openxlsx)
#library(xml2)
library(sp)
library(plotly)
library(radarchart)
library(fmsb)
library(DT)
library(stringr)
library(caret)
#Reading the required csv files.
#demand <- data.frame(fread("demand.csv", stringsAsFactors = FALSE))

demand.dump <-data.frame(fread("dump2.csv", stringsAsFactors = FALSE))
#demand.dump <-data.frame(fread("dump2_full_data_dilip.csv", stringsAsFactors = FALSE))# testing

demand.upload <- demand.dump 
#demand.upload$V1 <- NULL
#quarter(dmy(demand.dump$Approval.Date[1110]),with_year = FALSE, fiscal_start = 4) by dilip
#demand.dump$quarter <- quarter(dmy(demand.dump$Approval.Date))
# demand.dump$quarter<- quarter(dmy(demand.dump$Approval.Date),with_year = FALSE, fiscal_start = 4)
# demand.dump$year <- year(dmy(demand.dump$Approval.Date))
# demand.dump$month <- month(dmy(demand.dump$Approval.Date))
# maxdate <- max(dmy(demand.dump$Approval.Date))
# maxdate_req <- max(dmy(demand.dump$Req.Date))





#datasetexp<-data.frame(fread("excel1.csv", stringsAsFactors = FALSE))
colors <- c('#4AC6B7', '#2457C5', '#DF0B0B',"#24C547", '#E71BB6')
#indiadistance<-data.frame( fread("indaiusa Distance1.csv"))
demandda<-demand.dump
#alternatives<-data.frame(fread("alternatives.csv"))
#rowman<-data.frame(fread("ronnames chan1.csv"))
dd<-data.frame(fread("consolidated_skills1.csv", stringsAsFactors = FALSE))
#cons <- data.frame(fread("Consolidated.csv", stringsAsFactors = F))

cons<-cbind(demand.upload,dd)
cons<-add_column(cons, X.1 = " ", .after ="V1" )
cons<-add_column(cons, X = 0, .after ="X.1" )
cons<-add_column(cons, IPython = 0, .after ="IPv6" )


               
ui <-dashboardPage(#skin = "blue",
  
  #Header for the App, The sidebar and the menu items.
  dashboardHeader(title = "Recruitment Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(tags$head(tags$style(HTML('.content{
                                          background-color: white;
                                          } 
                                          .skin-blue .main-header .navbar{
                                          background-color:#003da5}
                                          .skin-blue .main-header .logo{
                                          background-color:#003da5                                  
                                          }
                                          .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li:hover > a{
                                          border-left-color:#003da5                                        
                                          }
                                          h1{
                                          font-family:"Cambria"
                                          }'))),
  
                tabItem(tabName = "Pop",
                        tags$h1("DSM+ Data Upload"),
                        fluidRow(

                  # Input: Select a file ----
                  box(
                    title = "Upload new Demand Data",
                    status = "danger",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    #titlePanel("Appending New Demand"),
                    
                    fileInput("file1", "Choose CSV File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv"))
            
            
          ),
          
          # Main panel for displaying outputs ----
          mainPanel(
            
            # Output: Data file ----
            tableOutput("contents")
            
          )
          
        )
      )))

# Define server logic to read selected file ----
server <- function(input, output) {
 
  
  #Append_demand<-function(){
  output$contents <- renderTable({
    infile<-input$file1
    dframe <- read.csv(infile$datapath,stringsAsFactors = F)
    new.demand <-dframe
    #df$X <- NULL
    #demand <- rbind(demand.upload, df)
    print("uploaded")
    #setwd("D:\\HCL\\LikeMe")
    #write.csv(demand, "newdemand_pp.csv")
    
    
    #new.demand <- read.csv("FullData_dilip.csv", stringsAsFactors = F)
    new.demand<-add_column(new.demand, V1 = " ", .before ="ReqNo")
    #new.demand<-new.demand[,!(names(new.demand) %in% c("OnBehalfOf"))]# for full data append
    #colnames(new.demand)[1]<-"V1"
    seg <- read.csv("Segment.csv", stringsAsFactors = F)
    new.demand<-merge(x=new.demand,y=seg ,by =c("Customer"), all.x = TRUE)
    new.demand$Skill.Bucket <- ""
    new.demand$X<-NULL
    
    new.demand <- new.demand[!duplicated(new.demand),]
    
    new.demand$requirement <- paste(new.demand$Primary.Skill.Area,new.demand$vAdditionalRemarks)
    full.tokens <- tokens(new.demand$requirement, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
    full.tokens <- tokens_tolower(full.tokens)
    full.tokens <- tokens_select(full.tokens, stopwords(), selection = "remove")
    full.tokens <- tokens_wordstem(full.tokens, language = "english")
    full.tokens.dfm <- dfm(full.tokens, tolower = FALSE)
    full.tokens.matrix <- as.matrix(full.tokens.dfm)
    full.tokens.df <- data.frame(full.tokens.matrix)
    full.tokens.df$class.label <- new.demand$Skill.Bucket
    
    dd_skills <- data.frame(fread("list_train_updated.csv", stringsAsFactors = F))
    
    x<-tolower(dd_skills[,1])
    y<-colnames(full.tokens.df)
    z<-intersect(y,x)
    full.tokens.df1<-full.tokens.df[,z]
    d<-data.frame(t(dd_skills[,1]))
    colnames(d)<-tolower(dd_skills[,1])
    
    for(i in col(d))
    {
      d[,i]<-as.numeric(as.character(as.factor(d[,i])))
    }
    d[is.na(d)]<-0
    library(gtools)
    full.tokens.df2<-smartbind(d,full.tokens.df1)
    full.tokens.df2<-full.tokens.df2[-1,]
    full.tokens.df2[is.na(full.tokens.df2)]<-0
    
    
    test <- full.tokens.df2
    setnames(test, old = c("x.name","x1.5.10year","x10g","x10x","x12yr","x14nm","x15th","x178b","x1p","x2.way","x2008r2","x21cfr","x2d","x2exposur","x3.leg","x300work",
                           "x32.bit","x3d","x3p","x3yrs.6yr",
                           "x5p","x5th","x5yr","x64.bit","x802.11d"),
             new = c("X.name","X1.5.10year","X10g","X10x","X12yr","X14nm","X15th","X178b","X1p","X2.way","X2008r2","X21cfr","X2d","X2exposur","X3.leg","X300work",
                     "X32.bit","X3d","X3p","X3yrs.6yr",
                     "X5p","X5th","X5yr","X64.bit","X802.11d"))
    
    setnames(test, old=colnames(test[1856]),
             new=paste("X",substr(colnames(test[1856]),2,3),sep=""))
    colnames(test[1856])<-paste("X",substr(colnames(test[1856]),2,3),sep="")
    
    #test[1856]<-0
    load("skill_bucket_model.rda")
    rf.predict <- predict(rf.train, test)
    test$class.label <- rf.predict
    new.demand$Skill.Bucket<-test$class.label
    new.demand_load<-new.demand[,!(names(new.demand)%in% c("requirement"))]
    #new.demand_load$V1<-0
    #library(gtools)
    print(names(demand.upload))
    print(names(new.demand_load))
    dmp2<-rbind(demand.upload,new.demand_load)
    dmp2 <- dmp2[!duplicated(dmp2),]
    #file 1
    write_csv(dmp2[1:nrow(dmp2),], "dump2_created.csv")
    #write_csv(dmp2[31049:nrow(dmp2),], "dump2_created.csv")
    # #file 2
    cons1<-smartbind(cbind(demand.upload,dd),cbind(new.demand_load,test))
    for(i in 134:ncol(cons1))
    {
      cons1[,i]<-as.numeric(as.character(as.factor(cons1[,i])))
    }
    
    cons1[is.na(cons1)]<-0
    write.csv(cons1, "consolidated_created.csv")
    #file 3
    dd1<-smartbind(dd,test)
    for(i in 1:ncol(dd1))
    {
      dd1[,i]<-as.numeric(as.character(as.factor(dd1[,i])))
    }
    
    dd1[is.na(dd1)]<-0
    write.csv(dd1, "consolidated_skills_created.csv")
    new.demand$quarter<- quarter(dmy(new.demand$Approval.Date),with_year = FALSE, fiscal_start = 4)
    new.demand$year <- year(dmy(new.demand$Approval.Date))
    new.demand$month <- month(dmy(new.demand$Approval.Date))
    new.demand_load1<-new.demand[,!(names(new.demand)%in% c("requirement"))]
    new.demand_load1<-cbind(new.demand_load1,test)
    demand.dump<-cbind(demand.dump,dd)
    demand.dump<-smartbind(demand.dump,new.demand_load1)
    for(i in 138:ncol(demand.dump))
    {
      demand.dump[,i]<-as.numeric(as.character(as.factor(demand.dump[,i])))
    }
    demand.dump[is.na(demand.dump)]<-0
    demand.dump <- demand.dump[!duplicated(demand.dump),]
    maxdate <- max(dmy(demand.dump$Approval.Date))
    maxdate_req <- max(dmy(demand.dump$Req.Date))
    demand.dump<-cbind(demand.dump,test)
    # file1
    # file2
    #file3
    return(demand.dump)
})
  # 
  # demand.dump.1<-reactive({
  #   if(is.null(input$file1))
  #   {
  #     print("check upload")
  #     print(ncol(demand.dump))
  #     demand.dump} else {
  #       print("checking done")
  #       #demand.dump
  #       print(c("appended demand.dump",nrow(data.append()[,1:137]),ncol(data.append()[,1:137])))
  #       return(as.data.frame(data.append()[,1:137]))
  #       
  #     }
  # })
  
  
  # dd.1<-reactive({
  #   if(is.null(input$file1))
  #   {
  #     print("check upload")
  #     print(c("demand.dump",nrow(demand.dump),ncol(demand.dump)))
  #     print(c("dd",nrow(dd),ncol(dd)))
  #     print(ncol(demand.dump))
  #     dd} else {
  #       write_csv(as.data.frame(data.append()[,138:ncol(data.append())]),"new_consolidated_skills1.csv")
  #       print("checking done")
  #       print(c("new_consolidated_skills",nrow(data.append()[,138:ncol(data.append())]),ncol(data.append()[,138:ncol(data.append())])))
  #       return(as.data.frame(data.append()[,138:ncol(data.append())]))
  #       
  #     }
  # })
  # 
  # cons.1<-reactive({dta
  #   if(is.null(input$file1))
  #   {
  #     print("check upload")
  #     print(c("demand.dump",nrow(demand.dump),ncol(demand.dump)))
  #     print(c("cons",nrow(cons),ncol(cons)))
  #     cons} else {
  #       # data_cons<-data.append()[,names(data_append)%in% c("V1","quater","year","month")]
  #       write_csv(as.data.frame(data_cons),"new_consolidated.csv")
  #       print("checking done")
  #       print(c("data.append()",nrow(data.append()),ncol(data.append())))
  #       return(as.data.frame(data.append()))
  #     }
  # })
  


#data.append<-eventReactive(input$file1,{Append_demand()})
  

}
# Create Shiny app ----
shinyApp(ui, server)
