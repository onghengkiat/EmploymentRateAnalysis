library(shiny)
library(ggplot2)
library(xlsx)
library(rsconnect)
library(shinyjs)

#set up the account info
rsconnect::setAccountInfo(name='onghengkiat',
                          token='AA5C3516E0990B59FB0F66B865EA1958',
                          secret='+eQXsKOs/8qelgATciTXu/U18dAfVCS/uQicOPSo')

#reads the data
dataset <- read.xlsx("data/cleanedData.xlsx",sheetIndex=1,header=TRUE)

#simply choose a seed so that the performance of the model would be more stable
set.seed(10)  

#split the train and test set with 80% and 20% proportion
split_index <- sample(2,nrow(dataset),replace = T , prob = c(0.8,0.2))
train_set <- dataset[split_index == 1,]
test_set <- dataset[split_index == 2,]
  
#build the regression function to predict output with certain variables that are most significant 
regression_function <- Employed ~ Disabled + Internet.Access+ Receive.Food.Stamps+
  Anxiety + Age + Household.Income  

#build the model using the function and train set
#binomial is used as family because it is classification problem
logistic_model <- glm(regression_function , data = train_set, family ='binomial')

#get the probability to predict the train data
train_predict <- predict(logistic_model, train_set, type = 'response')

#If probability higher than 0.5 than it is 1 which means employed, and vice versa
train_predict <- ifelse(train_predict >=0.5 , TRUE , FALSE)

#same goes to test set data
#the existence of the test data is to determine whether the model is overfitting, underfitting or just okay.
test_predict <- predict(logistic_model, test_set, type = 'response')
test_predict <- ifelse(test_predict >=0.5 , TRUE , FALSE)
paste(format(round(sum(test_predict == test_set$Employed)/nrow(test_set)*100),nsmall=2),"%")
paste(format(round(sum(train_predict == train_set$Employed)/nrow(train_set)*100),nsmall=2),"%")

ui <- fluidPage(
  #set up the shiny js
  useShinyjs(),
  titlePanel("Data Explorer For Employment Rate"),
  
  sidebarPanel(
    sliderInput(inputId='sampleSize', label='Choose the first n samples', min=1, max=nrow(dataset),
                value=nrow(dataset), step=1),
    selectInput(inputId='xaxis', label='Choose the X-axis', choices=names(dataset), selected=names(dataset)[[1]]),
    selectInput(inputId='yaxis', label='Choose the Y-axis', choices=names(dataset), selected=names(dataset)[[2]]),
    
    #put none as "." but still shown as none in the selection for syntax use in building facet grid later
    selectInput(inputId='facet_row', label='Choose the Facet Row', choices=c(None='.', names(dataset))), 
    selectInput(inputId='facet_col', label='Choose the Facet Column', choices=c(None='.', names(dataset))),
    
    selectInput(inputId='legend', label='Legend', choices=c('None', names(dataset))),
    checkboxInput(inputId = 'selectAll',label = 'Show All Variables', value = FALSE),
    
    checkboxGroupInput(inputId = 'modelCheckbox',label='Select the one which is/are TRUE',
      choiceNames=c('Legally Disable','Internet Access','Received Food Stamps','Anxiety'),
      choiceValues= c('Legally Disable','Internet Access','Received Food Stamps','Anxiety')
    ),
    selectInput(inputId ='age', label ='Age', choices = levels(dataset$Age)),
    selectInput(inputId = 'income', label = 'Household Income', choices = levels(dataset$Household.Income))
  ),
  
  mainPanel(
    
    tabsetPanel( id = 'tabsetPanel', selected = 'Graph',
                 
      tabPanel(h4("The graph plotted below is the jitter graph because the dataset consists of mostly discrete data"),
               title = "Graph", plotOutput("graph"),
               helpText("Note: If possible, please avoid from plotting both axis as continuous variable, it would not be your desired output")),
      
      tabPanel(title = "Table", tableOutput("table")),
      
      tabPanel(title = "Bar", plotOutput("bar"),
               helpText("Note: variable of y-axis will be automatically chosen to build the legends")),
      
      tabPanel(title = "Prediction Model", h3("Accuracy of the Model"),
               h4("The accuracy for the train set(%):"), verbatimTextOutput("trainAccuracy"),tableOutput("trainConfusion"),
               h4("The accuracy for the test set(%):"),verbatimTextOutput("testAccuracy"),tableOutput("testConfusion"),
               h4("The predicted output is shown below:"),verbatimTextOutput("predictOutput"))
    )
  )
)

server <- function(input, output) {
  
  #extract certain portion of the data from whole data sets
  data_shown <- reactive({
    dataset[1:input$sampleSize,]
  })
  
  #get the tabselected so that we can specifically show and hide certain input box in the sidebar panel
  tabselected <- reactive ({
    input$tabsetPanel
  })
  
  #get the facet row
  facet_row <- reactive({
    
    #it cannot be same with facet column, if it is, an error message will be shown
    validate(
      need((input$facet_row != input$facet_col ) || input$facet_row == '.', "Please choose value different with facet column for facet row")
    )
    input$facet_row
  })
  
  #get the facet column
  facet_column <- reactive({
    #it cannot be same with facet row, if it is, an error message will be shown
    validate(
      need((input$facet_col != input$facet_row) || input$facet_col == '.', "Please choose value different with facet row for facet column")
    )
    input$facet_col
  })
  
  output$graph <- renderPlot({
    
    a#show and hide some control widgets
    if(tabselected() == 'Graph'){
      show('sampleSize')
      show('xaxis')
      show('yaxis')
      show('facet_row')
      show('facet_col')
      hide('selectAll')
      show('legend')
      hide('modelCheckbox')
      hide('age')
      hide('income')
      hide('rebuild')
      hide('submit')
    }
    
    #plot the graph with jitter plot because most of the varaibles are nominal data
    #Thus jitter plot is the one that most suit with it
    #width and height is the range of the points scatter around
    p <- ggplot(data_shown(), aes_string(x=input$xaxis, y=input$yaxis)) +
      geom_jitter(width = 0.25, height = 0.25)
   
    if (input$legend != 'None')
      p <- p + aes_string(color=input$legend) +
        scale_fill_discrete(name = "Legends")
    
    facets <- paste(facet_row(), '~', facet_column())
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    #show the graph
    p
    
  })
  
  output$table <- renderTable({
    if(tabselected() == 'Table'){
      show('sampleSize')
      show('xaxis')
      show('yaxis')
      show('facet_row')
      show('facet_col')
      hide('legend')
      show('selectAll')
      hide('modelCheckbox')
      hide('age')
      hide('income')
      hide('rebuild')
      hide('submit')
    }
    
    #if user chooses to show all variable
    if (input$selectAll == TRUE){
      #just simply show the data 
      data_shown()
    }else{
      
      #else, get the xaxis,yaxis,facet row and facet column
      selected = c(input$xaxis,input$yaxis)
      
      # . means none for facet_row input, so if it is none, ignore it
      if (input$facet_row != '.'){
        #else, appends it to the selected vector
        selected = c(selected,facet_row())
      }
      
      #same trick with facet_row
      if (input$facet_col != '.')
        selected = c(selected,facet_column())
      
      #same trick with facet_row and column but just that none is none for legend input
      if (input$legend != 'None')
        selected = c(selected,input$legend)
  
      #show the table
      a <- subset(data_shown(), select = unique(selected))
      a
    }
  })
  
  output$bar <- renderPlot({
    if(tabselected() == 'Bar'){
      show('sampleSize')
      show('xaxis')
      show('yaxis')
      show('facet_row')
      show('facet_col')
      hide('legend')
      hide('selectAll')
      hide('modelCheckbox')
      hide('age')
      hide('income')
      hide('rebuild')
      hide('submit')
    }
    
    #position = dodge means that the group of bars will be side by side
    #by default it is stack, means that the group of bars will stack on each other
    #y = ..prop.. means it is count divided by sum of all count that belongs to same group
    #label = scales::percent is making it to become percentange to be shown as labels
    #limits = c(0,1) is making the y-axis labels to show from 0.00 to 1.00 which converts to percentage are 0% to 100%
    #vjust = -.5 makes the label text like 69.7% to be shown above the bar for each group
    gg <- ggplot(data_shown(), aes_string(x=input$xaxis, group=input$yaxis, fill= input$yaxis)) +
      geom_bar(aes(y=..prop..),position="dodge")+
      scale_y_continuous(limits = c(0,1),name = 'Percentage', labels = scales::percent)+
      geom_text(aes(label = scales::percent(..prop..), y=..prop..), 
                stat="count", vjust=-.5,position=position_dodge(0.9))
 
    
    facets <- paste(facet_row(), '~', facet_column())
    if (facets != '. ~ .')
      gg <- gg + facet_grid(facets)
    
    #show the bar
    gg
  })
 
  
  output$trainAccuracy <- renderText({
    if(tabselected() == 'Prediction Model'){
      hide('sampleSize')
      hide('xaxis')
      hide('yaxis')
      hide('facet_row')
      hide('facet_col')
      hide('selectAll')
      hide('legend')
      show('modelCheckbox')
      show('age')
      show('income')
      show('rebuild')
      show('submit')
    }
    
    #format(), nsmall = 2 is making it to become 2 decimal places only
    #this is calculating the accuracy of model on predicting train set in percentage
    paste(format(round(sum(train_predict == train_set$Employed)/nrow(train_set)*100),nsmall=2),"%")
  })
  
  output$trainConfusion <- renderTable({
    #makes a confusion matrix on train set
    #it displays the number of true positive, false positive, true negative and false negative
    table(Prediction = train_predict , Actual = train_set$Employed)
  })
  
  #calculate accuracy of model on predicting test set in percentage
  output$testAccuracy <- renderText({
    paste(format(round(sum(test_predict == test_set$Employed)/nrow(test_set)*100),nsmall=2),"%")
  })
  
  output$testConfusion <- renderTable({
    #makes a confusion matrix on test set
    #it displays the number of true positive, false positive, true negative and false negative
    table(Prediction = test_predict , Actual = test_set$Employed)
  })
  
  
  output$predictOutput <- renderText({
    
    #create a dataframe for the model to predict output
    #these are the default values of the dataframe
    input_data <- data.frame(
      Disabled = factor("FALSE", levels = levels(dataset$Disabled)),
      Internet.Access = factor("FALSE", levels = levels(dataset$Internet.Access)),
      Receive.Food.Stamps = factor("FALSE", levels = levels(dataset$Receive.Food.Stamps)),
      Anxiety = factor("FALSE", levels = levels(dataset$Anxiety)),
      Age = input$age,
      Household.Income = input$income
    )
    
    #check is any one or more than one of the check box in the check box group is selected
    #if it is, then update the value in the dataframe
    # "str" %in% vector means that "str" is inside the vector
    # so if it is TRUE, then means the variable's value is TRUE, update the dataframe
    if(length(input$modelCheckbox) > 0){
      if("Legally Disable" %in% input$modelCheckbox){
        input_data$Disabled = "TRUE"
      }
      if("Internet Access" %in% input$modelCheckbox){
        input_data$Internet.Access = "TRUE"
      }
      if("Received Food Stamps" %in% input$modelCheckbox){
        input_data$Receive.Food.Stamps = "TRUE"
      }
      if("Anxiety" %in% input$modelCheckbox){
        input_data$Anxiety = "TRUE"
      }
    }
    
    #predict on the input
    prediction <- predict(logistic_model, input_data, type = 'response')
    
    #if the probability more than or equals to 0.5, it is employed
    #else, it is unemployed
    prediction <- ifelse(prediction >=0.5 , "Congratulation, the result is employed!" , "Sadly, it looks like would be unemployed")
    
    #show the result of the prediction
    prediction
    
  })

}

#run the shiny app
shinyApp(ui = ui, server=server)

