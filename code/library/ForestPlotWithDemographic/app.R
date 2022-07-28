library(readxl)
library(metafor)
library(tools)
library(gridExtra)
library(grid)
library(ggplot2)
library(shiny)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(dplyr)

load("data.RData")

######### DEFINE HARD CODED PARAMETERS, change these to variables #####################
pred_length = 100
cex_lab_value = 0.8 # magnification of x and y labels relative to cex
cex_main_value = 0.8 # magnification of titles relative to cex
las_value = 1
pch_value = 19
size_min_value = 0.5
size_ratio_value = 0.5                     
########################################################################################

#####DATA CLEANING##########
FileReadFunc = function(path) {
  if(file_ext(path) == 'txt')
    data = read.delim(path)
  if(file_ext(path) == 'csv')
    data = read.csv(path)
  if(file_ext(path) == 'xls' || file_ext(path) == 'xlsx')
    data = read_excel(path)
  return(data)
}

RemoveMissingFunc = function(data, desiredCols) {
  completeVec = complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

FilterMergFunc = function(data, desiredCols) {
  missingVec = is.na(data[, desiredCols])
  return(data[missingVec, ])
}

RemoveNonEuroFunc = function(data){
  return(data[!is.element(data$Study,NonEuro),])
}

DataStats = function(data) {
  print(summary(data))
  print(fivenum(data))
}

#Define a function to adjust the digits of the statistics
digits = function(regression_object){
  if(abs(regression_object$beta[1])<0.001){
    round_decimal=5
  }else{round_decimal=2}
  return(round_decimal)
}

#####READ DATA AND DATA CLEANING########
#Read data from csv
data = FileReadFunc("data.csv") 

#Remove missing data in Effect and Age
data = RemoveMissingFunc(data, c(effect, demographic))

#Remove mergeD cohorts based on column N
data = FilterMergFunc(data, cohort_size) 

#Remove Non European cohorts
data = RemoveNonEuroFunc(data) 

# Manually add Age when NA
data[is.na(data[demographic]),demographic] = 30

#Convert data types for certain columns
data[cols]=lapply(data[cols],as.character)
data[cols]=lapply(data[cols],as.numeric)

#Extract features to run meta regression
data[cohort_size] = data[sample_size]
data$SE =(data[[CI_UBB]] - data[[CI_LBB]])/(2*1.96)
data$Z = data[effect]/data$SE

#######UI########
ui <- fluidPage(
  
  #Theme
  theme=shinytheme("lumen"),
  #Title
  titlePanel(title = h2("Meta-Analysis of ENIGMA Studies", style = "text-align:center; font-weight:bold;color:black")),
  
  #Demographic filter
  mainPanel(
    h3("Enter Range",style="text-align:left; font-weight:bold;color:black"),
    splitLayout(
      cellArgs = list(style="padding:2px"),cellWidths = 140,
      numericInput("min",min=min_val,max=max_val,label=tags$p(tags$span("Min Value",style="text-align:left; font-weight:bold;color:black;font-size:110%;"), tags$span(paste("( min is", min_val, ")", sep=" "),style="text-align:left;color:black")),value=min_val, width="120px"),
      numericInput("max",min=min_val,max=max_val,label=tags$p(tags$span("Max Value",style="text-align:left; font-weight:bold;color:black;font-size:110%;"), tags$span(paste("( max is", max_val, ")", sep=" "),style="text-align:left;color:black")),value=max_val, width="120px"),
      tags$button(id="enter_button", class="btn action-button", tags$img(src="https://s3-us-west-2.amazonaws.com/courses-images/wp-content/uploads/sites/855/2016/11/15045703/brainlobes.png",
                                                                         height="20px", width="25px", style="align:left"),
                  tags$span(" Run Analysis"))),
    tags$style(style='text/css', "#enter_button {height:37px; width:125px; margin-top: 36px; 
                                         background-color:DarkSeaGreen;}",
               "#enter_button:hover {background-color: SeaGreen;
                                              font-weight:bold;}",
               "#enter_button:focus {outline:none;}"),
    #Annotations
    tags$p(
      tags$div("The results are for: ", style="font-weight:bold;color:LightSlateGray;font-size:120%"),
      tags$span("Area: ", style="font-weight:bold;color:LightSlateGray", class="annotation"),
      tags$span(Area, style="color:DarkMagenta"),
      tags$span(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"SNP: ", style="font-weight:bold;color:LightSlateGray", class="annotation"),
      tags$span(snp, style="color:DarkMagenta"),
      tags$span(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Trait: ", style="font-weight:bold;color:LightSlateGray", class="annotation"),
      tags$span(Trait, style="color:DarkMagenta")
    ),
    
    #Outputs panel
    # h3("Outputs",style="text-align:left; font-weight:bold;color:black"),
    
    #Add loading message
    tags$head(tags$style(type="text/css","
             #loadmessage {
               position: fixed;
               top: 500px;
               left: 200px;
               width: 50%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 120%;
               color: #000000;
               background-color: LavenderBlush;
               z-index: 105;
             }
          ")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),
    
    #Outputs  
    navbarPage(id="navbar",title=span("",style="font-weight:bold;font-size:15px"),collapsible = TRUE,fluid = TRUE,selected=paste(demographic_annot,"vs. Allele", sep=" "),
               tabPanel(paste(demographic_annot,"vs. Allele", sep=" "),fluidRow(column(width=12, align='center', offset=2, plotlyOutput("scatterplot1", width = 600, height = 500))),
                        fluidRow(column(width=12, align='left', offset=5, div(tableOutput('table1'), style='font-size:85%'),
                                        # change table style
                                        tags$head(tags$style("#table1 table{background-color:lightgrey}", media="screen",type="text/css"))))),
               tabPanel("Forest Plot",fluidRow(column(width=12, align='center', offset=2, plotOutput("forestplot1", width = 600, height = 800)))),
               tags$style(type="text/css",
                          HTML('.navbar {background-color:Linen;font-size:15px;}',
                               '.navbar-default .navbar-nav>li:hover{background-color:BlanchedAlmond;font-weight:bold;}',
                               '.navbar-default .navbar-nav>.active>a{font-weight:bold;}',
                               '.navbar-brand{display:none}'
                          ))
    )
  )
)              

#######SERVER########
server = function(input, output,session) {
  #Define res function and error control
  res=function(x){
    tryCatch(rma.uni(yi=x[[effect]], sei=x$SE, mods=x[,demographic],verbose=TRUE,method="FE",control=list(stepadj=0.4, maxiter=1000), digits = 5),
             warning=function(w){showModal(modalDialog(
               title="Warning", HTML("Warning:<br>Regression failed<br>Model failed to converge, please select more studies."),easyClose = FALSE, footer = modalButton('OK')))
               return(NULL)},
             error=function(e){showModal(modalDialog(
               title="Warning", HTML("Warning:<br>Regression failed<br>Model failed to converge, please select more studies."),easyClose = FALSE, footer = modalButton('OK')))
               return(NULL)})
  }
  
  observeEvent(input$enter_button, 
               { 
                 #Reset selected tab after recalculation 
                 # updateNavbarPage(getDefaultReactiveDomain(),"navbar",selected=paste(demographic_annot,"vs. Allele" sep=" "))
                 
                 # Evaluate input values 
                 if(between(input$min,min_val,max_val)==FALSE|between(input$max,min_val,max_val)==FALSE){
                   showModal(modalDialog(
                     title="Warning", HTML("Min/Max values are out of range!"),easyClose = FALSE, footer = modalButton('OK')))
                   output$scatterplot1=renderPlotly({})
                   output$forestplot1=renderPlot({})
                   
                 }else if(input$min>input$max){
                   showModal(modalDialog(
                     title="Warning", HTML("Min value cannot exceed max value!"),easyClose = FALSE, footer = modalButton('OK')))
                   output$scatterplot1=renderPlotly({})
                   output$forestplot1=renderPlot({})
                 }  
                 #Generate outputs  
                 else{
                   subset_data=eventReactive(input$enter_button,{subset(data, data[[demographic]] >=input$min & data[[demographic]]<=input$max)},ignoreNULL = FALSE,ignoreInit = FALSE)
                   reg_result = reactive( {res(subset_data())})
                   if(is.null(reg_result())==TRUE){
                     output$scatterplot1=renderPlotly({})
                     output$forestplot1=renderPlot({})
                   }else if(is.null(reg_result())==FALSE){
                     pval = reactive({reg_result()$pval})
                     beta = reactive({reg_result()$beta})
                     #Convert the result to dataframe
                     result = reactive({preds = predict(reg_result(), newmods=seq(min(subset_data()[[demographic]]),max(subset_data()[[demographic]]),length=pred_length))
                     result_df = as.data.frame(preds)
                     result_df[demographic] = seq(min(subset_data()[[demographic]]),max(subset_data()[[demographic]]),length=pred_length)
                     return(result_df)})
                     #Size of dots
                     size = reactive({size_min_value + size_ratio_value * (subset_data()[[cohort_size]] - min(subset_data()[[cohort_size]]))/(max(subset_data()[[cohort_size]]) - min(subset_data()[[cohort_size]]))})
                     #Decimal places
                     round_decimal = reactive({digits(reg_result())})
                     #Calculate statistics
                     beta_ci = reactive(paste("(",format(round(reg_result()$ci.lb[2],round_decimal()),scientific=FALSE),", ",format(round(reg_result()$ci.ub[2],round_decimal()),scientific=FALSE),")",sep=""))
                     beta = reactive(paste(format(round(reg_result()$beta[2],round_decimal()),scientific=FALSE),beta_ci()))
                     p_value = reactive(paste(round(reg_result()$QMp,2)))
                     num_study = reactive(paste(reg_result()$k))
                     
                     #Generate statistics table
                     stats = data.frame(beta = beta(), pval=p_value(), nstudy=num_study())
                     names(stats)[1] = 'Beta (95% CI)'
                     names(stats)[2] = 'Beta P-Value'
                     names(stats)[3] = 'Number of Studies'
                     
                     #Generate meta regression scatter plot
                     output$scatterplot1 = renderPlotly({
                       ggplotly(ggplot(subset_data(), aes(x=subset_data()[[demographic]], y=subset_data()[[effect]])) +
                                  geom_point(aes(size=size(),text=paste0('Study: ',Study,'\n', paste(demographic_annot, ':', sep=' '),subset_data()[[demographic]],'\n','Effect size: ',format(subset_data()[[effect]],scientific=FALSE),'\n','Cohort size: ', N))) +
                                  xlab(demographic_annot) +    
                                  ylab('Unstandardized Effect Size')+
                                  geom_line(data=result(), aes(x=result()[[demographic]], y=pred)) +
                                  geom_line(data=result(), aes(x=result()[[demographic]], y=ci.lb), linetype="dashed") +
                                  geom_line(data=result(), aes(x=result()[[demographic]], y=ci.ub), linetype="dashed") +
                                  theme_classic() +
                                  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),'cm'),
                                        axis.title.x = element_text(face='bold',size=14),
                                        axis.title.y = element_text(face='bold',size=14),
                                        plot.title = element_text(face='bold',size=14,hjust=0.5, vjust=0.5)),
                                tooltip="text")
                     })
                     
                     output$forestplot1 = renderPlot({
                       ci_min = min(subset_data()[[CI_LBB]])
                       x_pos = ci_min - abs(ci_min)*0.3
                       forest(x = reg_result(),
                              ilab = subset_data()[[demographic]],
                              ilab.xpos = x_pos, 
                              slab=paste(subset_data()$Study),     
                              cex=1, 
                              addfit=FALSE, 
                              header=TRUE,
                              digits=round_decimal(),
                              top=1,
                              psize=0.6)
                       y_pos = length(subset_data()$Study) + 2     
                       par(col='black',cex=1,font=2)
                       text(x_pos, y_pos, demographic_annot) 
                       x = predict(reg_result(),newmods = mean(subset_data()[[demographic]]))
                       print(length(x$pred))
                       print(length(x$se))
                       par(col='firebrick2',col.lab='black')
                       addpoly(x$pred, sei=x$se, mlab="Discovery", efac = 1,cex=1.2,rows=-0.5,digits=round_decimal(),col='firebrick2',border='firebrick2')
                     }) }
                 }}, ignoreNULL = FALSE,ignoreInit = FALSE,priority=2)
  
}
shinyApp(ui = ui, server = server)
