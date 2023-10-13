library(shiny)
library(shinydashboard) # <-- Change this line to: library(semantic.dashboard)
library(httr)
library(jsonlite)
library(ggplot2)


start_test <- function(min_items=5,max_items=10){
  url <- "http://api-service:1994/"
  
  url_role <- "new_test"
  payload <- toJSON(auto_unbox = T,
                    list(
                      min_items = min_items,
                      max_items = max_items
                    ))
  
  encode <- "json"
  
  response <- VERB(
    "POST", 
    url = paste0(url,url_role), 
    body = payload, 
    content_type("application/json"), 
    accept("application/json"), encode = encode)
  
  fromJSON(content(response, "text"))
}
answer_question <- function(response){
  url <- "http://api-service:1994/"
  
  url_role <- "answer_question"
  payload <- toJSON(auto_unbox = T,
                    list(
                      response = response
                    ))
  
  encode <- "json"
  
  response <- VERB(
    "POST", 
    url = paste0(url,url_role), 
    body = payload, 
    content_type("application/json"), 
    accept("application/json"), encode = encode)
  
  fromJSON(content(response, "text"))
}
results <- function(){
  url <- "http://api-service:1994/"
  
  url_role <- "results"
  
  encode <- "json"
  response <- VERB(
    "GET", 
    url = paste0(url,url_role), 
    content_type("application/json"), 
    accept("application/json"))
  
  fromJSON(content(response, "text"))
  
  
}
plot_theta <- function(){
  
  res <- results()
  
  df <- data.frame(
    item=factor(res$ites_answered),
    theta_hist=res$theta_hist[-1],
    theta_se_hist=res$theta_se_hist[-1],
    thetasSEhigh=(res$theta_hist+res$theta_se_hist)[-1],
    thetasSElow=(res$theta_hist-res$theta_se_hist)[-1]) 
  
  df|> 
    ggplot(aes(x=item,y=theta_hist, group=1))+
    geom_line(linewidth=0.7)+
    geom_ribbon(aes(ymin=thetasSElow, ymax=thetasSEhigh),alpha=0.3)+
    scale_x_discrete(expand = c(0.01,0.01))+
    ylab("Altura (em cm)")+xlab("Item")+
    theme_bw()
  
}

ui <- dashboardPage(
  dashboardHeader(title = "Altura"),
  dashboardSidebar(
    sidebarMenu(
    column(12,align = "center",offset = 0,actionButton("start","Começar"))
  )),
  dashboardBody(
    fluidRow(
      box(width = 12,title = "Teste adaptativo de Altura pela TRI",status = "primary",
        tags$div(id = 'placeholder') 
      )
    )
  )
)

server <- function(input, output) {
  
  output$SelectCategory <-renderUI({
    radioButtons(inputId = "radio",label =  "Nhu",inline = F,
                 choices = list("Choice 1" = 1, "Choice 2" = 2,"Choice 3" = 3),
                 selected = 1)
  })
  
  
  ## keep track of elements inserted and not yet removed
  inserted <- c()
  
  observeEvent(input$start, {
    btn <- input$start
    id <- paste0('txt', btn)
    
    
    api_start <- start_test()
    item <- api_start$question
    choices <- as.character(api_start$question_alternatives)
    names(choices) <- choices
    
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
    
    
    insertUI(
      selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
      
      ui = tags$div(
        box(width =12,
            radioButtons(inputId = "resp",label =  item,inline = F,choices = as.list(choices)), 
            actionButton("next_item","Next")
        ),
        id = id
      )
    )
    inserted <<- c(id, inserted)
  })
  
  observeEvent(input$next_item, {
    btn <- input$next_item
    id <- paste0('txt', btn)
    
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
    
    response <- input$resp
    
    api_answer_question <- answer_question(response)
    item <- api_answer_question$next_question
    choices <- as.character(api_answer_question$next_question_alternatives)
    names(choices) <- choices
    
    insertUI(
      selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
      
      ui = tags$div(
        if(!api_answer_question$is_over){
          
          box(width =12,
              radioButtons(inputId = "resp",label =  item,inline = F,choices = as.list(choices)), 
              actionButton("next_item","Next")
          )
        }else{
          api_result <- results()
          final_result <- round(api_result$theta_hist[length(api_result$theta_hist)])
          box(width =12,
              div(h4(paste0("Seu teste acabou, sua altura é de: ", final_result)),align="center"),
              renderPlot({
                plot_theta()
              })
              
          )
        }

        ,
        id = id
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
}

options(shiny.host = '0.0.0.0')
options(shiny.port = 3838)
shinyApp(ui, server)
