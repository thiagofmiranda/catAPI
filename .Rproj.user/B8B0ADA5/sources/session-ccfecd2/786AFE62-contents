#
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
#
# In RStudio 1.1 or older, see the Plumber documentation for details
# on running the API.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(mirtCAT)
library(RMySQL)
library(mirt)
library(dplyr)
library(purrr)
library(stringr)

con <- function(database){
  # criando conexão com a base de dados
  lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  cn <- dbConnect(MySQL(), host= "db-service",port= 3307, user = "thiago", password = "123456",
                  dbname = database)
  if(Sys.getlocale("LC_COLLATE")!="Portuguese_Brazil.1252"){dbSendQuery(cn, "SET CHARSET 'utf8';")}
  return(cn)
}

generate_mirt_object <- function(pars,mod='3PL',ncat=2,transform=F) {
  to_mirt <- function(a,b,c,mod,ncat) {
    vec <- c(a=a, b=b, c=c, u=1)
    data.frame(t(mirt::traditional2mirt(vec, mod, ncat)))
  }
  
  if(transform){
    pars <- pmap_dfr(.l = list(a = pars$a, b = pars$b, c = pars$c, mod=list(mod), ncat=list(ncat)), .f = to_mirt)
  }
  
  ## Generate mirt_object for CAT session
  mirt_object <- generate.mirt_object(pars, itemtype = mod, latent_covariance = matrix(2))
  
  return(mirt_object)
}

altura <- dbReadTable(con("altura"), "altura")
df <- dbReadTable(con("altura"), "df")
pars <- dbReadTable(con("altura"), "pars")

mirt_model <- generate_mirt_object(pars,mod = "2PL",transform = T)


#* @apiTitle Plumber Example API

#* Return the sum of two numbers
#* @get /results
function(){
  list(
    is_over = is_stop(CATdesign$person$raw_responses,CATdesign$person$thetas_history,CATdesign$design@delta_thetas),
    ites_answered =factor(CATdesign$person$items_answered[!is.na(CATdesign$person$items_answered)]),
    responses = CATdesign$person$raw_responses,
    theta_hist = as.numeric(CATdesign$person$thetas_history)* 8 + 168,
    theta_se_hist = as.numeric(CATdesign$person$thetas_SE_history)* 8 + 168
  )
}

#* Return the sum of two numbers
#* @post /new_test
function(min_items=5,max_items=10){
  
  design <<- list(delta_thetas  = 0.03, min_items = min_items, max_items = max_items)
  
  CATdesign <<- mirtCAT(mo = mirt_model, criteria = 'MI', design_elements = TRUE,  start_item = 1, design = design)
  
  ni <<- findNextItem(CATdesign)
  
  list(
    question = df$Questions[ni],
    question_alternatives = list(df$Option.1[ni],df$Option.2[ni])
  )
  
}

is_stop <- function(raw_responses,thetas,criteria){
  
  is_all_resp <- all(!is.na(raw_responses))
  
  thetas <- as.numeric(thetas)
  n_thetas <- length(thetas)
  
  if(is_all_resp){
    T
  }
  if(n_thetas > 1){
    theta_1 <- thetas[n_thetas-1]
    theta_2 <- thetas[n_thetas]
    abs(diff(c(theta_1,theta_2))) <= criteria
  }else{
    F
  }
  
}

#* Return the sum of two numbers
#* @param response vetor de respostas
#* @post /answer_question
function(response){
  
  remaining_itens <- sum(is.na(CATdesign$person$raw_responses))
  if(is_stop(CATdesign$person$raw_responses,CATdesign$person$thetas_history,CATdesign$design@delta_thetas)){
    list(
      is_over = T,
      ites_answered =factor(CATdesign$person$items_answered[!is.na(CATdesign$person$items_answered)]),
      responses = CATdesign$person$raw_responses,
      next_quetion = "The test is over!",
      next_quetion_alternatives = "The test is over!",
      theta_hist = as.numeric(CATdesign$person$thetas_history),
      theta_se_hist = as.numeric(CATdesign$person$thetas_SE_history)
    )
  }else if(remaining_itens != 1){
    CATdesign <<- updateDesign(CATdesign, new_item = ni, new_response = (response == "Sim")*1,updateTheta = T)
    
    ni <<- findNextItem(CATdesign)
    
    list(
      is_over = F,
      ites_answered =factor(CATdesign$person$items_answered[!is.na(CATdesign$person$items_answered)]),
      responses=CATdesign$person$raw_responses,
      next_question=df$Questions[ni],
      next_question_alternatives=list(df$Option.1[ni],df$Option.2[ni]),
      theta_hist=as.numeric(CATdesign$person$thetas_history),
      theta_se_hist=as.numeric(CATdesign$person$thetas_SE_history)
    )
    
  }else{
    CATdesign <<- updateDesign(CATdesign, new_response = (response == "Sim")*1,updateTheta = T)
    
    list(
      is_over = F,
      ites_answered =factor(CATdesign$person$items_answered[!is.na(CATdesign$person$items_answered)]),
      responses = CATdesign$person$raw_responses,
      next_question = df$Questions[ni],
      next_question_alternatives = list(df$Option.1[ni],df$Option.2[ni]),
      theta_hist = as.numeric(CATdesign$person$thetas_history),
      theta_se_hist = as.numeric(CATdesign$person$thetas_SE_history)
    )
  }
  
}
