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

altura <- read.fwf('altura211.dat', widths=c(3,4,rep(1,14)),header=FALSE,dec='.')
altura[altura==9] <- 0

df <- data.frame(
  Questions = c("Na cama, você frequentemente sente frio nos pés?",
                "Você frequentemente desçe as escadas de dois em dois degraus?",
                "Você acha que se daria bem em um time de basquete?",
                "Como policial você impressionaria muito?",
                "Na maioria dos carros você se sente desconfortável?",
                "Você literalmente olha para seus colegas de cima para baixo.",
                "Você é capaz de pegar um objeto no alto de um armário sem usar escada?",
                "Você abaixa quando vai passar por uma porta?",
                "Você consegue guardar a bagagem no porta-malas do avião?",
                "Você regula o banco do carro para trás?",
                "Normalmente, quando você está andando de carona, lhe oferecem o banco da frente?",
                "Quando você e várias outras pessoas vão tirar fotos, formando-se três fileiras, onde ninguém ficará agachado, você costuma ficar atrás?",
                "Você tem dificuldade para se acomodar no ônibus?",
                "Em uma fila, por ordem de tamanho, você é sempre colocado atrás?"),
  Answer = "Sim",
  Option.1 = "Sim",
  Option.2 = "Não",
  Type = "radio"
)

mirt_model = mirt(altura[,-c(1,2)], itemtype = "2PL", SE = F,verbose = F)

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
