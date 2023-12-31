


library(httr)
library(jsonlite)
library(ggplot2)


start_test <- function(min_items=5,max_items=10){
  url <- "http://127.0.0.1:1994/"
  
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
  url <- "http://127.0.0.1:1994/"
  
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
  url <- "http://127.0.0.1:1994/"
  
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
    ylab("Theta")+xlab("Item")+
    ylim(-3,3)+
    theme_bw()
  
}



start_test()
answer_question("Não")
results()
plot_theta()


# ----------------------------------------------
install.packages("RMySQL")
library(RMySQL)

con <- function(database){
  # criando conexão com a base de dados
  lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  cn <- dbConnect(MySQL(), host= "localhost",port= 3307, user = "thiago", password = "123456",
                  dbname = database)
  if(Sys.getlocale("LC_COLLATE")!="Portuguese_Brazil.1252"){dbSendQuery(cn, "SET CHARSET 'utf8';")}
  return(cn)
}


dbGetQuery(con("altura"), "SHOW TABLES")


altura <- read.fwf('api/altura211.dat', widths=c(3,4,rep(1,14)),header=FALSE,dec='.')
altura[altura==9] <- 0

dbReadTable(con("altura"), "altura")
dbReadTable(con("altura"), "df")

dbWriteTable(con("altura"), "altura", altura,overwrite=T)


dbWriteTable(con("altura"), "df", df,overwrite=T)


# https://stackoverflow.com/questions/25920029/setting-up-mysql-and-importing-dump-within-dockerfile
# 


# -----------------------------------
library(ltm)
ltm_model <- tpm(data=altura[,-c(1,2)],constraint=cbind(1:14,1,0))

pars <- coef(ltm_model, simplify = TRUE) |> 
  data.frame() |> 
  setNames(c("c","b","a")) 

dbWriteTable(con("altura"), "pars", pars,overwrite=T)
