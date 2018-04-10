#' Get Document Scores from the Prattle API
#'
#' Primary R function for gathering document scores.
#'
#' @param bank          : A string vector that matches a central bank shorthand code.
#' @param type          : Defaults to an xts data frame.
#' @param email         : A string vector with an email for portal login
#' @param portal_pwd    : A string vector with a password for portal login
#' @param agg.level     : Level of aggregation for the data. Default is raw scores. Can be "daily" or "weekly".
#' @param ssl_workaround : Rarely used option for users with SSL certification issues. Can be "True", or "False".
#'
#'
#' @return Dataframe. By default, xts/zoo type.
#'
#' @examples
#' head(get_scores('frc', email = '<YOUR EMAIL>', pwd = '<YOUR PASSWORD>'))
#'
#' @export
get_scores <- function (bank='frc',
                        type='xts',
                        email=NULL,
                        pwd=NULL,
                        agg.level = 'raw',
                        add.extra = TRUE,
                        ssl_workaround = FALSE
) {
  
  
  x<-c('lubridate', 'httr', 'jsonlite', 'xts', 'RCurl')
  lapply(x, FUN = function(X) {
    suppressMessages(do.call("require", list(X)))
  })
  
  # request user authentication from server to get json web token
  jwt_url <- "https://portal.prattle.co/auth/local/"
  if(ssl_workaround){
    jwt_result <- postForm(jwt_url,email=email,password=pwd,style="POST", .opts = list(ssl.verifypeer = FALSE))  
  } else {
    jwt_result <- postForm(jwt_url,email=email,password=pwd,style="POST")  
  }
  
  jwt_result <- fromJSON(jwt_result)
  print(jwt_result)
  
  # define url and clear cache to make sure connection is live.
  url<-paste0("https://portal.prattle.co/api/documents/bank/", bank, '/')
  
  handle_find(url)
  handle_reset(url)
  
  # paste Bearer to auth token
  auth.token.fin<-paste0('Bearer ', jwt_result$token)
  
  json.data<-GET(url, add_headers(Authorization=auth.token.fin))
  
  text<-content(json.data, "text")
  text2<-gsub('NaN', 10000, text)
  json<-fromJSON(text2, flatten=T)
  
  if('speaker' %in% names(json)){
    # extract speaker name and resid info
    hold<-lapply(json$speaker, '[', 'residual')
    dat<-as.data.frame(do.call(rbind, lapply(hold, rbind)))
    dat2<-sapply(dat, function(x) ifelse(x == "NULL", NA, x))
    json$speaker.residual<-unlist(dat2[,1])
    
    
    hold<-lapply(json$speaker, '[', 'name')
    dat<-as.data.frame(do.call(rbind, lapply(hold, rbind)))
    dat2<-sapply(dat, function(x) ifelse(x == "NULL", NA, x))
    json$speaker.name<-unlist(dat2[,1])
  }
  
  
  
  if(add.extra){
    col.index<-c('date_updated', 
                 'score', 
                 'url', 
                 'speaker.name', 
                 'speaker.residual')
  } else {
    col.index<-c('date_updated', 'score')
  }
  json<-json[, col.index]
  df<-subset(json, score!=10000)
  
  if(add.extra){
    names(df)<-c('date', 'score', 'url', 'speaker', 'resid')
  } else {
    names(df)<-c('date', 'score')
  }
  
  df$date<-ymd_hms(df$date)
  
  if(agg.level=='daily'){
    df$date<-ymd(format(df$date, "%Y%m%d"))
    df<-df[,c('date', 'score')]
    df<-aggregate(df$score, by=list(df$date), mean)
    names(df)<-c('date', 'score')
  }
  
  if(type=='xts') {
    df<-xts(df$score, order.by=df$date)
  }
  
  return(df)
}

