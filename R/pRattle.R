#' Get Document Scores from the Prattle API
#'
#' Primary R function for gathering document scores.
#'
#' @param bank A string vector that matches a central bank shorthand code.
#' @param type Defaults to an xts data frame.
#' @param auth.token Oauth2 token (a string) used to retreive information from Prattle API.
#' @param agg.level Level of aggregation for the data. Default is raw scores. Can be "daily" or "weekly".
#' 
#'
#' @return Dataframe. By default, xts/zoo type.
#'
#' @examples
#' head(get_scores('frc'))
#'
#' @export
get_scores <- function (bank, 
                        type='xts',
                        auth.token=NULL,
                        agg.level = 'raw'
                        ) {
  
  
  x<-c('lubridate', 'httr', 'jsonlite', 'xts')
  lapply(x, FUN = function(X) {
    suppressMessages(do.call("require", list(X))) 
  })
  
  
  # define url and clear cache to make sure connection is live.
  url<-paste0("https://banks.prattle.co/api/documents/bank/", bank, '/')  
  
  handle_find(url)
  handle_reset(url)
  
  # paste Bearer to auth token
  auth.token.fin<-paste0('Bearer ', auth.token)
  
  # TODO: disable after ssl cert is set up
  # set_config( config( ssl.verifypeer = 0L ) )
  
  json.data<-GET(url, add_headers(Authorization=auth.token.fin))
  
  text<-content(json.data, "text")
  text2<-gsub('NaN', 10000, text)
  json<-fromJSON(text2)
#   print(names(json))
  json<-json[, c('date_updated', 'score', 'url')]
  df<-subset(json, score!=10000)
  names(df)<-c('date', 'score', 'url')
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
