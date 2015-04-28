#' Get Document Scores from the Prattle API
#'
#' Primary R function for gathering document scores.
#'
#' @param bank A string vector that matches a central bank shorthand code.
#' @param startdate Date to start the resulting data set from.
#' @param enddate Final date of data for data set.
#' @param type Defaults to an xts data frame.
#' @param auth.token Oauth2 token used to retreive information from Prattle API.
#' @param agg.level Level of aggregation for the data. Default is raw scores. Can be daily or weekly.
#' @param dev Pull from dev or prod?
#'
#' @return Dataframe. By default, xts/zoo type.
#'
#' @examples
#' head(get_scores('frc'))
#'
#' @export
get_scores <- function (bank, 
                        startdate=19980101, 
                        enddate=NA, 
                        type='xts',
                        auth.token="Bearer bRtn76o7'g3U66?yo3S~K6(vfqLC~8",
                        agg.level = 'raw',
                        dev = F
                        ) {
  x<-c('lubridate', 'httr', 'jsonlite', 'xts')
  lapply(x, FUN = function(X) {
    suppressMessages(do.call("require", list(X))) 
  })
  
  if(dev) {
    auth.token<-"Bearer maQpgYqgZqH6P1geZ1zZawIsPT6mRu"
    url<-paste0("https://pa-api-dev.net/api/documents/bank/", bank)
  } else {
    url<-paste0("https://banks.prattle.co/api/documents/bank/", bank)  
  }
  
  handle_find(url)
  handle_reset(url)
  
  # TODO: disable after ssl cert is set up
  set_config( config( ssl.verifypeer = 0L ) )
  
  json.data<-GET(url, add_headers(Authorization=auth.token))
  
  text<-content(json.data, "text")
  text2<-gsub('NaN', 10000, text)
  json<-fromJSON(text2)[, c('date_updated', 'score', 'url', 'feed')]
  df<-subset(json, score!=10000)
  names(df)<-c('date', 'score', 'url', 'feed')
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
