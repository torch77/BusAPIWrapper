#### Functions to Wrap Bus Data API
#### To Do
# implement order checking
# input validation (proper routes, stops, etc)
# seperate url build and API query into seperate function to be run after input validation

#' Function to query service, ewt, wtp, otp endpoints
#' @param endpoint String. Endpoint to query one of: service, ewt, wtp, otp
#' @param year_list Integer vector of years to query (2014, 2015, or 2016)
#' @param month_list Character vector of months to query in YYYYMMDD or YYYY-MM-DD format
#' @param route_list Character vector of route ids to query
#' @param stop_list Character vector of stop ids to query
#' @param weekend T/F flag for analyzing weekends. True is just weekends, false is no weekends. Not passing is all days
#' @param period_list Integer vector of periods to query. One or more of:
#' 1: 7am–10am
#' 2: 10am–4pm
#' 3: 4pm–7pm
#' 4: 7pm–11pm
#' 5: 11pm–7am
#' @param sbs T/F flag for whether to include SBS routes in computations
#' @param group_list Character vector of fields to aggregate by. One or more of:
#' year
#' month
#' route_id
#' stop_id
#' weekend
#' period
#' @param order_list Character vector of fields to sort by. One of: year, month, route_id, stop_id, weekend, period, or
#' one of the metrics returne from endpoint. See API documentation. Add a hyphen to field to sort in descending order
#' (e.g. -year)
#' @param limit Integer. Number of results to return
#' @param token String. API documentation says this is required but doesn't appear to be so. See API page to obtain one.
#'
#' @return a dataframe of results
#'
#' @examples
#' bus_query(endpoint = "speed", year_list = c(2015), weekend = FALSE, period_list = c(1,2,3),
#' group_list = ("route_id"), order_list = c("speed"), limit = 10)
#'
#' @export

bus_query <- function(endpoint = "speed", year_list = list(), month_list = list(), route_list = list(), stop_list = list(),
                      weekend = NA, period_list = list(), sbs = NA, group_list = list(), order_list = list(), limit = NA,
                      token = NA){
  ###### don't validate inputs for now ######
  # base url
  base_url <- "http://13.68.19.99/api/v1/"

  # construct url for query
  query <- paste0(base_url, endpoint, "?")

  # collapse lists and include as params
  if(length(year_list)>0){
    years <- paste0(year_list, collapse = ",")
    query <- paste0(query, "years=", years)
  }

  if(length(month_list)>0){
    month <- paste0(month_list, collapse = ",")
    query <- paste0(query, "&months=", month)
  }

  if(length(route_list)>0){
    routes <- paste0(route_list, collapse = ",")
    query <- paste0(query, "&routes=", routes)
  }

  if(length(stop_list)>0){
    stops <- paste0(stop_list, collapse = ",")
    query <- paste0(query, "&stops=", stops)
  }

  if(!is.na(weekend)){
    if(weekend){
      query <- paste0(query, "&weekend=", 1)
    }else{
      query <- paste0(query, "&weekend=", 0)
    }
  }

  if(length(period_list)>0){
    periods <- paste0(period_list, collapse = ",")
    query <- paste0(query, "&periods=", periods)
  }

  if(!is.na(sbs)){
    if(sbs){
      query <- paste0(query, "&sbs=", 1)
    }else{
      query <- paste0(query, "&sbs=", 0)
    }
  }

  if(length(group_list)>0){
    groups <- paste0(group_list, collapse = ",")
    query <- paste0(query, "&groups=", groups)
  }

  if(length(order_list)>0){
    orders <- paste0(order_list, collapse = ",")
    query <- paste0(query, "&order=", orders)
  }

  if(!is.na(limit)){
    query <- paste0(query, "&limit=", limit)
  }

  if(!is.na(token)){
    query <- paste0(query, "&token=", token)
  }

  # query API
  resp <- httr::GET(query)

  # extract response
  resp <- httr::content(resp, as = "text")

  # parse json
  resp_df <- jsonlite::fromJSON(resp, flatten = T)

  # return df
  return(resp_df)

}
