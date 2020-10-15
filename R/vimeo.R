#' OAuth2.0 procedure for Vimeo
#'
#' @return
#' @export
#'
#' @examples none
vimeo_auth <- function(){
  clientId <- "f909156f231b78083c0a73a27abf49740357f0e2"
  clientSecret <- "IUjR8pOWKS4hu7PvjBQAV+BkOqbDyAOwjaKDggOa+m9XnLDw3HUT4fVcVT21PnmWmjx3Fis+bXbnxgZKxfDQqQe6hLnOsNiTN17P0h46pZuJkoMnu48+8IFfHaCiK/r7"
  auth.code <- httr::oauth2.0_token(endpoint = httr::oauth_endpoints("vimeo"),
                                    app = httr::oauth_app(
                                      appname="student video watching",
                                      key=clientId,
                                      secret=clientSecret
                                    ),
                                    scope = c("public","private"))
  return(auth.code)
}
# clientSecret <- "c7dNwD8FV/H8j3FBkF8nyguxyM4yeHF+KdJjH9FTVW3Lms1CwCcc3KU21QwsjfT5QLEEtRJU5JLPSuw696cknjAyvY3YOQzlenmdqA17onR+xjFgnzDBltqCJIqENIa9"
# clientId <- "ac08ba30b859e89e5c0a95865c0c9217b7a8d4bb"
#
# # https://www.r-bloggers.com/how-to-authenticate-using-oauth2-through-r/
# library(httr)
# Ask user to authorize access to his or her data and gain a token.

#' Get information of all my videos
#'
#' @param auth.code A list, returned by vimeo_auth()
#'
#' @return A data frame.
#' @export
#'
#' @examples none
get_myVideoData <-function(auth.code=vimeo_auth()){
  httr::GET("https://api.vimeo.com/me/videos",
      config=httr::config(token=auth.code)) -> response

  httr::content(response, as="text") -> response_text

  jsonlite::fromJSON(response_text) -> response_list

  response_list$data -> videoData

  # paging
  library(dplyr); library(stringr)
  response_list$paging$last %>%
    str_extract("[0-9]+$") -> lastPageNumber
  response_list$paging$last %>%
    str_extract(glue::glue("[:graph:]+(?={lastPageNumber})")) -> endpoint

  for(pageNumber in 2:as.integer(lastPageNumber)){
    httr::GET(glue::glue("https://api.vimeo.com{endpoint}{pageNumber}"),
        config=httr::config(token=auth.code)) -> response
    httr::content(response, as="text") -> response_text

    jsonlite::fromJSON(response_text) -> response_list

    bind_rows(videoData, response_list$data) -> videoData

  }

  videoData
}

#' create Vimeo service
#'
#' @return
#' @export
#'
#' @examples none
vimeoService_create <- function(){

  rlang::new_environment(parent=globalenv()) -> vimeoServe
  vimeoServe$auth <- vimeo_auth
  vimeoServe$getMyInfo <- retrieve_myInfo
  vimeoServe$getMyShowcases <- get_userShowcases
  vimeoServe$get_showcaseVideos <- get_showcaseVideos
  vimeoServe$getVideo <- get_myVideoData
  return(vimeoServe)
}



# helper ------------------------------------------------------------------



vimeoService <- function(token=vimeoToken,
                         key=vimeoKey,
                         secret=vimeoSecret){

  vimeoFun <- reticulate::import("vimeo")
  vimeoFun$VimeoClient(
    token=token,
    key=key,
    secret=secret
  ) -> v_service

  # v_service$mget <- v_service %>% extendto_mget()

  return(v_service)
}

extendto_mget <- function(v_service){
  mget <- function(servicePoint){
    availableVideos_response = v_service$get(servicePoint)
    availableVideos = availableVideos_response$json()
    availableVideos[["paging"]][["first"]] -> first_page
    str_extract(first_page,"[:digit:]+$") %>% as.integer() -> count_first
    availableVideos[["paging"]][["last"]] %>%
      str_extract("[:digit:]+$") %>% as.integer() -> count_last
    # showcase原始資料 availableVideos_list
    availableVideos_list = vector("list", length(count_last))
    availableVideos_list[[1]] <- availableVideos
    first_page %>% str_replace("page=1","page={i}") -> glueText_i
    if(count_first != count_last){
      for(i in count_first:count_last){
        availableVideos_list[[i]] <-{
          v_service$get(glue::glue(glueText_i)) -> v_response
          v_response$json()
        }
      }
    }

    return(availableVideos_list)
  }
  v_service$mget <- mget
  v_service
}

form_vimeoDataframe <- function(availableVideos){
  if("data" %in% names(availableVideos)){
    availableVideos$data %>%
      map_dfr(
        ~.x[c("name","duration","created_time")])
  } else {
    stop("Maybe you should use map_dataFrame instead.")
  }

}
map_vimeoDataframe <- function(availableVideos_list){
  if("data" %in% names(availableVideos_list)){
    stop("Maybe you should use form_dataFrame instead.")
  } else {
    map_dfr(availableVideos_list,
            form_dataFrame)

  }
}

get_studentVideoWatchingData <- function(params, date_range, df_lms){

  drake::make(classmanagement::plan_googleAnalytics)
  drake::readd(df_video_watching_complete)
}

view_googleAnalytics_plan <- function(){
  file.edit(
    file.path(
      cmRoot_file(),"R","drake_googleAnalytics.R"
    )
  )
}

get_albumIdFromShowcaseUri <- function(uri){
  uri %>%
    stringr::str_extract("(?<=/)[:digit:]+$")
}
