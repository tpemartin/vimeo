# postingMessage="GET /me"
retrieve_myInfo <- function(){
  postingMessage="GET /me"
  get_me <- vimeo_apiFunctional(postingMessage)
  get_me()
}
get_userShowcases <- function(){
  postingMessage = "GET /me/albums"
  get_showcases <- vimeo_apiFunctional(postingMessage)
  get_showcases()
}
get_showcaseVideos <- function(uri=NULL, album_id=NULL)
{
  if(!is.null(uri)){
    album_id = get_albumIdFromShowcaseUri(uri)
  }
  postingMessage=glue::glue("GET /me/albums/{album_id}/videos")
  get_showcaseVideosFun <- vimeo_apiFunctional(postingMessage)
  get_showcaseVideosFun()

}
# myShowcases <- get_userShowcases()
vimeo_apiFunctional <- function(postingMessage){
  split_postingMessage=stringr::str_split(postingMessage,"\\s")
  VERB=split_postingMessage[[1]][[1]]
  path=split_postingMessage[[1]][[2]]
  VERB=as.name(VERB)
  require("httr")
  endpoint="https://api.vimeo.com"
  function(...){
    requestExpr=rlang::quo({
      loadNamespace("httr")
      (!!VERB)(
        url=endpoint,
        path=path,
        config=httr::config(token=vimeo_auth()),...
      )
    })

    response <- rlang::eval_tidy(
      requestExpr
    )
    # response
    httr::content(response, as="text", encoding='UTF-8') -> response_text

    jsonlite::fromJSON(response_text) -> response_list
    response_list
  }
}
# retrieve_myInfo() -> myInfo
