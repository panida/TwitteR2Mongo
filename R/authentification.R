

#' Store Twitter-API Credentials
#'
#' This function stores you Twitter-API key and secret to make it available for other functions and
#' for other instances of R.
#'
#' @param key The key for the Twitter-API: Consumer Key (API Key)
#' @param secret The scret for the Twitter-API: Consumer Secret (API Secret)
#'
#' @return None
#'
#' @examples
#' setAPICredentials(key = "Consumer Key", secret = "Consumer Secret")
#'
#' @export

setAPICredentials <- function(key, secret,access_token="",access_secret="") {
  if ((!class(key) == "character") ||
      (!class(secret) == "character")) {
    stop("Key and sectet must be character")
  }
  Sys.setenv(twitter_key = key)
  Sys.setenv(twitter_secret = secret)
  Sys.setenv(access_token = access_token)
  Sys.setenv(access_secret = access_secret)
  message("Credentials stored, make sure to set callback url to http://127.0.0.1:1410")
}

#' Make OAuth
#'
#' Creates oAuth token for Twitter-API Access and returns token
#'
#' @param key The key for the Twitter-API (character)
#' @param secret The scret for the Twitter-API (character)
#'
#' @return OAuth token
#'
#' @examples
#' #makeOAuth()
#'
#' @export
#'

get_twitter_token_via_sign = function(app, access_token, access_secret) {
  print("Using direct authentication")
  params <- list(as_header = TRUE)
  credentials <- list(oauth_token = access_token, 
                      oauth_token_secret = access_secret)
  twitter_token <- httr::Token1.0$new(endpoint = NULL, params = params, 
                                app = app, credentials = credentials)

  if (is.null(twitter_token))
    stop("Invalid response for twitter_token")

  twitter_token
}

get_twitter_token_via_browser = function(app, ...) {
  print("Using browser based authentication") 
  httr::oauth1.0_token(httr::oauth_endpoints('twitter'), app)
}
  
makeOAuth <-
  function(key = Sys.getenv("twitter_key"), secret = Sys.getenv("twitter_secret"),access_token=Sys.getenv("access_token"), access_secret=Sys.getenv("access_secret")) {
    # Generate or load authetification token, please store the credentials

    if ((key == "") || (secret == "")) {
      stop("Twitter key or secret are not set properly, use setAPICredentials to fix this.")
    }
    if ((key == "") || (secret == "")) {
      token_func = get_twitter_token_via_browser
    } else {
      token_func = get_twitter_token_via_sign
    }
    myapp <- httr::oauth_app("twitter", key = key, secret = secret)
    twitter_token = token_func(myapp, access_token, access_secret)
    assign("oauth_token", twitter_token, envir=oauth_cache)
    return (twitter_token)
  }
