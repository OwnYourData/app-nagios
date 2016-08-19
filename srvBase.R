# basic functions for accessing PIA
# last update:2016-07-27

# Low-level functions to access PIA =======================
# used header for GET and POST requests
defaultHeaders <- function(token) {
        c('Accept'        = '*/*',
          'Content-Type'  = 'application/json',
          'Authorization' = paste('Bearer', token))
}

# URL to access a repo
itemsUrl <- function(url, repo_name) {
        paste0(url, '/api/repos/', repo_name, '/items')
}

# request token for a plugin (app)
getToken <- function(pia_url, app_key, app_secret) {
        auth_url <- paste0(pia_url, '/oauth/token')
        response <- tryCatch(
                postForm(auth_url,
                         client_id     = app_key,
                         client_secret = app_secret,
                         grant_type    = 'client_credentials'),
                error = function(e) { return(NA) })
        if (is.na(response)) {
                return(NA)
        } else {
                return(fromJSON(response[1])$access_token)
        }
}

# vector with all plugin (app) infos to access PIA
setupApp <- function(pia_url, app_key, app_secret) {
        app_token <- getToken(pia_url, 
                              app_key, 
                              app_secret)
        c('url'        = pia_url,
          'app_key'    = app_key,
          'app_secret' = app_secret,
          'token'      = app_token)
}

# Read and CRUD Operations for a Plugin (App) =============
# read data from PIA
readItems <- function(app, repo_url) {
        if (length(app) == 0) {
                data.frame()
        } else {
                headers <- defaultHeaders(app[['token']])
                url_data <- paste0(repo_url, '?size=2000')
                response <- tryCatch(
                        getURL(url_data,
                               .opts=list(httpheader = headers)),
                        error = function(e) { return(NA) })
                if (is.na(response)) {
                        data.frame()
                } else {
                        if (nchar(response) > 0) {
                                retVal <- fromJSON(response)
                                if(length(retVal) == 0) {
                                        data.frame()
                                } else {
                                        if ('error' %in% names(retVal)) {
                                                data.frame()
                                        } else {
                                                if (!is.null(retVal$message)) {
                                                        if (retVal$message == 
                                                            'error.accessDenied') {
                                                                data.frame()
                                                        } else {
                                                                retVal
                                                        }
                                                } else {
                                                        retVal
                                                }
                                        }
                                }
                        } else {
                                data.frame()
                        }
                }
        }
}

# write data into PIA
writeItem <- function(app, repo_url, item) {
        headers <- defaultHeaders(app[['token']])
        data <- rjson::toJSON(item)
        response <- tryCatch(
                postForm(repo_url,
                         .opts=list(httpheader = headers,
                                    postfields = data)),
                error = function(e) { 
                        return(NA) })
        response
}

# update data in PIA
updateItem <- function(app, repo_url, item, id) {
        headers <- defaultHeaders(app[['token']])
        item <- c(item, c(id=as.numeric(id)))
        data <- rjson::toJSON(item)
        response <- tryCatch(
                postForm(repo_url,
                         .opts=list(httpheader = headers,
                                    postfields = data)),
                error = function(e) { return(NA) })
        response
}

# delete data in PIA
deleteItem <- function(app, repo_url, id){
        headers <- defaultHeaders(app[['token']])
        item_url <- paste0(repo_url, '/', id)
        response <- tryCatch(
                DELETE(item_url, 
                       add_headers(headers)),
                error = function(e) { return(NA) })
        response
}

# other helper functions ==================================
# remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
