# last update: 2016-05-14
 
# Accessing a Repo ========================================
defaultHeaders <- function(token) {
        c('Accept'        = '*/*',
          'Content-Type'  = 'application/json',
          'Authorization' = paste('Bearer', token))
}

itemsUrl <- function(url, app_key) {
        paste0(url, '/api/repos/', app_key, '/items')
}

getToken <- function(url, app_key, app_secret) {
        url_auth <- paste0(url, '/oauth/token')
        response <- tryCatch(
                postForm(url_auth,
                         client_id=app_key,
                         client_secret=app_secret,
                         grant_type='client_credentials'),
                error = function(e) { return(NA) })
        if (is.na(response)) {
                return(NA)
        } else {
                return(fromJSON(response[1])$access_token)
        }
}

getRepo <- function(url, key, secret) {
        c('url'        = url,
          'app_key'    = key,
          'app_secret' = secret,
          'token'      = getToken(url, 
                                  key, 
                                  secret))
}

writeRecord <- function(repo, url, record) {
        headers <- defaultHeaders(repo[['token']])
        data <- gsub("^\\[|\\]$", '', 
                     toJSON(record, auto_unbox = TRUE))
        response <- tryCatch(
                postForm(url,
                         .opts=list(httpheader = headers,
                                    postfields = data)),
                error = function(e) { return(NA) })
        response
}

updateRecord <- function(repo, url, record, id) {
        headers <- defaultHeaders(repo[['token']])
        record$id <- as.numeric(id)
        data <- gsub("^\\[|\\]$", '', 
                     toJSON(record, auto_unbox = TRUE))
        response <- tryCatch(
                postForm(url,
                         .opts=list(httpheader = headers,
                                    postfields = data)),
                error = function(e) { return(NA) })
        response
}

deleteRecord <- function(repo, url, id){
        headers <- defaultHeaders(repo[['token']])
        url <- paste0(url, '/', id)
        response <- tryCatch(
                DELETE(url, add_headers(headers)),
                error = function(e) { return(NA) })
        response
}

readItems <- function(repo, url) {
        if (length(repo) == 0) {
                data.frame()
        } else {
                headers <- defaultHeaders(repo[['token']])
                url_data <- paste0(url, '?size=2000')
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
                                                        if (retVal$message == 'error.accessDenied') {
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

# other helper functions ==================================
createDigest <- function(data, fields){
        if (nrow(data)>0) {
                data <- unite_(data, 'merged', 
                               fields, 
                               remove=FALSE)
                data$digest <- sapply(data$merged, digest)
                data[, c(fields,  'digest')]
        } else {
                data.frame()
        }
}

validEmail <- function(email){
        emailPtrn <- "^[\\w\\.-]+@([\\w\\-]+\\.)+[A-Za-z]{2,4}$"
        if (any(grep(emailPtrn, email, perl = TRUE))) {
                TRUE
        } else {
                FALSE
        }
}

combineData <- function(dat1, dat2){
        data <- data.frame()
        if(nrow(dat1) == 0) {
                data <- dat2
        } else {
                if(nrow(dat2) == 0){
                        data <- dat1
                } else {
                        data <- merge(dat1[, !names(dat1) %in% c('id')], 
                                      dat2[, !names(dat2) %in% c('id')],
                                      by='date', all=TRUE)
                }
        }
        data
}

