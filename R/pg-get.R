filter.convert <- function(filter, pg.filter.syntax){
	if(pg.filter.syntax){
		filter.exp <- filter
	} else {
		filter.exp <- filter
		filter.exp <- gsub("( |c*\\(|\\))", "",filter.exp)
		filter.exp <- gsub(">=","=gte.",filter.exp)
		filter.exp <- gsub("<=","=lte.",filter.exp)
		filter.exp <- gsub(">", "=gt.",filter.exp)
		filter.exp <- gsub("<", "=lt.",filter.exp)
		filter.exp <- gsub("==","=eq.",filter.exp)
		filter.exp <- gsub("!=","=neq.",filter.exp)
		filter.exp <- gsub("%*in%*","=in.",filter.exp)
	}

	filter <- paste(filter.exp, collapse = "&")
	return(filter)
}


#' @export
#' @title PostgREST GET
#' @description RESTful GET for PostgREST resources.
#' @aliases pg.get pg.GET pgGET pgget postgrest.get
#' @param domain The root of the PostgREST server. E.g.
#'   https://postgrest.herokuapp.com
#' @param table The table you are querying. Can be an empty string ("") for
#'   default root table listing.
#' @param select Character vector or comma separated string of columns to return
#'   (optional)
#' @param filter Character vector or comma separated string of filter
#'   expressions in R syntax or PostgREST syntax when pg.filter.syntax == TRUE.
#' @param limit Integer limiting the number of records returned from the API.
#' @param pg.filter.syntax Boolean indicating whether your filter expression is
#'   in PostgREST filter syntax or not. Defaults to FALSE using R expressions.
#' @param url.only Boolean when TRUE returns the URL built and does not call the
#'   API. Useful for debugging purposes. Defaults to FALSE.
#' @param encoding Character passed to \link[httr]{content}. Defaults to UTF-8
#' @param ... Extra parameters passed to \link[httr]{GET}. e.g. config =
#'   add_headers(custom_header="hello world")
#' @return data.frame of your response
#' @examples
#' domain <- "https://postgrest.herokuapp.com"
#'
#' pg.get(domain, "speakers", limit = 5,
#' 	filter = c("id >= 228", "featured == TRUE"))
#'
#' pg.get(domain, "speakers", filter = "id=eq.228",
#' 	pg.filter.syntax = TRUE)
#'
#' # All of the below give the same result
#' pg.get(domain, "speakers", filter = "id in (228,161)")
#' pg.get(domain, "speakers", filter = "id %in% (228,161)")
#' pg.get(domain, "speakers", filter = "id in c(228,161)")
#' pg.get(domain, "speakers", filter = "id %in% c(228,161)")
#'
#' # View table list
#' pg.get(domain, "")
#'
#' # View the URL that will be called for debugging purposes
#' pg.get(domain, "speakers", select = "id,name,bio",
#' 	limit = 2, filter = "id in (228,161),featured == TRUE",
#' 	url.only = TRUE)
#'

pg.get <- function(domain,
				   table,
				   select = "",
				   filter = "",
				   limit = "",
				   pg.filter.syntax = FALSE,
				   url.only = FALSE,
				   encoding = "UTF-8",
				   ...) {
	## select ##
	if(length(select) > 1){
		select <- paste(select, collapse = ",")
	}

	## filter ##
	if(length(filter) == 1){
		# negative lookahead to only split on commas not inside parens
		filter <- unlist(strsplit(filter, ",(?![^\\(]*\\))", perl=TRUE))
	}

	filter <- filter.convert(filter, pg.filter.syntax)

	## limit ##
	if(!limit==""){
		limit <- as.integer(limit)
		if(is.na(limit)){
			stop("limit is not an integer or cannot be coerced to one.")
		}
		limit <- paste0("limit=", limit)
	}

	## url build ##
	base.url <- paste0(domain, "/", table, "?")
	url <- paste(base.url, select, filter, limit, sep = "&")
	url <- gsub("(\\n|\\t|\\r)", "", url)

	if(url.only){
		return(url)
	}

	r <- httr::GET(url, ...)
	r.content <- httr::content(r, "text", encoding = encoding)

	## response handling ##
	if(grepl("json", r$headers$`content-type`)){
		dat <- jsonlite::fromJSON(r.content)
	} else if(grepl("csv", r$headers$`content-type`)){
		dat <- utils::read.csv(text = r.content)
	} else {
		warning(paste(r$headers$`content-type`,
					  "is not supported. Returning response object."))
		return(r)
	}

	return(dat)
}
