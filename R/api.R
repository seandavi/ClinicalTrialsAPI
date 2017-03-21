.api_version = 'v1'
.api_base    = 'https://clinicaltrialsapi.cancer.gov'
.additional_fields = c('size','from','_fulltext','include','exclude')

#' @importFrom httr http_type content http_error status_code
.ctapi_check_response <- function(resp) {
  if (httr::http_type(resp) != "application/json") {
    stop(sprintf("API did not return json, status code: %d",httr::status_code(resp)), call. = FALSE)
  }
  parsed = httr::content(resp)
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Clinical Trials API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp)
      ),
      call. = FALSE
    )
  }
  return(parsed)
}

#' @importFrom httr GET modify_url
.ctapi_get = function(path, query = NULL) {
  path = paste(.api_version,path,sep="/")
  url = httr::modify_url(.api_base,path = path,query = query)
  if(getOption('ctapi.verbose',FALSE)) message(url)
  resp = httr::GET(url)
  return(.ctapi_check_response(resp))
}

#' @importFrom httr POST modify_url
.ctapi_post = function(path, query = NULL) {
  path = paste(.api_version,path,sep="/")
  resp = httr::POST(httr::modify_url(.api_base,path=path),body = query)
  return(.ctapi_check_response(resp))
}


#' Return available API fields and data types
#'
#' This function simply returns a named character
#' vector with all available fields for searching
#' and returning. It is meant to inform building
#' queries.
#'
#' @references \url{https://clinicaltrialsapi.cancer.gov/}
#'
#' @export
fields <- function() {
  res = Filter(function(x) x != 'nested',unlist(.ctapi_get(path=paste('clinical-trial.json'))))
  names(res) = sub('\\.type$','',names(res))
  names(res) = gsub('\\.properties','',names(res))
  return(
    structure({
      res
    },class=c('ctapi_fields',class(res))
    )
  )
  return(res)
}

#' Term mapping api (for auto-complete, for example)
#'
#' This API endpoint simply maps a search term
#' to available terms in the search index.
#'
#' @param term character(1) search term
#' @param term_type character(1) term_type. I am not
#'     certain what the available term_types are as of
#'     now.
#' @return a \code{data.frame} with columns:
#'   \itemize{
#'   \item{term_key}
#'   \item{item}
#'   \item{term_type}
#'   \item{count}
#'   \item{count_normalized}
#'   \item{codes}
#'   \item{score}
#'   }
#'
#' @references \url{https://clinicaltrialsapi.cancer.gov/}
#'
#' @examples
#' lookup_term('TP53')
#' lookup_term('panc')
#'
#' @export
lookup_term <- function(term,term_type=NULL) {
  parsed = .ctapi_get('terms',query=list(term=term,term_type=term_type))
  if(parsed$total==0) {
    return(data.frame())
  }
  terms = parsed$terms
  resdf = data.frame(
    term_key     = terms %>% purrr::map_chr('term_key'),
    term         = terms %>% purrr::map_chr('term'),
    term_type    = terms %>% purrr::map_chr('term_type'),
    count        = terms %>% purrr::map_int('count'),
    count_normalized = terms %>% purrr::map_dbl('count_normalized'),
    codes        = terms %>% purrr::map('codes') %>% purrr::flatten() %>% purrr::flatten_chr(),
    score        = terms %>% purrr::map_dbl('score')
  )
  resdf
}

#' Search Clinical Trials using the REST API
#'
#' Filters all clinical trials based upon supplied filter params.
#'
#' @details
#'
#' Filter params may be any of the fields in the schema as well as any of the following params...
#' \itemize{
#' \item{size: limit the amount of results a supplied amount (default is 10, max is 50)}
#' \item{from: start the results from a supplied starting point (default is 0)}
#' \item{include: include only the supplied filter param fields in all results (useful if you want to minimize the payload returned)}
#' \item{exclude: exclude the supplied filter param fields from all results (useful if you want most of the payload returned with the exception of a few fields)}
#' \item{fulltext: filter results by examining a variety of text-based fields (including the trial title, description, treatments, etc)}
#' }
#' Note that string field values are not case sensitive (must otherwise must match exactly).
#'
#' For field params which are filtering as ranges (date and long types), please supply _gte or _lte
#' to the end of the field param (depending on if you are filtering on greater than or equal (gte),
#' less than or equal (lte), or both): \code{<field_param>_gte=<field_value_from>},
#' \code{<field_param>_lte=<field_value_to>}
#'
#' \code{record_verification_date_gte="2016-08-25"}
#'
#' For field params which are geolocation coordinates (geo_point), please supply the
#' following to the end of the field param:
#' \itemize{
#' \item{\code{_lat} - The latitude in decimal degrees and plus/minus.}
#' \item{\code{_lon} - The longitude in decimal degrees and plus/minus.}
#' \item{\code{_dist} - The radius to search within. }
#' }
#'
#' Format must be an integer followed by a unit defined as:
#' \itemize{
#' \item{mi - miles (for example \code{"2mi"})}
#' \item{km - kilometer (for example \code{"5km")}}
#' }
#'
#' If you are crafting more complicated queries, it might be best to use the POST endpoint of the same name.
#'
#' @references \url{https://clinicaltrialsapi.cancer.gov/}
#'
#' @examples
#' res = ct_search(eligibility.structured.gender="female",
#'        include="nct_id")
#' str(res)
#' res = ct_search(record_verification_date_gte="2016-08-25")
#' res$count
#' res = ct_search(sites.org_coordinates_lat=39.1292,
#'              sites.org_coordinates_lon=-77.2953,
#'              sites.org_coordinates_dist="100mi")
#'
#' @export
ct_search <- function(...) {
  tmp = list(...)
  fieldnames = names(fields())
  available_fields = c(.additional_fields, fieldnames,
                       outer(fieldnames,c('_lte','_gte','_lat','_lon','_dist'),paste0))
  stopifnot(all(names(tmp) %in% available_fields))
  stopifnot(all(sapply(tmp,length)==1))
  res = .ctapi_get('clinical-trials',query=tmp)
  return(structure(
    list(count=res$total,
         trials = res$trials),
    class = c('ctapi_results','list')
  ))
}
