#' R package for SK Telecom TMAP API
#' geocode
#'
#' @author Youngdeok Hwang \email{yhwang@@g.skku.edu} and Jae-kwang Kim \email{jkim@@iastate.edu}
#' @param addr Target address character, which can be either old format (admin/lot number) or new format (road/building number)
#' @param app_key SK Telecom OPEN API perosonal key from [TMAP webpage.](http://tmapapi.sktelecom.com/main.html#web/guide/webGuide.sample1)
#' @return A tibble of 26 varialbles is returned. Its elements depend on how the address is provided into the function.
#' @details
#' This function is *not* to be used for a lazy search; for a POI type search, change the code by yourself.
#' * (`newLat`, `newLon`) is returned When *new* address template is used (road - builidng number).
#' * (`lat`, `lon`) is returned When *old* address template is used (admin-lot number)
#' @md
#' @export

tmap_geocode <- function(addr, app_key) {
  # a part for query format is needed here
  url <- paste0("https://api2.sktelecom.com/tmap/geo/fullAddrGeo?version=1&format=json&fullAddr=",
                addr, "&appKey=", app_key)
  request <- GET(url, encode = "form")
  response <-  fromJSON(content(request, "text"))[[1]]
  out_tbl <- response$coordinate %>% as_tibble
  out_tbl
}


#' reverse geocode function
#'
#' @author Youngdeok Hwang \email{yhwang@@g.skku.edu} and Jae-kwang Kim \email{jkim@@iastate.edu}
#' @param lat Target latitude. Default value is for SKKU in Seoul, Korea.
#' @param lon Target longitude. Default value is for SKKU in Seoul, Korea.
#' @param app_key SK Telecom OPEN API perosonal key from [TMAP webpage.](http://tmapapi.sktelecom.com/main.html#web/guide/webGuide.sample1)
#' @return A tibble of 18 varialbles is returned.
#' @details
#' `addressType=A10` in the API gives the "full set" of the address; this is one of the options in TMAP API. In the package A10 is set as default. For the other options, check  [TMAP webpage.](http://tmapapi.sktelecom.com/main.html#web/guide/webGuide.sample1). This option provides the triple of:
#' 1. administrative district name (e.g., Sajikdong)
#' 1. old address (admin-code and lot-number based)
#' 1. new address (road-centric)
#' When returned, fullAddress are provided after separated accordingly; admin_name, jibun_address, and road_address.
#' @md
#' @export

tmap_rev_geocode <- function(lat = 37.587228, lon = 126.993115, app_key) {
  url <- paste0("https://api2.sktelecom.com/tmap/geo/reversegeocoding?version=1",
                "&lat=", lat,
                "&lon=", lon,
                "&addressType=A10",
                "&appKey=", app_key)
  request <- httr::GET(url, encode = "form")
  response <- jsonlite::fromJSON(content(request, "text"))[[1]]

  # addressType=A10: gives the "full set" of the address of the triple of
  # 1: administrative district name
  # 2: old address (admin-code or lot-number based)
  # 3: new address (road-centric)
  # fullAddress should be separated accordingly.
  fullAddress <- response$fullAddress
  address_vec <- str_split(fullAddress, pattern = ",", simplify = TRUE)

  if (length(address_vec) == 3){
    # when everything is nice
    message("Address query seems fine.")
  } else if (length(address_vec)==2){
    if (str_count(response$roadName) == 0){
      # no road name; third tuple is NA
      address_vec_mod <- c(address_vec, "")
      message("!!No road-address is found.")
    } else if (str_count(response$legalDong) == 0){
      # no legal dong name; the second tuple should be NA. Haven't seen such a case though
      address_vec_mod <- c(address_vec[1], "", address_vec[2])
      message("!!No legal-dong name is found.")
    } else if (str_count(response$adminDong) == 0){
      # no admin dong name; the first tuple should be NA. Haven't seen such a case though
      address_vec_mod <- c("", address_vec)
      message("!!No admin-dong name is found.")
    } else {stop("Check your query again; found no matching address.")}
    response$fullAddress <- paste0(address_vec_mod, collapse = ",")
  } else  {stop("Check your lat/lon query again; found no matching address.")}
  out_tbl <- response %>%
    as_tibble %>%
    separate(fullAddress, sep =",", c("admin_name", "jibun_address", "road_address"))
  out_tbl
}

#' route search function
#'
#' @author Youngdeok Hwang \email{yhwang@@g.skku.edu} and Jae-kwang Kim \email{jkim@@iastate.edu}
#' @param lat_origin origin's latitude.
#' @param lon_origin origin's longitude.
#' @param lat_dest destination latitude.
#' @param lon_dest destination longitude.
#' @param app_key SK Telecom OPEN API perosonal key from [TMAP webpage.](http://tmapapi.sktelecom.com/main.html#web/guide/webGuide.sample1)
#' @return A Simple feature collection with 28 features and 16 fields. For more on simple feature in R, see [sf package.](https://cran.r-project.org/web/packages/sf/index.html)
#' @md
#' @export

tmap_route_search <- function(lat_origin, lon_origin, lat_dest, lon_dest, app_key){
  request_body <- list(startX = lon_origin, startY = lat_origin, endX = lon_dest,
                  endY = lat_dest)
  request <- httr::POST("https://api2.sktelecom.com/tmap/routes?version=1",
                  body = request_body, add_headers(.headers = c(appKey = app_key)), encode = "form")
  response <- jsonlite::fromJSON(content(request, as = "text"))

  sf_obj <- st_read(content(request, as = "text"), quiet = TRUE) # silenced
  sf_obj
}

