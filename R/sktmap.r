#' R package for SK Telecom TMAP API
#' geocode  \code{\link{sktmap}}
#'
#' @author YD Hwang \email{yhwang@@g.skku.edu}
#' @param addr Target address character, which can be either old format (admin/lot number) or new format (road/building number)
#' @param app_key SK Telecom OPEN API perosonal key \url{http://tmapapi.sktelecom.com/main.html#web/guide/webGuide.sample1}
#' @return A tibble of 26 varialbles is returned. Its elements depend on how the address is provided into the function; when "new" address template is used (road - builidng number), `newLat` and `newLon` are returned, while the "old" address template is used `lat` and `lon` are returned.
#' @export

tmap_geocode <- function(addr, app_key) {
  url <- paste0("https://api2.sktelecom.com/tmap/geo/fullAddrGeo?version=1&format=json&fullAddr=",
                addr, "&appKey=", app_key)
  request <- GET(url, encode = "form")
  response <-  fromJSON(content(request, "text"))[[1]]
  out_tbl <- response$coordinate %>% as_tibble
  out_tbl
}


#' reverse geocode function \code{\link{sktmap}}
#'
#' @author YD Hwang \email{yhwang@@g.skku.edu}
#' @param lat Target latitude. Default value is for SKKU in Seoul, Korea.
#' @param lon Target longitude. Default value is for SKKU in Seoul, Korea.
#' @param app_key SK Telecom OPEN API perosonal key \url{http://tmapapi.sktelecom.com/main.html#web/guide/webGuide.sample1}
#' @return A tibble of 18 varialbles is returned. addressType=A10: gives the "full set" of the address of the triple of (1) administrative district name, (2) old address (admin-code or lot-number based), (3) new address (road-centric). When returned, fullAddress are provided after separated accordingly; admin_name, jibun_address, and road_address.
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
  out_tbl <- response %>%
    as_tibble %>%
    separate(fullAddress, sep =",", c("admin_name", "jibun_address", "road_address"))
  out_tbl
}

#' route search function \code{\link{sktmap}}
#'
#' @author YD Hwang \email{yhwang@@g.skku.edu}
#' @param lat_origin origin's latitude.
#' @param lon_origin origin's longitude.
#' @param lat_dest destination latitude.
#' @param lon_dest destination longitude.
#' @param app_key SK Telecom OPEN API perosonal key \url{http://tmapapi.sktelecom.com/main.html#web/guide/webGuide.sample1}
#' @return A Simple feature collection with 28 features and 16 fields. For more on simple feature in R, see sf package's manual.
#' @export

tmap_route_search <- function(lat_origin, lon_origin, lat_dest, lon_dest, app_key){
  request_body <- list(startX = lon_origin, startY = lat_origin, endX = lon_dest,
                  endY = lat_dest)
  request <- httr::POST("https://api2.sktelecom.com/tmap/routes?version=1",
                  body = request_body, add_headers(.headers = c(appKey = app_key)), encode = "form")
  response <- jsonlite::fromJSON(content(request, as = "text"))

  sf_obj <- st_read(content(request, as = "text"))
  sf_obj
}

