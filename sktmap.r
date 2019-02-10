library(httr)
library(jsonlite)
library(maptools)
library(tidyverse)
library(ggmap)
library(geojsonio)
library(sf)



## function definition

coord_to_addr <- function(lat = 37.587228, lon = 126.993115, app_key) {
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

# matchFlag
# 매칭 구분 코드입니다. <매칭 구분 코드> 
# M11 - 법정동 정매칭 - 법정동 코드 + 지번이 모두 일치 
# M12 - 법정동 부지번 근사 매칭 - 법정동 코드 + 지번의 주번이 같고 부번이 ±5 이내로 부번과 일치 
# M13 - 법정동 주지번 근사 매칭 - 법정동 코드 + 지번의 주번이 동일하지 않고 ±5 이내로 주번과 일치 
# M21 - 행정동 정매칭 - 행정동 코드 + 지번이 모두 일치 
# M22 - 행정동 부지번 근사 매칭 - 행정동 코드 + 지번의 주번이 같고 부번이 ±5 이내로 부번과 일치 
# M23 - 행정동 주지번 근사 매칭 - 행정동 코드 + 지번의 주번이 동일하지 않고 ±5 이내로 주번과 일치 
# M31 - 법정리 중심 매칭 - 읍/면/동/리의 중심 매칭 
# M32 - 행정동의 중심 매칭 
# M33 - 법정동의 중심 매칭 
# M41: - 법정동 코드 + 건물명칭이 일치(동일 법정동 내 동일 건물명이 없다는 전제) 
# M42 : - 법정동 코드 + 건물 동이 매칭

# newMatchFlag 
# N51 - 새(도로명) 주소 정매칭입니다. - 새(도로명) 주소 도로명이 일치하고 건물의 주번/부번이 모두 일치 
# N52 - 새(도로명) 주소 근사 매칭(2차 종속) - 2차 종속도로에서 주번이 같고 부번이 다름(직전 부번[좌/우 구분]의 끝 좌표를 반환) 
# N53 - 새(도로명) 주소 근사 매칭(1차 종속) - 1차 종속도로에서 주번이 같고 부번이 다름(직전 부번[좌/구 구분]의 끝 좌표를 반환) 
# N54 - 새(도로명) 주소 근사 매칭(0차 종속) - 0차 종속도로에서 주번이 같은 것이 없어서 가장 가까운 직전 주번[좌/우 구분]의 가장 끝 좌표 
# N55 - 새(도로명) 주소 길 중심 매칭 - 새(도로명) 주소 도로명은 일치하나 주번/부번의 직전 근사값이 없는 경우, 새(도로명) 주소 길 중심 좌표를 반환 
# N61 - 새(도로명) 주소 도로명이 틀리나 동일구 내 1개의 건물명과 일치하는 경우, 해당하는 건물 좌표를 반환 
# N62 - 새주소 도로명이 틀리나 동일구 내 1개의 건물명과 동명이 일치하는 경우, 해당하는 건물 좌표를 반환

addr_to_coor <- function(addr, app_key) {
  url <- paste0("https://api2.sktelecom.com/tmap/geo/fullAddrGeo?version=1&format=json&fullAddr=", 
                addr, "&appKey=", app_key)
  request <- httr::GET(url, encode = "form")
  response <-  jsonlite::fromJSON(content(request, "text"))[[1]]
  out_tbl <- response$coordinate %>% as_tibble
  out_tbl
}

route_search <- function(lat_origin, lon_origin, lat_dest, lon_dest, app_key){
  request_body <- list(startX = lon_origin, startY = lat_origin, endX = lon_dest, 
                  endY = lat_dest)
  request <- httr::POST("https://api2.sktelecom.com/tmap/routes?version=1", 
                  body = request_body, add_headers(.headers = c(appKey = app_key)), encode = "form")
  response <- jsonlite::fromJSON(content(request, as = "text"))
  
  topo <- content(request, as = "text") %>% geo2topo 
  geo <- topo2geo(topo)
  sf_obj <- geojson_list(geo) %>% geojson_sf
  sf_obj
}
  
## ----------------------------------
# 
# lat_origin <- 37.495894 
# lon_origin <- 126.943945
# lat_dest <- 37.554838 
# lon_dest <- 126.971733
# 
# reqBody <- list(startX = lon_origin, startY = lat_origin, endX = lon_dest, 
#                 endY = lat_dest)
# request <- POST("https://api2.sktelecom.com/tmap/routes?version=1", 
#                 body = reqBody, add_headers(.headers = c(appKey = app_key)), encode = "form")
# response <- fromJSON(content(request, as = "text"))
# # https://www.rdocumentation.org/packages/geojsonio/versions/0.6.0
# 
# response$features$geometry$coordinates # "routes" info
# response$features$properties # properites of each segment of the route
# 
# route_path <- filter(response$features$geometry, type == "LineString")  %>% 
#   select(coordinates)
# 
# 
# lat_origin <- 37.495894 
# lon_origin <- 126.943945
# lat_dest <- 37.554838 
# lon_dest <- 126.971733
# 
# reqBody <- list(startX = lon_origin, startY = lat_origin, endX = lon_dest, 
#                 endY = lat_dest)
# request <- POST("https://api2.sktelecom.com/tmap/routes?version=1", 
#                 body = reqBody, add_headers(.headers = c(appKey = app_key)), encode = "form")
# response <- fromJSON(content(request, as = "text"))
# # https://www.rdocumentation.org/packages/geojsonio/versions/0.6.0
# 
# response$features$geometry$coordinates # "routes" info
# response$features$properties # properites of each segment of the route
# 
# route_path <- filter(response$features$geometry, type == "LineString")  %>% 
#   select(coordinates)
# 
# 
# topo <- content(request, as = "text") %>% geo2topo 
# geo <- topo2geo(topo)
# sf_obj <- geojson_list(geo) %>% geojson_sf
# 
# 
# 
# leaflet() %>% setView(lng = 126.996949, lat = 37.55484, zoom = 12) %>% addTiles()
# 
# 
# props <- response$features$properties %>% filter(!is.na(time))
# 
# dumb <- list()
# for (k in seq_along(route_path[[1]])){
#   tbl_k <- route_path[[1]][k][[1]] %>% as_tibble %>% 
#     rename(long = V1, lat = V2)
#   tbl_k$time <- props$time[k]
#   tbl_k$distance <- props$distance[k]
#   dumb[[k]] <- tbl_k
# }
# dumb <- bind_rows(dumb)
# dumb <- dumb %>% mutate(speed = (distance/1000)/(time/60/60))
# 
# route_point <- filter(response$features$geometry, type == "Point") %>% 
#   select(coordinates)
# route_path <- do.call("rbind", route_path$coordinates) %>% 
#   as_tibble %>% 
#   rename(long = V1, lat = V2)
# route_point <- do.call("rbind", route_point$coordinates) %>% 
#   as_tibble %>% 
#   rename(long = V1, lat = V2)
# 
# ggplot() + 
#   geom_point(data = route_point, aes(x = long, y = lat), col = "red") +
#   geom_path(data = route_path, aes(x = long, y = lat)) 
# 
# test_map <- get_map(location = c(126.94271, 37.49538, 126.99868, 37.58703), 
#         source="stamen", maptype="toner-2011", crop=FALSE, zoom = 15) 
# (gmap_out <- ggmap(test_map) +
#   geom_path(data = dumb %>% mutate(speed = ifelse(speed>30, 30, speed)), aes(x = long, y = lat, col = speed), alpha = 0.7, lwd = 3) + 
#   geom_point(data = route_point, aes(x = long, y = lat), col = "white") +
#   scale_color_gradient(low = "red", high = "blue", name = "Speed (Km/h)"))
# ggsave(gmap_out, file = "speed_map.png", width = 10, height = 10)
# 
# 
# # geojsonR case.. probably not very useful
# library(geojsonR)
# temp <- FROM_GeoJson(content(request, as = "text")) 
# temp$features
# response$features
# 
# 
# 
# 
# 
# routes_to_eta <- function(origin_addr, dest_addr, app_key) {
#   origin <- addrTocoor(origin_addr, app_key)
#   dest <- addrTocoor(dest_addr, app_key)
#   time <- tmap(origin$lat, origin$lon, dest$lat, dest$lon, app_key)
#   return(time)
# }
# 
# 
# tmap <- function(lat_origin, lon_origin, lat_dest, lon_dest, app_key) {
#   # returns the total travel time by the unit of seconds
#   reqBody <- list(startX = lon_origin, startY = lat_origin, endX = lon_dest, 
#                   endY = lat_dest)
#   req <- POST("https://api2.sktelecom.com/tmap/routes?version=1", body = reqBody, 
#               add_headers(.headers = c(appKey = app_key)), encode = "form")
#   res <- fromJSON(content(req, "text"))
#   
#   a <- res$features
#   res_list <- res$features
#   res_elem <- res_list[[1]]
#   res_elem_elem <- res_elem[[3]]
#   time <- res_elem_elem$totalTime  # unit: second(s)
#   
#   time
# }
# 
# 
# 
# 
# 
# 
# # coord_to_addr(37.587228, 126.993115, app_key)
