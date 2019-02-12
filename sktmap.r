library(httr)
library(jsonlite)
library(maptools)
library(tidyverse)
library(ggmap)
library(geojsonio)
library(sf)



## function definition

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

tmap_geocode <- function(addr, app_key) {
  url <- paste0("https://api2.sktelecom.com/tmap/geo/fullAddrGeo?version=1&format=json&fullAddr=", 
                addr, "&appKey=", app_key)
  request <- httr::GET(url, encode = "form")
  response <-  jsonlite::fromJSON(content(request, "text"))[[1]]
  out_tbl <- response$coordinate %>% as_tibble
  out_tbl
}

tmap_route_search <- function(lat_origin, lon_origin, lat_dest, lon_dest, app_key){
  request_body <- list(startX = lon_origin, startY = lat_origin, endX = lon_dest, 
                  endY = lat_dest)
  request <- httr::POST("https://api2.sktelecom.com/tmap/routes?version=1", 
                  body = request_body, add_headers(.headers = c(appKey = app_key)), encode = "form")
  response <- jsonlite::fromJSON(content(request, as = "text")) 
  
  sf_obj <- st_read(content(request, as = "text"))
  sf_obj 
}

