#' Tract Sample
#'
#' This function gets a sample of Sex-Age-Race.
#' @param geoid Tract GEOID, n Sample size
#' #' @param n Sample size
#' @keywords 
#' @export
#' @examples
#' tract_sample()
tract_sample <- function(geoid, n){
  stopifnot(is.character(geoid) == TRUE)
  
  # get state and county id's
  stfips <- stringr::str_sub(geoid, 1, 2)
  cntyfips <- stringr::str_sub(geoid, 3, 5)
  
  # get table numbers
  tlist <- paste0("B01001", LETTERS[1:9])
  ids <- str_pad(c(7:16, 22:31), width = 3, side = "left", pad = "0")
  tset <- expand.grid(tlist, ids)
  tget <- paste0(tset$Var1, "_", tset$Var2)
  
  acsfun <- function(x){get_acs(geography = "tract", 
                                table = x, 
                                state = stfips, 
                                county = cntyfips, 
                                year = 2017, 
                                survey = "acs5",
                                cache_table = T)}
  acstest <- map(tlist, acsfun)
  acstest <- map(acstest, function(x){x %>% filter(GEOID == geoid)})
  acstest <- map(acstest, function(x){x %>% filter(variable %in% tget)})
  SAR <- bind_rows(acstest)
  sampdf <- as.data.frame(sample(SAR$variable, n, replace = T, prob = SAR$estimate))
  names(sampdf) <- c("tbl_grp")
  return(sampdf)
}

#' Tract Education
#'
#' This function gets Education by Age.
#' @param geoid Tract GEOID
#' @keywords 
#' @export
#' @examples
#' tract_ed()
tract_ed <- function(geoid){
  stopifnot(is.character(geoid) == TRUE)
  # get state adn county id's
  stfips <- stringr::str_sub(geoid, 1, 2)
  cntyfips <- stringr::str_sub(geoid, 3, 5)
  ids <- str_pad(c(4:10, 12: 18, 20:26, 28:34, 36:42, 45:51, 53:59, 61:67, 69:75, 77:83 ), width = 3, side = "left", pad = "0")
  tlist <- paste0("B15001_", ids)
  acstest <- get_acs(geography = "tract", 
                     table = "B15001", 
                     state = stfips, 
                     county = cntyfips, 
                     year = 2017, 
                     survey = "acs5",
                     cache_table = T)
  # acstest <- map(tlist, acsfun)
  acstest <-  acstest %>% filter(GEOID == geoid)
  acstest <-  acstest %>% filter(variable %in% tlist)
  return(acstest)
}

#' Tract Income
#'
#' This function gets income by Race and Age.
#' @param geoid Tract GEOID
#' @keywords 
#' @export
#' @examples
#' tract_inc()
tract_inc <- function(geoid){
  stopifnot(is.character(geoid) == TRUE)
  # get state adn county id's
  stfips <- stringr::str_sub(geoid, 1, 2)
  cntyfips <- stringr::str_sub(geoid, 3, 5)
  # get table numbers
  tlist <- paste0("B19037", LETTERS[1:9])
  ids <- str_pad(c(3:18, 20:35, 37:52, 54:69), width = 3, side = "left", pad = "0")
  tset <- expand.grid(tlist, ids)
  tget <- paste0(tset$Var1, "_", tset$Var2)
  
  acsfun <- function(x){get_acs(geography = "tract", 
                                table = x, 
                                state = stfips, 
                                county = cntyfips, 
                                year = 2017, 
                                survey = "acs5",
                                cache_table = T)}
  acstest <- map(tlist, acsfun)
  acstest <- map(acstest, function(x){x %>% filter(GEOID == geoid)})
  acstest <- map(acstest, function(x){x %>% filter(variable %in% tget)})
  SAR <- bind_rows(acstest)
  return(SAR)
}