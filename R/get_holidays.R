library("tidyverse")

get_holidays <- function(year, country = "ru") {
  
  if(length(year) > 1L) return(purrr::map_df(year, 
                                             get_holidays, 
                                             country = country))
  
  stopifnot(is.numeric(year))
  stopifnot(year >= 2010 & year <= lubridate::year(lubridate::today()))
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_locale))
  
  if (tolower(country) != "ru") stop("Only ru country is supported.")
  
  tz <- "Europe/Moscow"
  locale <- "ru_RU.UTF-8"
  
  Sys.setlocale("LC_TIME", locale)
  
  server <- "http://mirkosmosa.ru"
  path <- paste("calendar", year, sep = "/")
  
  resp <- httr::GET(server, path = path)
  
  stopifnot(httr::status_code(resp) == 200)
  
  parsed <- httr::content(resp, "text") %>% 
    xml2::read_html()
  
  clndr <- parsed %>% 
    rvest::html_nodes(".holiday_calendar_work") %>% 
    `[[`(2) %>% 
    rvest::html_nodes("table") %>% 
    rvest::html_table() %>% 
    `[[`(1)
  
  if (!identical(colnames(clndr), 
                 c("", "Всего дней", "Раб. дни",
                   "Вых. дни", "Сокр. дни"))) 
    stop("Structure of calendar table changed for ", year)
  
  colnames(clndr) <- c("month", "total", "work", "holi", "short")
  
  clndr <- clndr %>% 
    as.tibble() %>% 
    filter(month %in% format(ISOdate(2004,1:12,1),"%B")) %>% 
    mutate(
      date = stringr::str_c(1, month, year, sep = " "),
      date = lubridate::dmy(date)
      ) %>% 
    select(date, work, holi, short) %>% 
    tidyr::gather(daytype, count, work:short) %>% 
    mutate(count = as.integer(count))
  
  clndr
  
}
