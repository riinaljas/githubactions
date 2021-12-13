

library(rvest)
library(tidyverse)
library(magrittr)
library(gsheet)
library(googlesheets4)
library(RSelenium)

## load last changed data from previous scrape data 

eelmine <- gsheet2tbl("https://docs.google.com/spreadsheets/d/14S3M0uWRZuxUR6XhSfpYjzPDh7f360if5f5R5kwhENI/edit#gid=1773489470", 2)


#https://web.archive.org/save/https://www.tallinn.ee/est/Tallinna-linna-ariuhingute-noukogude-ja-juhatuste-koosseisud

## SIHTASUTUSED  ---- 

myurl <- "https://www.tallinn.ee/est/Tallinna-linna-poolt-asutatud-sihtasutused"



viimatimuudetud <- myurl %>% 
  read_html() %>% 
  html_nodes("#page > div.body > div > div.def.content > div.autor_aeg") %>% 
  html_text()


## scrape by department/institution - as the webpage has built with just random tables this makes sure we get them all 

tabelitearv <- myurl %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_children() %>% 
  html_text() %>%
  length() %>% 
  as.numeric()



## start scraping, but only if the page has changed 

## write an empty data frame where to store data 
kokku= data.frame()

## iterate over the number of tables, plus one for differences 
for (x in c(1:(tabelitearv+1))){
  
  x <- x %>% as.numeric()
print(x)
  
## scrape only if the page has changed, but save the page to Wayback Machine just in case 

  if (viimatimuudetud %in% eelmine$viimatimuudetud == T)
  {
    
    rD <- rsDriver(browser="firefox", port=4546L, verbose=F)
    remDr <- rD[["client"]]
  
    
    remDr$navigate("https://web.archive.org/save/")
    Sys.sleep(10)
    remDr$findElement("css", "#web-save-url-input")$sendKeysToElement(list(myurl))
    Sys.sleep(10)
    remDr$findElement("css", ".web-save-button")$clickElement()
    Sys.sleep(10)
    
    next}

  if (x==max(c(1:(tabelitearv+1))))
    
    
  {
    asutus <- myurl %>%
      read_html() %>%
      html_nodes("#page > div.body > div > div.def.content") %>%
      html_nodes("h3") %>%
      extract2(1) %>%
      html_text()
    
    
    juhatuse_esimees <- myurl %>%
      read_html() %>%
      html_nodes("tr") %>%
      extract2(x) %>% 
      html_nodes("p > strong, td > strong") %>%
      html_text() %>% 
      head(1)
    if (length(juhatuse_esimees)==0)
    {
      juhatuse_esimees <- "pole"
    }
    
    liikmed <- myurl %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="page"]/div[2]/div/div[2]/p[27]/text()[2]') %>%
      html_text() %>%
      as.data.frame() %>%
      rename(nimi = 1) 
    
    
    # mailid <- myurl %>% 
    #   read_html() %>% 
    # html_nodes("#page > div.body > div > div.def.content > p:nth-child(48)") %>%
    #   html_children() %>% 
    #   html_text() %>% 
    #   as.data.frame() %>% 
    #   rename(mailid = 1) 
    
      
    
    
   # if (length(mailid) != nrow(liikmed)) 
    #{
      #length(mailid) <- nrow(liikmed)
    #}
    
    
    temp <- data.frame(asutus, 
                       liikmed
     #                  mailid
     ) %>% 
      mutate(positsioon = ifelse(str_detect(nimi, juhatuse_esimees),
                                 "esimees", 
                                 "liige"), 
             tabelinr = x
      )
    
    
    kokku <- bind_rows(kokku, temp)
    
    print(x)
  }
    

  asutus <- myurl %>%
    read_html() %>%
    html_nodes("#page > div.body > div > div.def.content") %>%
    html_nodes("h3") %>%
    extract2(1) %>%
    html_text()

  
  juhatuse_esimees <- myurl %>%
    read_html() %>%
    html_nodes("table") %>%
    extract2(x) %>% 
    html_nodes("p > strong, td > strong") %>%
    html_text() %>% 
    head(1)
  if (length(juhatuse_esimees)==0)
  {
    juhatuse_esimees <- "pole"
  }
  if (length(juhatuse_esimees)>1)
      {length(juhatuse_esimees) <- 1}

  
  liikmed <- myurl %>%
    read_html() %>%
    html_nodes("table") %>%
    extract2(x) %>%
    html_nodes('tr') %>%
    html_text() %>%
    as.data.frame() %>%
    rename(nimi = 1) 
  # %>%
  #   filter(!str_detect(nimi, "@"), 
  #         str_detect(nimi, "[a-z]"))
  # 
  if (nrow(liikmed)==0)
  {
    
    liikmed <- myurl %>%
      read_html() %>%
      html_nodes("table") %>%
      extract2(x) %>%
      html_nodes("tr") %>%
      html_text() %>%
      as.data.frame() %>%
      rename(nimi = 1) 
  }
   
  
  # 
  # mailid <-  myurl %>%
  #   read_html() %>%
  #   html_nodes("table") %>%
  #   extract2(x) %>%
  #   html_nodes("a") %>%
  #   html_text()
  #   

  #if (length(mailid) > nrow(liikmed)) 
  #{
   # length(mailid) <- nrow(liikmed)
 # }
  #if (length(mailid) < nrow(liikmed))
#  {length(mailid) <- nrow(liikmed)}
      
temp <- data.frame(asutus, 
                   liikmed
                  # mailid
                  ) %>% 
  mutate(positsioon = ifelse(str_detect(nimi, juhatuse_esimees),
                             "esimees", 
                             "liige"), 
         tabelinr = x
  ) %>% 
  mutate(viimatimuudetud = viimatimuudetud)


kokku <- bind_rows(kokku, temp)

print(x)

}

if (nrow(kokku)>0)
{kokku <- kokku %>% 
  mutate(tuup = "sihtasutus") %>% 
    mutate(noukogu_voi_juhatus = ifelse(tabelinr %% 2 == 0,
                               "nõukogu",
                         "juhatus")) }


# 
# kokku <- kokku %>% 
#   mutate(tuup = ifelse(tabelinr %% 2 == 0, 
#                              "nõukogu",
#                        "juhatus")) %>% 
#   mutate(tuup = case_when(tabelinr %in% c(13, 15)~"nõukogu", 
#                           tabelinr %in% c(14, 16)~"juhatus", 
#                           TRUE~tuup), 
#          asutus = case_when(tabelinr %in% c(1, 2)~"Aktsiaselts Tallinna Linnatransport", 
#                             tabelinr %in% c(3, 4)~"Aktsiaselts Tallinna Soojus", 
#                             tabelinr %in% c(5, 6)~"Tallinna Linnahalli AS",
#                             tabelinr %in% c(7, 8)~"Aktsiaselts Ida-Tallinna Keskhaigla",
#                             tabelinr %in% c(9, 10)~"Aktsiaselts Lääne-Tallinna Keskhaigla",
#                             tabelinr %in% c(11, 12)~"Aktsiaselts Tallinna Tööstuspargid",
#                             tabelinr %in% c(13, 16)~"Osaühing Tallinna Perearstikeskus",
#                             tabelinr %in% c(14, 15)~"Aktsiaselts Tallinna Jäätmete Taaskasutuskeskus", 
#                             TRUE~asutus)
#   ) %>% 
#   arrange(asutus) %>% 
#   select(-tabelinr) %>% 
#   mutate(viimatimuudetud= viimatimuudetud)
         
         

##ÄRIÜHINGUD-----


myurl <- "https://www.tallinn.ee/est/Tallinna-linna-ariuhingute-noukogude-ja-juhatuste-koosseisud"



viimatimuudetud <- myurl %>% 
  read_html() %>% 
  html_nodes("#page > div.body > div > div.def.content > div.autor_aeg") %>% 
  html_text()



tabelitearv <- myurl %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_children() %>% 
  html_text() %>%
  length() %>% 
  as.numeric()




##alusta scrape 

for (x in c(1:(tabelitearv+1))){
  
  x <- x %>% as.numeric()
  print(x)
  
  ## scrape only if the page has changed, but save the page to Wayback Machine just in case 
  
  if (viimatimuudetud %in% eelmine$viimatimuudetud == T)
  {
    
    rD <- rsDriver(browser="firefox", port=4547L, verbose=F)
    remDr <- rD[["client"]]
    
    
    remDr$navigate("https://web.archive.org/save/")
    Sys.sleep(10)
    remDr$findElement("css", "#web-save-url-input")$sendKeysToElement(list(myurl))
    Sys.sleep(10)
    remDr$findElement("css", ".web-save-button")$clickElement()
    Sys.sleep(10)
    
    next}
  
  
  if (x==max(c(1:(tabelitearv+1))))
    
    
  {
    asutus <- myurl %>%
      read_html() %>%
      html_nodes("#page > div.body > div > div.def.content") %>%
      html_nodes("h3") %>%
      extract2(1) %>%
      html_text()
    
    
    juhatuse_esimees <- myurl %>%
      read_html() %>%
      html_nodes("tr") %>%
      extract2(x) %>% 
      html_nodes("p > strong, td > strong") %>%
      html_text() %>% 
      head(1)
    if (length(juhatuse_esimees)==0)
    {
      juhatuse_esimees <- "pole"
    }
    
    liikmed <- myurl %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="page"]/div[2]/div/div[2]/p[27]/text()[2]') %>%
      html_text() %>%
      as.data.frame() %>%
      rename(nimi = 1) 
    
  
    
    temp <- data.frame(asutus, 
                       liikmed
                       #                  mailid
    ) %>% 
      mutate(positsioon = ifelse(str_detect(nimi, juhatuse_esimees),
                                 "esimees", 
                                 "liige"), 
             tabelinr = x
      )
    
    
    kokku <- bind_rows(kokku, temp)
    
    print(x)
  }
  
  
  asutus <- myurl %>%
    read_html() %>%
    html_nodes("#page > div.body > div > div.def.content") %>%
    html_nodes("h3") %>%
    extract2(1) %>%
    html_text()
  
  
  juhatuse_esimees <- myurl %>%
    read_html() %>%
    html_nodes("table") %>%
    extract2(x) %>% 
    html_nodes("p > strong, td > strong") %>%
    html_text() %>% 
    head(1)
  if (length(juhatuse_esimees)==0)
  {
    juhatuse_esimees <- "pole"
  }
  if (length(juhatuse_esimees)>1)
  {length(juhatuse_esimees) <- 1}
  
  
  liikmed <- myurl %>%
    read_html() %>%
    html_nodes("table") %>%
    extract2(x) %>%
    html_nodes('tr') %>%
    html_text() %>%
    as.data.frame() %>%
    rename(nimi = 1) 
  # %>%
  #   filter(!str_detect(nimi, "@"), 
  #         str_detect(nimi, "[a-z]"))
  # 
  if (nrow(liikmed)==0)
  {
    
    liikmed <- myurl %>%
      read_html() %>%
      html_nodes("table") %>%
      extract2(x) %>%
      html_nodes("tr") %>%
      html_text() %>%
      as.data.frame() %>%
      rename(nimi = 1) 
  }
  
  
  
  temp <- data.frame(asutus, 
                     liikmed
                     # mailid
  ) %>% 
    mutate(positsioon = ifelse(str_detect(nimi, juhatuse_esimees),
                               "esimees", 
                               "liige"), 
           tabelinr = x,
           viimatimuudetud = viimatimuudetud
    )
  
  
  kokku <- bind_rows(kokku, temp)
  
  print(x)
  
}

if (nrow(kokku > 0))

{kokku <- kokku %>% 
  mutate(tuup = ifelse(is.na(tuup)==T, 
                       "äriühing", 
                       tuup)) %>% 
  mutate(noukogu_voi_juhatus = ifelse(tabelinr %% 2 == 0,
                                      "nõukogu",
                                      "juhatus")) %>%
  mutate(noukogu_voi_juhatus = ifelse((tuup == "äriühing"& tabelinr %in% c(13, 15)), 
                                      "nõukogu",
                                     noukogu_voi_juhatus)) %>% 
  mutate(noukogu_voi_juhatus = ifelse((tuup == "äriühing"& tabelinr %in% c(14, 16)), 
                                      "juhatus",
                                     noukogu_voi_juhatus))
           

kokku <- kokku %>%
  mutate(nimi = nimi %>% 
           str_replace_all("[\r\n]", ";") %>%
           str_replace_all(" ", "\\-")) %>% 
    mutate(nimi = nimi %>% 
                    str_remove_all("\\s+")) %>% 
  mutate(nimi = nimi %>% 
           str_replace_all(";+", ";")) %>% 
  mutate(nimi = nimi %>% 
           str_replace_all(";-;", ";")) %>% 
  mutate(nimi = nimi %>% 
           str_remove_all("-;$|;$|-$| $|^;")) %>% 
  separate(nimi,c("nimi", "amet", "email"), ";") %>% 
  filter(str_detect(nimi, "[a-z]")) %>% 
  mutate(email = ifelse(str_detect(nimi, "@"),
                        str_extract(nimi, "(--=?).*@.*$"), 
                        email)) %>% 
  mutate(nimi = ifelse(str_detect(nimi, "@"),
                       str_remove(nimi, "(--=?).*@.*$"), 
                       nimi)) %>% 
 mutate(email = ifelse(str_detect(amet, "@"), 
                        amet, 
                        email)) %>% 
  mutate(amet = ifelse((amet == email & !is.na(amet) & !is.na(email)),
                       NA, 
                       amet))

myfunction <- function(x){
  x = x %>% 
    str_remove_all("--")
} 

kokku <- lapply(kokku, myfunction) %>% 
  as.data.frame()

myfunction <- function(x){
  x = x %>% 
    str_replace_all("-", " ") %>% 
    trimws()
} 

kokku <- lapply(kokku, myfunction) %>% 
  as.data.frame()

## append data to existing spreadsheet 

mydataurl <-("https://docs.google.com/spreadsheets/d/14S3M0uWRZuxUR6XhSfpYjzPDh7f360if5f5R5kwhENI/edit#gid=1399531370")      

gs4_browse(mydataurl)

sheet_append(mydataurl, kokku, sheet = 2)}
  
