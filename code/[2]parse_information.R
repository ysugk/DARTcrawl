library(tidyverse)
library(rvest)

prospectus_url_parameter <- read_csv("temp/prospectus_url_parameter.csv")

# Assume that you are interested in the section ``Ⅰ. 모집 또는 매출에 관한 일반사항.''
# Then, you can construct the url addresses for this section as below.
targeted_url <- prospectus_url_parameter %>%
  filter(section == "Ⅰ. 모집 또는 매출에 관한 일반사항") %>%
  mutate(url = paste0("http://dart.fss.or.kr/report/viewer.do?",
                      "rcpNo=", rcpNo, "&",
                      "dcmNo=", dcmNo, "&",
                      "eleId=", eleId, "&",
                      "offset=", offset, "&",
                      "length=", length, "&",
                      "dtd=", dtd))

# First, download HTML files.
html_download <- function(id, url){
  Sys.sleep(2)
  print(paste0("This function is now working on firm ", id))
  
  read_html(url) %>%
    write_html(file = paste0("temp/HTMLfiles/", id, ".html"))
}

walk2(targeted_url$id, targeted_url$url, html_download)
# Check the directory `HTMLfiles`.
# Using these HTML files, you can do the analysis what you want.
# For example, to extract setences
HTMLfiles <- dir("temp/HTMLfiles/")

document_tokenize <- function(html){
  path <- paste0("temp/HTMLfiles/", html)
  
  sentences <- read_html(path) %>%
    html_nodes("p") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("\\s+", " ") %>%
    str_subset("다.$") %>%
    str_split("(?<=다\\.)") %>%
    unlist() %>%
    str_replace_all("[:punct:]", "") %>%
    str_trim("both") %>%
    setdiff("")
  
  data_frame("id" = str_sub(html, 1, 6),
             "sentences" = sentences)
}

sentences <- map_dfr(HTMLfiles, document_tokenize)
print(sentences)
# Now you extarct sentences.

write_excel_csv(sentences, "output/sentences.csv")
