### Load necessary functions:
library(pdftools)
library(stringr)
library(dplyr)
library(rebus)

### Function to tidy TV ratings data in R:
get_ratings = function(date){
  # Set the name of the html link that contains the pdf file
  pdf_name = paste0("http://anythingkiss.com/pi_feedback_challenge/ratings/",format(as.Date(date),"%Y%m%d"),"_TVRatings.pdf")
  
  # Download the file, read in the file, and turn it into a vector of strings (then remove the file from computer)
  download.file(pdf_name,"ratings.pdf",quiet=TRUE)
  text <- pdf_text("ratings.pdf") %>% paste(collapse = " ") %>% 
    str_split("\\n",simplify=TRUE) %>% 
    t()
  
  text = text[str_detect(text,START %R% SPC %R% optional(SPC) %R% DGT)]
  file.remove("ratings.pdf")
  
  # Loop over each line, extract elements
  for(i in 1:length(text)){
    pieces_1 = unlist(str_match_all(text[i],SPC %R% optional(DGT) %R% optional(DGT) %R% DGT %R% or(or(":",DOT) %R% DGT %R% optional(DGT),SPC%R%SPC)))
    if(str_detect(text[i],"Game "%R%DGT)){
      pieces_1 = pieces_1[-which(pieces_1 %>% str_trim(side = "both") == str_match(text[i],"Game "%R%capture(DGT))[2])]}
    pieces_2 = unlist(str_match_all(text[i],or("Mon","Tue","Wed","Thu","Fri","Sat","Sun")%R%SPC))                     
    pieces_3 = unlist(str_match_all(text[i],optional("-") %R% DGT %R% optional(DGT) %R% optional(DGT) %R% "%"))
    
    pieces_4 = str_replace_all(text[i],or1(pieces_1),"") %>% 
      str_replace_all(or1(pieces_2),"") %>% 
      str_replace_all(or1(pieces_3),"") %>% 
      str_replace_all("%","") %>% 
      str_trim(side="both") %>%
      str_replace(" ","_") %>% 
      str_split("_") %>% 
      unlist()
    
    # Fill in missing values
    if(length(pieces_1)<9){pieces_1 = c(pieces_1,rep(NA,9-length(pieces_1)))}
    dat = c(pieces_4,pieces_2,pieces_1,pieces_3) %>% str_trim(side = "both")
    
    if(length(dat)==12){dat = c(dat,NA)}
    
    if(i == 1){data = dat}
    if(i != 1){data = rbind(data,dat)}
  }
  
  # Some more tidying, change variable names, add date of show, some logical variables
  data = data %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    select(network = V1, program = V2, weekday = V3, time = V8, P2_ranking = V4, A18_34_ranking = V5, 
           A18_49_ranking = V6, A25_54_ranking = V7,viewers_millions = V9,A18_34 = V10,
           A18_49 = V11, A25_54 = V12,P2_change = V13) %>% 
    mutate(start_of_week = date,
           date = ifelse(weekday == "Mon",as.Date(date),
                         ifelse(weekday == "Tue",as.Date(date)+1,
                                ifelse(weekday == "Wed",as.Date(date)+2,
                                       ifelse(weekday == "Thu",as.Date(date)+3,
                                              ifelse(weekday == "Fri",as.Date(date)+4,
                                                     ifelse(weekday == "Sat",as.Date(date)+5,as.Date(date)+6)))))),
           date = as.Date(date,origin = "1970-01-01"),
           full_date = as.POSIXct(paste(date,time,sep=" "))+12*3600,
           rerun = str_detect(program,"\\[R\\]"),
           premiere = str_detect(program, "\\(SP\\)"),
           season_finale = str_detect(program,"\\(SF\\)"),
           fall_finale = str_detect(program,"\\(FF\\)"),
           movie = str_detect(program,"Movie: "),
           program = str_replace(program,or("\\[R\\]","\\(SP\\)","\\(SF\\)","\\(FF\\)","Movie: "),"") %>% str_trim(side = "both")) %>% 
    filter(network %in% c("ABC","CBS","CW","FOX","NBC"))
  
  # Convert variables to numeric
  for(j in c(5:12)){data[,j] = as.numeric(data[,j])}
  return(data)
}

### Example:
latest_date = "2018-01-29"
get_ratings(latest_date)
