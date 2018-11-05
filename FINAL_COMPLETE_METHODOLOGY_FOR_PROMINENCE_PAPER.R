##############################################################
## Code and methodology for Hansard Prominence paper
##                             
## This script has four main parts:
## 1) scraping data from Hansard
## 2) cleaning the data and curating it
## 3) machine learning to classify the prominence labels
## 4) generating analysis / results
##
## Timothy Graham | Australian National University | 9/3/2017                  
##############################################################

library(rvest)
require(magrittr)
library(stringr)
require(httr)

######### PART ONE - DATA COLLECTION #########

# we find the user agent to identify ourselves to the server, to help avoid connection errors
se <- html_session( "https://httpbin.org/user-agent" )
myAgent <- as.character(se$response$request$options$useragent)

# STEP 1 - import the full list of groups and ids
groupsData <- read.csv("Hansard_20102016.csv",head=T)
# delete the name with bad chars
groupsData <- groupsData[-1023,] # Federation des Alliances Fran\x8daises d'Australie

# let's just do UniqID=407 ("Australian Industry Group")
# toKeep <- which(groupsData$uniqid==407)
# groupsData <- groupsData[toKeep,]

# STEP 2 - initialise dataframe to store final results
# (we will simply append data to this dataframe as it is collected for each group)
FINAL_FIXED_HANSARD_DATA_DF <- data.frame(UniqID=NA,
                                          Name=NA,
                                          Title=NA,
                                          Database=NA,
                                          Date=NA,
                                          Source=NA,
                                          ParlNo=NA,
                                          Electorate=NA,
                                          Interjector=NA,
                                          Page=NA,
                                          Party=NA,
                                          Presenter=NA,
                                          Status=NA,
                                          QuestionNo=NA,
                                          Questioner=NA,
                                          Responder=NA,
                                          Speaker=NA,
                                          Stage=NA,
                                          Type=NA,
                                          Context=NA,
                                          SystemID=NA,
                                          Time=NA,
                                          Text=NA
)

FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-1,]

# STEP 3 - iterate through each group and collect data
for (groupToSearch in 1219:nrow(groupsData)) { # we run data collection for each group (STARTING FROM row 1219 where error occurred last run)
  # for (groupToSearch in 1:5) { # DEBUG
  
  # initialise the values for searching
  # groupToSearch <- 1 # debug !!!! 
  # skipBool <- F 
  currentUniqID <- groupsData$uniqid[groupToSearch]
  cat(paste("currentUniqID: "),currentUniqID,"\n")
  searchTerm <- groupsData$searchname[groupToSearch]
  searchTerm <- gsub(" ","+",searchTerm)
  searchTerm <- gsub("'","%27",searchTerm)
  searchTerm <- paste0("%22",searchTerm,"%22")
  cat(paste("searchTerm: "),searchTerm,"\n")
  
  # first we collect the SENATE data
  hansardQueryURL_SENATE <- paste0("http://www.aph.gov.au/Parliamentary_Business/Hansard/Search?ind=0&st=1&sr=1&q=",
                                   searchTerm,
                                   "&expand=False&drvH=0&pnuH=0&f=28%2F09%2F2010&to=05%2F05%2F2016&pi=0&pv=&chi=1&ps=10")
  
  # resultsPageList <- read_html(hansardQueryURL_SENATE) # we have to scrape first page once first to get the total number of results to loop through
  resultsPageList <- read_html(GET(hansardQueryURL_SENATE,user_agent(as.character(myAgent))))
  
  # resultsData <- resultsPageList %>% html_nodes("p.title") %>% html_children() %>% html_attrs()
  resultsData <- c() # initialise an empty vector (we are gonna collect everything and put it into here..)
  
  numOfResults <- resultsPageList %>% html_nodes("p#main_0_content_0_pTotalResults") %>% html_text()
  numOfResults <- as.numeric(gsub("[^0-9]", "", numOfResults))
  
  if (length(numOfResults)==0) { # if there are no search results returned
    print("Zero search results returned. Assigning 0 to numOfResults...")
    numOfResults <- 0
  }
  
  # collect the first page of results (further down we will collect page 2 to page n)
  results <- vector(mode="character",length=10)
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_0") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_0") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[1] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[1] <- tempResults[[1]]["href"]
  } else {
    results[1] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_1") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_1") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[2] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[2] <- tempResults[[1]]["href"]
  } else {
    results[2] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_2") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_2") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[3] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[3] <- tempResults[[1]]["href"]
  } else {
    results[3] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_3") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_3") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[4] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[4] <- tempResults[[1]]["href"]
  } else {
    results[4] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_4") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_4") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[5] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[5] <- tempResults[[1]]["href"]
  } else {
    results[5] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_5") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_5") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[6] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[6] <- tempResults[[1]]["href"]
  } else {
    results[6] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_6") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_6") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[7] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[7] <- tempResults[[1]]["href"]
  } else {
    results[7] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_7") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_7") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[8] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[8] <- tempResults[[1]]["href"]
  } else {
    results[8] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_8") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_8") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[9] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[9] <- tempResults[[1]]["href"]
  } else {
    results[9] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_9") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_9") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[10] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[10] <- tempResults[[1]]["href"]
  } else {
    results[10] <- tempResults2[[1]]["href"]
  }
  
  resultsData <- c(resultsData,results)
  
  if (numOfResults > 10) { # we don't bother searching on page 2, if there is no page 2 (i.e., only 10 or less results...)
    
    for (i in 2:ceiling(numOfResults / 10)) { # we skip the first page of results because we already have these...
      # if (counterVar > numOfResults) {next}
      hansardQueryURL <- paste0("http://www.aph.gov.au/Parliamentary_Business/Hansard/Search?page=",i,"&q=",
                                searchTerm,
                                "&ps=10&drt=0&drv=0&drvH=0&f=28%2f09%2f2010&to=05%2f05%2f2016&pnu=0&pnuH=0&pi=0&chi=1&coi=0&st=1&sr=1")
      
      resultsPageList <- read_html(GET(hansardQueryURL,user_agent(as.character(myAgent))))
      
      # tempResults <- resultsPageList %>% html_nodes("p.title") %>% html_children() %>% html_attrs()
      
      results <- vector(mode="character",length=10)
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_0") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_0") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[1] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[1] <- tempResults[[1]]["href"]
      } else {
        results[1] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_1") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_1") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[2] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[2] <- tempResults[[1]]["href"]
      } else {
        results[2] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_2") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_2") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[3] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[3] <- tempResults[[1]]["href"]
      } else {
        results[3] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_3") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_3") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[4] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[4] <- tempResults[[1]]["href"]
      } else {
        results[4] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_4") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_4") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[5] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[5] <- tempResults[[1]]["href"]
      } else {
        results[5] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_5") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_5") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[6] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[6] <- tempResults[[1]]["href"]
      } else {
        results[6] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_6") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_6") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[7] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[7] <- tempResults[[1]]["href"]
      } else {
        results[7] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_7") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_7") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[8] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[8] <- tempResults[[1]]["href"]
      } else {
        results[8] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_8") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_8") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[9] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[9] <- tempResults[[1]]["href"]
      } else {
        results[9] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_9") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_9") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[10] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[10] <- tempResults[[1]]["href"]
      } else {
        results[10] <- tempResults2[[1]]["href"]
      }
      
      resultsData <- c(resultsData,results)
      
    }
  }
  
  # we might end up with some elements of resultsData==""
  # which results from the final page of results not having exactly 10 results
  # so we clean away these elements
  toRem <- which(resultsData=="")
  if(length(toRem)>0) {
    resultsData <- resultsData[-toRem]
  }
  
  if(length(resultsData)>0) { # if there are no results then we skip this process...
    
    # now we need to follow the links in each of the elements of `results` (i.e, URLS)
    # and then scrape the text and other information out of those...
    
    # we will store it in a temporary dataframe 
    ALL_HANSARD_DATA_df <- data.frame(UniqID=c(rep(NA,length(resultsData))),
                                      Name=c(rep(NA,length(resultsData))),
                                      Title=c(rep(NA,length(resultsData))),
                                      Database=c(rep(NA,length(resultsData))),
                                      Date=c(rep(NA,length(resultsData))),
                                      Source=c(rep(NA,length(resultsData))),
                                      ParlNo=c(rep(NA,length(resultsData))),
                                      Electorate=c(rep(NA,length(resultsData))),
                                      Interjector=c(rep(NA,length(resultsData))),
                                      Page=c(rep(NA,length(resultsData))),
                                      Party=c(rep(NA,length(resultsData))),
                                      Presenter=c(rep(NA,length(resultsData))),
                                      Status=c(rep(NA,length(resultsData))),
                                      QuestionNo=c(rep(NA,length(resultsData))),
                                      Questioner=c(rep(NA,length(resultsData))),
                                      Responder=c(rep(NA,length(resultsData))),
                                      Speaker=c(rep(NA,length(resultsData))),
                                      Stage=c(rep(NA,length(resultsData))),
                                      Type=c(rep(NA,length(resultsData))),
                                      Context=c(rep(NA,length(resultsData))),
                                      SystemID=c(rep(NA,length(resultsData))),
                                      Time=c(rep(NA,length(resultsData))),
                                      Text=c(rep(NA,length(resultsData)))
    )
    
    cat(paste0("Length of resultsData for ",searchTerm," in SENATE is:", length(resultsData),"\n"))
    
    for (i in 1:length(resultsData)) {
      # results_mined <- read_html(results[i])
      results_mined <- read_html(GET(resultsData[i],user_agent(as.character(myAgent))))
      metaDataExtra_data <- results_mined %>% html_nodes("dd.mdValue") %>% html_text()
      results_mined_time <- results_mined %>% html_nodes("span.HPS-Time") %>% html_text()
      
      results_mined_time <- paste(results_mined_time,collapse=",") # if there are multiple time values for different speaker/questioner etc
      if (length(results_mined_time)==0) {results_mined_time <- NA}
      # results_mined_FullText <- results_mined %>% html_nodes("span.HPS-Normal") %>% html_text() # <-- NOT GETTING ALL TEXT
      results_mined_FullText <- results_mined %>% html_nodes("div#documentContentPanel") %>% html_text() # <-- REPLACEMENT CODE TEST
      results_mined_FullText <- paste(results_mined_FullText,collapse=" ")
      
      # update the final dataframe
      ALL_HANSARD_DATA_df$Title[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Title') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Time[i] <- results_mined_time
      ALL_HANSARD_DATA_df$Text[i] <- results_mined_FullText
      ALL_HANSARD_DATA_df$Name[i] <- searchTerm
      ALL_HANSARD_DATA_df$UniqID[i] <- currentUniqID
      ALL_HANSARD_DATA_df$Database[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Database') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Date[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Date') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Source[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Source') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$ParlNo[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Parl No.') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Electorate[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Electorate') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Interjector[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Interjector') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Page[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Page') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Party[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Party') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Status[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Status') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Questioner[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Questioner') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Speaker[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Speaker') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Stage[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Stage') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Type[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Type') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Context[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Context') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$SystemID[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('System Id') + dd.mdValue") %>% html_text() # DEBUG
      
      # if you want to sleep for random time between 1 and 5 seconds (TO AVOID GETTING BLACKLISTED BY SERVER!). This will differ depending where you are making server requests from (e.g. institutional IP blocks might be OK, but if you're on a cheap commercial server, then maybe you will have problems
      # Sys.sleep(round(runif(1, 0, 5),0))
      
    }
    
    # FINALLY we append the collected data to `FINAL_FIXED_HANSARD_DATA_DF`
    FINAL_FIXED_HANSARD_DATA_DF <- rbind(FINAL_FIXED_HANSARD_DATA_DF,ALL_HANSARD_DATA_df)
  }
  
  #####################################################################################################################################
  #### SECOND, we collect the HOUSE OF REPS data ######################################################################################
  
  hansardQueryURL_HOUSE <- paste0("http://www.aph.gov.au/Parliamentary_Business/Hansard/Search?ind=0&st=1&sr=1&q=",
                                  searchTerm,
                                  "&expand=False&drvH=0&pnuH=0&f=28%2F09%2F2010&to=05%2F05%2F2016&pi=0&pv=&chi=2&ps=10")
  
  # resultsPageList <- read_html(hansardQueryURL_HOUSE) # we have to scrape first page once first to get the total number of results to loop through
  resultsPageList <- read_html(GET(hansardQueryURL_HOUSE,user_agent(as.character(myAgent))))
  
  # resultsData <- resultsPageList %>% html_nodes("p.title") %>% html_children() %>% html_attrs()
  resultsData <- c() # initialise an empty vector (we are gonna collect everything and put it into here..)
  
  numOfResults <- resultsPageList %>% html_nodes("p#main_0_content_0_pTotalResults") %>% html_text()
  numOfResults <- as.numeric(gsub("[^0-9]", "", numOfResults))
  
  if (length(numOfResults)==0) { # if there are no search results returned
    print("Zero search results returned. Assigning 0 to numOfResults...")
    numOfResults <- 0
  }
  
  # collect the first page of results (further down we will collect page 2 to page n)
  results <- vector(mode="character",length=10)
  
  # counterVar <- 1 # to know when to stop
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_0") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_0") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[1] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[1] <- tempResults[[1]]["href"]
  } else {
    results[1] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_1") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_1") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[2] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[2] <- tempResults[[1]]["href"]
  } else {
    results[2] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_2") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_2") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[3] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[3] <- tempResults[[1]]["href"]
  } else {
    results[3] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_3") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_3") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[4] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[4] <- tempResults[[1]]["href"]
  } else {
    results[4] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_4") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_4") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[5] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[5] <- tempResults[[1]]["href"]
  } else {
    results[5] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_5") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_5") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[6] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[6] <- tempResults[[1]]["href"]
  } else {
    results[6] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_6") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_6") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[7] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[7] <- tempResults[[1]]["href"]
  } else {
    results[7] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_7") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_7") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[8] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[8] <- tempResults[[1]]["href"]
  } else {
    results[8] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_8") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_8") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[9] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[9] <- tempResults[[1]]["href"]
  } else {
    results[9] <- tempResults2[[1]]["href"]
  }
  
  tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_9") %>% html_attrs()
  tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_9") %>% html_attrs()
  if (length(tempResults)==0 & length(tempResults2)==0) {
    results[10] <- ""
  } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
    results[10] <- tempResults[[1]]["href"]
  } else {
    results[10] <- tempResults2[[1]]["href"]
  }
  
  resultsData <- c(resultsData,results)
  
  if (numOfResults > 10) { # we don't bother searching on page 2, if there is no page 2 (i.e., only 10 or less results...)
    
    for (i in 2:ceiling(numOfResults / 10)) { # we skip the first page of results because we already have these...
      # if (counterVar > numOfResults) {next}
      hansardQueryURL <- paste0("http://www.aph.gov.au/Parliamentary_Business/Hansard/Search?page=",i,"&q=",
                                searchTerm,
                                "&ps=10&drt=0&drv=0&drvH=0&f=28%2f09%2f2010&to=05%2f05%2f2016&pnu=0&pnuH=0&pi=0&chi=2&coi=0&st=1&sr=1")
      
      # resultsPageList <- read_html(hansardQueryURL)
      
      resultsPageList <- read_html(GET(hansardQueryURL,user_agent(as.character(myAgent))))
      
      # tempResults <- resultsPageList %>% html_nodes("p.title") %>% html_children() %>% html_attrs()
      
      # FIXED VERSION... (9/3/16) !
      results <- vector(mode="character",length=10)
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_0") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_0") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[1] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[1] <- tempResults[[1]]["href"]
      } else {
        results[1] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_1") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_1") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[2] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[2] <- tempResults[[1]]["href"]
      } else {
        results[2] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_2") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_2") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[3] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[3] <- tempResults[[1]]["href"]
      } else {
        results[3] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_3") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_3") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[4] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[4] <- tempResults[[1]]["href"]
      } else {
        results[4] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_4") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_4") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[5] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[5] <- tempResults[[1]]["href"]
      } else {
        results[5] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_5") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_5") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[6] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[6] <- tempResults[[1]]["href"]
      } else {
        results[6] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_6") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_6") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[7] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[7] <- tempResults[[1]]["href"]
      } else {
        results[7] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_7") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_7") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[8] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[8] <- tempResults[[1]]["href"]
      } else {
        results[8] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_8") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_8") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[9] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[9] <- tempResults[[1]]["href"]
      } else {
        results[9] <- tempResults2[[1]]["href"]
      }
      
      tempResults <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlTitle_9") %>% html_attrs()
      tempResults2 <- resultsPageList %>% html_nodes("a#main_0_content_0_lvResults_hlHtml_9") %>% html_attrs()
      if (length(tempResults)==0 & length(tempResults2)==0) {
        results[10] <- ""
      } else if (length(grep("http://parlinfo.aph.gov.au/parlInfo/search",tempResults[[1]][2]))>0) {
        results[10] <- tempResults[[1]]["href"]
      } else {
        results[10] <- tempResults2[[1]]["href"]
      }
      
      resultsData <- c(resultsData,results)
      
    }
  }
  
  # we might end up with some elements of resultsData==""
  # which results from the final page of results not having exactly 10 results
  # so we clean away these elements, if required.
  toRem <- which(resultsData=="")
  if(length(toRem)>0) {
    resultsData <- resultsData[-toRem]
  }
  
  cat(paste0("Length of resultsData for ",searchTerm," in HOUSE is: ", length(resultsData),"\n"))
  
  if(length(resultsData)>0) { # if we don't have any results, then we skip this process
    
    # now we need to follow the links in each of the elements of `results` (i.e, URLS)
    # and then scrape the text and other information out of those...
    
    # we will store it in a temporary dataframe 
    ALL_HANSARD_DATA_df <- data.frame(UniqID=c(rep(NA,length(resultsData))),
                                      Name=c(rep(NA,length(resultsData))),
                                      Title=c(rep(NA,length(resultsData))),
                                      Database=c(rep(NA,length(resultsData))),
                                      Date=c(rep(NA,length(resultsData))),
                                      Source=c(rep(NA,length(resultsData))),
                                      ParlNo=c(rep(NA,length(resultsData))),
                                      Electorate=c(rep(NA,length(resultsData))),
                                      Interjector=c(rep(NA,length(resultsData))),
                                      Page=c(rep(NA,length(resultsData))),
                                      Party=c(rep(NA,length(resultsData))),
                                      Presenter=c(rep(NA,length(resultsData))),
                                      Status=c(rep(NA,length(resultsData))),
                                      QuestionNo=c(rep(NA,length(resultsData))),
                                      Questioner=c(rep(NA,length(resultsData))),
                                      Responder=c(rep(NA,length(resultsData))),
                                      Speaker=c(rep(NA,length(resultsData))),
                                      Stage=c(rep(NA,length(resultsData))),
                                      Type=c(rep(NA,length(resultsData))),
                                      Context=c(rep(NA,length(resultsData))),
                                      SystemID=c(rep(NA,length(resultsData))),
                                      Time=c(rep(NA,length(resultsData))),
                                      Text=c(rep(NA,length(resultsData)))
    )
    
    for (i in 1:length(resultsData)) {
      # results_mined <- read_html(results[i])
      results_mined <- read_html(GET(resultsData[i],user_agent(as.character(myAgent))))
      # resultsExtractedData <- results_mined %>% html_nodes("p.mdItem") %>% html_text()
      # names(resultsExtractedData) <- results_mined %>% html_nodes("dt.mdLabel") %>% html_text()
      # metaDataExtra_data <- results_mined %>% html_nodes("#metadataExtra p.mdItem") %>% html_text()
      metaDataExtra_data <- results_mined %>% html_nodes("dd.mdValue") %>% html_text()
      # names(metaDataExtra_data) <- results_mined %>% html_nodes("#metadataExtra dt.mdLabel") %>% html_text()
      
      # resultsExtractedData <- resultsExtractedData[-2] # we don't want the 2nd element
      # results_mined_title <- paste0(resultsExtractedData[1],"_",resultsExtractedData[2],"_",resultsExtractedData[3])
      
      # tempVAR <- results_mined %>% html_nodes("div.metaPadding") %>% html_text()
      # tempVAR <- paste(tempVAR,collapse="")
      # tempVAR <- gsub("\n"," ",tempVAR)
      
      results_mined_time <- results_mined %>% html_nodes("span.HPS-Time") %>% html_text()
      
      results_mined_time <- paste(results_mined_time,collapse=",") # if there are multiple time values for different speaker/questioner etc
      if (length(results_mined_time)==0) {results_mined_time <- NA}
      # results_mined_FullText <- results_mined %>% html_nodes("span.HPS-Normal") %>% html_text() # <-- NOT GETTING ALL TEXT
      results_mined_FullText <- results_mined %>% html_nodes("div#documentContentPanel") %>% html_text() # <-- REPLACEMENT CODE TEST
      results_mined_FullText <- paste(results_mined_FullText,collapse=" ")
      
      # update the final dataframe
      ALL_HANSARD_DATA_df$Title[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Title') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Time[i] <- results_mined_time
      ALL_HANSARD_DATA_df$Text[i] <- results_mined_FullText
      ALL_HANSARD_DATA_df$Name[i] <- searchTerm
      ALL_HANSARD_DATA_df$UniqID[i] <- currentUniqID
      ALL_HANSARD_DATA_df$Database[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Database') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Date[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Date') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Source[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Source') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$ParlNo[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Parl No.') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Electorate[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Electorate') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Interjector[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Interjector') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Page[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Page') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Party[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Party') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Status[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Status') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Questioner[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Questioner') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Speaker[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Speaker') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Stage[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Stage') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Type[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Type') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$Context[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('Context') + dd.mdValue") %>% html_text() # DEBUG
      ALL_HANSARD_DATA_df$SystemID[i] <- html_nodes(results_mined, css = "dt.mdLabel:contains('System Id') + dd.mdValue") %>% html_text() # DEBUG
      
      # sleep for random time between 1 and 5 seconds (TO AVOID GETTING BLACKLISTED BY SERVER!)
      # Sys.sleep(round(runif(1, 0, 5),0))
      
    }
    # FINALLY we append the collected data to `FINAL_FIXED_HANSARD_DATA_DF`
    FINAL_FIXED_HANSARD_DATA_DF <- rbind(FINAL_FIXED_HANSARD_DATA_DF,ALL_HANSARD_DATA_df)
  }
  
}

# FINAL_FIXED_HANSARD_DATA_DF_BACKUP <- FINAL_FIXED_HANSARD_DATA_DF # !!!!!! BACKUP... !!!!!!!

# remove the three major parties from the results (they shouldn't be included)
# 376	Australian Greens
# 459	Australian Labor Party
# 1638	The Nationals
toRem <- which(FINAL_FIXED_HANSARD_DATA_DF$UniqID==376)
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toRem,]
toRem <- which(FINAL_FIXED_HANSARD_DATA_DF$UniqID==459)
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toRem,]
toRem <- which(FINAL_FIXED_HANSARD_DATA_DF$UniqID==1638)
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toRem,]

# we 'fix' the `name` column to revert it back to plain english
# e.g., "%22ActionAid+Australia%22" becomes "ActionAid Australia"
idMatches <- match(FINAL_FIXED_HANSARD_DATA_DF$UniqID,groupsData$uniqid)
namesFixed <- groupsData$searchname[idMatches]
FINAL_FIXED_HANSARD_DATA_DF$Name <- namesFixed

# we have some text that has a weird quotation marks character causing problems (e.g., alzheimer's australia)
FINAL_FIXED_HANSARD_DATA_DF$Text <- gsub("’","'",FINAL_FIXED_HANSARD_DATA_DF$Text)
FINAL_FIXED_HANSARD_DATA_DF$Text <- gsub("‘","'",FINAL_FIXED_HANSARD_DATA_DF$Text)

# we have one group that does not return search results, but the "&" char in its name causes it
# to pick up other search results that are incorrect. So we delete these rows.
toDel <- which(FINAL_FIXED_HANSARD_DATA_DF$Name=="Action for More Independence & Dignity in Accomodation")
if (length(toDel)>0){
  FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toDel,] # don't do this is `toDel` has length==0 !!!
}

# there are some rows where no text was found by the search engine (and indeed there is no text available in search results)
tempText <- FINAL_FIXED_HANSARD_DATA_DF$Text
tempText <- gsub("\\n","",tempText)
#tempText[160]
#tempText[161]
toDel <- which(tempText=="")
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toDel,]

# we create a new column `TextParagraph` to store the paragraph of text in which the group was mentioned
FINAL_FIXED_HANSARD_DATA_DF$TextParagraph <- NA

for (i in 1:length(FINAL_FIXED_HANSARD_DATA_DF$TextParagraph)) {
  tempText <- tolower(FINAL_FIXED_HANSARD_DATA_DF$Text[i])
  tempVar <- scan(text=tempText,sep='\n',what='character')
  tempName <- tolower(FINAL_FIXED_HANSARD_DATA_DF$Name[i]) # there are sometimes misspellings in text due to punctuation in names of groups
  tempName <- gsub("[[:punct:]]", "", tempName) # so we will also match on the name without punctuation
  tempName <- gsub("\\s+"," ",tempName) # remove extra whitespaces
  toMatch <- unique(c(tolower(FINAL_FIXED_HANSARD_DATA_DF$Name[i]),tempName))
  matchElem <- grep(paste(toMatch,collapse="|"), tempVar, value=T)
  FINAL_FIXED_HANSARD_DATA_DF$TextParagraph[i] <- paste(matchElem,collapse="\n")
}

rownames(FINAL_FIXED_HANSARD_DATA_DF) <- c(1:nrow(FINAL_FIXED_HANSARD_DATA_DF))

# we create a `Prominence` column for the dependent variable of interest
FINAL_FIXED_HANSARD_DATA_DF$Prominence <- 0

# some more cleaning to remove extra whitespace from text
FINAL_FIXED_HANSARD_DATA_DF$Text <- gsub("\\s+"," ",FINAL_FIXED_HANSARD_DATA_DF$Text)
FINAL_FIXED_HANSARD_DATA_DF$TextParagraph <- gsub("\\s+"," ",FINAL_FIXED_HANSARD_DATA_DF$TextParagraph)

# remove the 'Type' column
toDel <- which(colnames(FINAL_FIXED_HANSARD_DATA_DF)=="Type")
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[,-toDel]

# coerce some of the columns to factors
FINAL_FIXED_HANSARD_DATA_DF$Party <- as.factor(FINAL_FIXED_HANSARD_DATA_DF$Party)
FINAL_FIXED_HANSARD_DATA_DF$Source <- as.factor(FINAL_FIXED_HANSARD_DATA_DF$Source)
FINAL_FIXED_HANSARD_DATA_DF$ParlNo <- as.factor(FINAL_FIXED_HANSARD_DATA_DF$ParlNo)
FINAL_FIXED_HANSARD_DATA_DF$Electorate <- as.factor(FINAL_FIXED_HANSARD_DATA_DF$Electorate)
FINAL_FIXED_HANSARD_DATA_DF$Speaker <- as.factor(FINAL_FIXED_HANSARD_DATA_DF$Speaker)

# we have some empty factor levels due to no data returned, so make these NA

makeNA <- FINAL_FIXED_HANSARD_DATA_DF$Electorate[1] # get an example of the "empty" factor level

toChange <- which(FINAL_FIXED_HANSARD_DATA_DF$Electorate==makeNA)
length(toChange)
FINAL_FIXED_HANSARD_DATA_DF$Electorate[toChange] <- NA

toChange <- which(FINAL_FIXED_HANSARD_DATA_DF$Interjector==makeNA)
length(toChange)
FINAL_FIXED_HANSARD_DATA_DF$Interjector[toChange] <- NA

toChange <- which(FINAL_FIXED_HANSARD_DATA_DF$Questioner==makeNA)
length(toChange)
FINAL_FIXED_HANSARD_DATA_DF$Questioner[toChange] <- NA

makeNA <- FINAL_FIXED_HANSARD_DATA_DF$Speaker[4] # get an example of the "empty" factor level (the 4th row has an example)
toChange <- which(FINAL_FIXED_HANSARD_DATA_DF$Speaker==makeNA)
length(toChange)
FINAL_FIXED_HANSARD_DATA_DF$Speaker[toChange] <- NA

toChange <- which(FINAL_FIXED_HANSARD_DATA_DF$Time=="") # time variable is not a factor - it is just a string
length(toChange)
FINAL_FIXED_HANSARD_DATA_DF$Time[toChange] <- NA

toChange <- which(FINAL_FIXED_HANSARD_DATA_DF$Stage==makeNA)
length(toChange)
FINAL_FIXED_HANSARD_DATA_DF$Stage[toChange] <- NA

# character problems with the Text column...
QuickCleanFunc <- function(data) {
  data <- tolower(data) # there are sometimes misspellings in text due to punctuation in names of groups
  data <- gsub("[[:punct:]]", "", data) # so we will also match on the name without punctuation
  data <- gsub("\\s+"," ",data) # remove extra whitespaces
  return(data)
}

Sys.setlocale('LC_ALL','C') # we have to do this to avoid error on the weird characters in french name
textBackup <- FINAL_FIXED_HANSARD_DATA_DF$Text
FINAL_FIXED_HANSARD_DATA_DF$Text <- QuickCleanFunc(FINAL_FIXED_HANSARD_DATA_DF$Text)
FINAL_FIXED_HANSARD_DATA_DF$Text <- stringr::str_replace_all(FINAL_FIXED_HANSARD_DATA_DF$Text,"[^a-zA-Z\\s]", " ")

#### TEMPORARY MEASURE - DELETE THE FULL TEXT COLUMN - OTHERWISE THE CSV IS BROKEN IN EXCEL...
toDel <- which(colnames(FINAL_FIXED_HANSARD_DATA_DF)=="Text")
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[,-toDel]

# WRITE TO FILE
write.csv(FINAL_FIXED_HANSARD_DATA_DF,"./NEW_FIXED_DATA_MARCH_2016/FINAL_COMPLETE_HANSARD_DATASET_INTEREST_GROUP_RAW_MENTIONS.csv",row.names = F,fileEncoding = "UTF-8")

########################################################################################

###### PART TWO - CLEANING AND CURATION OF DATA

FinalHansard_type <- read.csv("FinalHansard_type.csv")

toRemove <- c("Auditor-General's Reports",
              "Answers to Questions on Notice",
              "BUDGET",
              "Censure Motion",
              "Condolences",
              "Delegation Reports",
              "DISTINGUISHED VISITORS",
              "Governor-General's Speech",
              "Miscellaneous",
              "PARLIAMENTARY REPRESENTATION",
              "PERSONAL EXPLANATIONS",
              "PETITIONS",
              "QUESTIONS TO THE SPEAKER",
              "REGULATIONS AND DETERMINATIONS",
              "STATEMENTS ON INDULGENCE")

toDel <- match(FINAL_FIXED_HANSARD_DATA_DF$Context,toRemove)
toDelfinal <- which(!is.na(toDel))

FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toDelfinal,]
rownames(FINAL_FIXED_HANSARD_DATA_DF) <- c(1:nrow(FINAL_FIXED_HANSARD_DATA_DF))

### removing dummy values

toDel <- unique(FinalHansard_type$uniqid[which(FinalHansard_type$citizen_dummy==999)])
toRemove <- match(FINAL_FIXED_HANSARD_DATA_DF$UniqID,toDel)
toDelfinal <- which(!is.na(toRemove))
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toDelfinal,]

### need to merge the additional data types into the dataframe

tempColnames <- colnames(FinalHansard_type)[3:22]
FINAL_FIXED_HANSARD_DATA_DF$type <- NA
FINAL_FIXED_HANSARD_DATA_DF$staff_com <- NA
FINAL_FIXED_HANSARD_DATA_DF$formed_combined2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$stat2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$isic1_1 <- NA
FINAL_FIXED_HANSARD_DATA_DF$isic1_2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$isic2_1 <- NA
FINAL_FIXED_HANSARD_DATA_DF$isic2_2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$isic3_1 <- NA
FINAL_FIXED_HANSARD_DATA_DF$isic3_2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$cat1_1 <- NA
FINAL_FIXED_HANSARD_DATA_DF$cat1_2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$cat2_1 <- NA
FINAL_FIXED_HANSARD_DATA_DF$cat2_2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$cat3_1 <- NA
FINAL_FIXED_HANSARD_DATA_DF$cat3_2 <- NA
FINAL_FIXED_HANSARD_DATA_DF$X_merge <- NA
FINAL_FIXED_HANSARD_DATA_DF$citizen_dummy <- NA
FINAL_FIXED_HANSARD_DATA_DF$type_4cat <- NA
FINAL_FIXED_HANSARD_DATA_DF$interestgroup <- NA

matchRow <- match(FINAL_FIXED_HANSARD_DATA_DF$UniqID,FinalHansard_type$uniqid)

for (i in 1:nrow(FINAL_FIXED_HANSARD_DATA_DF)) {
FINAL_FIXED_HANSARD_DATA_DF[i,25:44] <- FinalHansard_type[matchRow[i],3:22] # !! manually check to make sure column names are the same
}

# we can also recode the two variables of interest using words instead of number codes
FINAL_FIXED_HANSARD_DATA_DF$citizen_dummy <- ifelse(FINAL_FIXED_HANSARD_DATA_DF$citizen_dummy == 0, "Business_group", ifelse(FINAL_FIXED_HANSARD_DATA_DF$citizen_dummy == 1, "Citizen_group", 999))

# type_4cat (1 "Citizen group" 2 "Business association" 3 "Trade union" 4 "Professional group")
FINAL_FIXED_HANSARD_DATA_DF$type_4cat <- ifelse(FINAL_FIXED_HANSARD_DATA_DF$type_4cat == 1, "Citizen_group", 
                                                  ifelse(FINAL_FIXED_HANSARD_DATA_DF$type_4cat == 2, "Business_association", 
                                                         ifelse(FINAL_FIXED_HANSARD_DATA_DF$type_4cat == 3, "Trade_union",
                                                                ifelse(FINAL_FIXED_HANSARD_DATA_DF$type_4cat == 4, "Professional_group",
                                                                       999))))

### we want to merge the party labels into four categories
# A and (blank) – remove
# N/A (these are the Speaker of either house) – remove
# AG – retain
# AMEP, AUS, DLP, FFP, GLT, IND, Ind., LDP, PUP – collapse into new 'OTHER' category
# ALP – retain
# CLP, LP, NATS, NatsWA – collapse into new ‘COALITION’ category

levelsParties <- as.character(levels(FINAL_FIXED_HANSARD_DATA_DF$Party))

FINAL_FIXED_HANSARD_DATA_DF$Party <- as.character(FINAL_FIXED_HANSARD_DATA_DF$Party)

toRem <- which(FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[1])
FINAL_FIXED_HANSARD_DATA_DF$Party[toRem] <- NA
toRem <- which(FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[14])
FINAL_FIXED_HANSARD_DATA_DF <- FINAL_FIXED_HANSARD_DATA_DF[-toRem,] # we actually remove these rows
toRem <- which(FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[4] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[5] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[7] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[8] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[9] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[10] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[11] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[12] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[18])
FINAL_FIXED_HANSARD_DATA_DF$Party[toRem] <- "OTHER"
toRem <- which(FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[6] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[13] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[15] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[16] |
                 FINAL_FIXED_HANSARD_DATA_DF$Party==levelsParties[17])
FINAL_FIXED_HANSARD_DATA_DF$Party[toRem] <- "COALITION"

FINAL_FIXED_HANSARD_DATA_DF$Party <- as.factor(FINAL_FIXED_HANSARD_DATA_DF$Party)
FINAL_FIXED_HANSARD_DATA_DF$ParlNo <- as.factor(FINAL_FIXED_HANSARD_DATA_DF$ParlNo)
FINAL_FIXED_HANSARD_DATA_DF$Text <- as.character(FINAL_FIXED_HANSARD_DATA_DF$Text)
FINAL_FIXED_HANSARD_DATA_DF$TextParagraph <- as.character(FINAL_FIXED_HANSARD_DATA_DF$TextParagraph)

## write the cleaned file to disk...
write.csv(FINAL_FIXED_HANSARD_DATA_DF,"./NEW_FIXED_DATA_MARCH_2016/FINAL_FIXED_HANSARD_DATA_DF.csv",row.names = F,fileEncoding = "UTF-8")

#############################################################################################################################################

####### PART THREE - MACHINE LEARNING TO CLASSIFY PROMINENCE

library(tm)
library(RTextTools)
require(xlsx)
library(stringr)

# We have a bug with the code that needs a workaround (I found one from: https://groups.google.com/forum/#!topic/rtexttools-help/VILrGoRpRrU)
ResortDtm <- function(working.dtm) {
  # sorts a sparse matrix in triplet format (i,j,v) first by i, then by j.
  # Args:
  #   working.dtm: a sparse matrix in i,j,v format using $i $j and $v respectively. Any other variables that may exist in the sparse matrix are not operated on, and will be returned as-is.
  # Returns:
  #   A sparse matrix sorted by i, then by j.
  working.df <- data.frame(i = working.dtm$i, j = working.dtm$j, v = working.dtm$v)  # create a data frame comprised of i,j,v values from the sparse matrix passed in.
  working.df <- working.df[order(working.df$i, working.df$j), ] # sort the data frame first by i, then by j.
  working.dtm$i <- working.df$i  # reassign the sparse matrix' i values with the i values from the sorted data frame.
  working.dtm$j <- working.df$j  # ditto for j values.
  working.dtm$v <- working.df$v  # ditto for v values.
  return(working.dtm) # pass back the (now sorted) data frame.
}  # end function

data_coded_new <- read.xlsx2("./FINAL_DATA_HANSARD_DF_cleaned_SAMPLE_n700_foranalysis.xls", 1)

data <- data_coded_new

# remove the useless/bad rows
toDel <- which(data$darren2==666 | data$darren2==999)
data <- data[-toDel,]

## now get the originally coded data
data_oldcoded <- read.csv("./FINAL_DATA_HANSARD_DF_cleaned_SAMPLE_n400_final07112016.csv", stringsAsFactors = F)

# remove the NA (the 666)
toDel <- which(data_oldcoded$Prominence==666)
data_oldcoded <- data_oldcoded[-toDel,]

#RECODE CATEGORIES TO BINARY ####

toRecode <- which(data_oldcoded$Prominence==1 | data_oldcoded$Prominence==2)
data_oldcoded$Prominence[toRecode] <- 0

toRecode <- which(data_oldcoded$Prominence==3 | data_oldcoded$Prominence==4)
data_oldcoded$Prominence[toRecode] <- 1

## Combine the datasets

data_combined <- data.frame(TextParagraph=data$TextParagraph,
                            Prominence=data$darren2)

temp <- data.frame(TextParagraph=data_oldcoded$TextParagraph,
                   Prominence=data_oldcoded$Prominence)

data_combined <- rbind(data_combined,temp)

data <- data_combined # <-- just overwrite/assign over the original object name

data$Prominence <- as.character(data$Prominence)
data$Prominence <- as.factor(data$Prominence)

# remove training examples where word count for text paragraph is less than 15 words (so, not a real paragraph...)
wordCounts <-str_count(data$TextParagraph, "\\S+")
toDel <- which(wordCounts < 15)
data <- data[-toDel]

## we implement 10-fold cross validation.
## so we are generating 10 SVM models,
## and then getting an idea of how our model performs out of sample,
## using 9 folds as training and 1 fold as test.

folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
rownames(data) <- c(1:nrow(data))

results_10fold_list <- vector("list",10)

#Perform 10 fold cross validation
for (i in 1:10) {
  
  #Segment your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  # test <- projectDataSubset[testIndexes, ]
  # train <- projectDataSubset[-testIndexes, ]
  
  data_test_temp <- data[testIndexes,]
  data_train_temp <- data[-testIndexes,]
  
  data_for_container <- rbind(data_test_temp,data_train_temp)
  
  corpus <- Corpus(VectorSource(data_for_container$TextParagraph))
  # can also use:
  # stemWords = TRUE, removeStopwords = TRUE
  doc_matrix <- DocumentTermMatrix(corpus, control=list(stemming=F, removeNumbers=F, stripWhitespace=TRUE, toLower=TRUE,removePunctuation = TRUE))
  #doc_matrix <- removeSparseTerms(doc_matrix, 0.99)
  
  doc_matrix <- ResortDtm(doc_matrix) # we now use the function to 'fix' the matrix
  
  trainSize_from <- as.numeric(length(testIndexes)+1)
  trainSize_to <- as.numeric(nrow(data_for_container))
  testSize_from <- 1
  testSize_to <- as.numeric(length(testIndexes))
  
  container <- create_container(doc_matrix, data_for_container$Prominence, trainSize=trainSize_from:trainSize_to, testSize=testSize_from:testSize_to, virgin=FALSE)
  
  ## Now train models
  # Start with SVM
  
### SVM
  
  SVM <- train_model(container,"SVM")
  SVM_CLASSIFY <- classify_model(container, SVM)
  analytics_SVM <- create_analytics(container,SVM_CLASSIFY)
  
  topic_summary_SVM <- analytics_SVM@label_summary
  alg_summary_SVM <- analytics_SVM@algorithm_summary
  ens_summary_SVM <-analytics_SVM@ensemble_summary
  doc_summary_SVM <- analytics_SVM@document_summary
  
  results_10fold_list[[i]] <- alg_summary_SVM
}

# get the average f-score on the target variable

average_fscore_on_target_var <- c(rep(NA,10))

for (i in 1:10) {
  average_fscore_on_target_var[i] <- results_10fold_list[[i]]$SVM_FSCORE[2]
}

# get the average f-score
mean(average_fscore_on_target_var)

## what this gives us is an idea of how the model performs on out of sample data.
## we report this in the paper.
## BUT - then we want to create a new model using all of our data (full sample),
## because we want to use as much data as possible, now we know how the model performs (i.e. it is viable for application)

data_UNCODED_cleaned <- read.csv("./NEW_FIXED_DATA_MARCH_2016/FINAL_FIXED_HANSARD_DATA_DF.csv", stringsAsFactors = F)
data_UNCODED_cleaned$Prominence <- NA

data$TextParagraph <- gsub("\\*","",data$TextParagraph)

quickTextClean <- function(text) {
  text <- tolower(text)
  text <- gsub("[[:punct:]]", "", text)
  text <- gsub("\\s+"," ",text)
  return(text)
}

wordCounts <-str_count(data_UNCODED_cleaned$TextParagraph, "\\S+")
toDel <- which(wordCounts < 15)
data_ROWS_REMOVED_TO_REINSERT_LATER <- data_UNCODED_cleaned[toDel,] # !! We want to REINSERT these deleted rows again later, labelling prominence as 999.
data_UNCODED_cleaned <- data_UNCODED_cleaned[-toDel,]

uncodedTextTemp <- quickTextClean(data_UNCODED_cleaned$TextParagraph)
dataTextTemp <- quickTextClean(data$TextParagraph)

rownames(data_UNCODED_cleaned) <- c(1:nrow(data_UNCODED_cleaned))

# matchesTemp <- c()
for (i in 1:nrow(data)) {
  # rowMatches <- which(uncodedTextTemp==dataTextTemp[i] &
  #                       data_UNCODED_cleaned$UniqID==data$UniqID[i] &
  #                       data_UNCODED_cleaned$Name==data$Name[i])
  rowMatches <- which(uncodedTextTemp==dataTextTemp[i])
  data_UNCODED_cleaned$Prominence[rowMatches] <- data$Prominence[i]
  # matchesTemp <- c(rowMatches,matchesTemp)
}
data_UNCODED_cleaned$Prominence <- as.factor(data_UNCODED_cleaned$Prominence)
codedRows <- which(data_UNCODED_cleaned$Prominence==1 | data_UNCODED_cleaned$Prominence==2)

data_UNCODED_cleaned_notcodedrows <- data_UNCODED_cleaned[-codedRows,]
data_UNCODED_cleaned_codedrows <- data_UNCODED_cleaned[codedRows,]
lengthOfCoded <- nrow(data_UNCODED_cleaned_codedrows) # so we know how big the training size (which row to stop at) for SVM

# we will train our model on the coded dataset first.
# we know from the previous 10-fold cross validation that it works quite well,
# so we can now use it in production, using all the coded data to train the model.

corpus_coded <- Corpus(VectorSource(data_UNCODED_cleaned_codedrows$TextParagraph))
doc_matrix_coded <- DocumentTermMatrix(corpus_coded, control=list(stemming=F, removeNumbers=F, stripWhitespace=TRUE, toLower=TRUE,removePunctuation = TRUE))
# doc_matrix_coded <- ResortDtm(doc_matrix_coded) # we now use the function to 'fix' the matrix
container <- create_container(doc_matrix_coded, data_UNCODED_cleaned_codedrows$Prominence, trainSize=1:nrow(data_UNCODED_cleaned_codedrows), virgin=FALSE)
SVM <- train_model(container,"SVM")

# then we will use our trained `svm` model to classify the uncoded (virgin) data
corpus_uncoded <- Corpus(VectorSource(data_UNCODED_cleaned_notcodedrows$TextParagraph))
doc_matrix_uncoded <- DocumentTermMatrix(corpus_uncoded, control=list(stemming=F, removeNumbers=F, stripWhitespace=TRUE, toLower=TRUE,removePunctuation = TRUE))
# doc_matrix_uncoded <- ResortDtm(doc_matrix_uncoded) # we now use the function to 'fix' the matrix
container_uncoded <- create_container(doc_matrix_uncoded, data_UNCODED_cleaned_notcodedrows$Prominence, testSize = 1:nrow(data_UNCODED_cleaned_notcodedrows), virgin=TRUE)

SVM_CLASSIFY_uncoded_data <- classify_model(container_uncoded, SVM)

# and then create our analytics
analytics_SVM_virgin <- create_analytics(container_uncoded,SVM_CLASSIFY_uncoded_data)

# We will pull out each separate summary from the analytics:
topic_summary_SVM_virgin <- analytics_SVM_virgin@label_summary
doc_summary_SVM_virgin <- analytics_SVM_virgin@document_summary

# We can now view the categories that our documents were classified into
doc_summary_SVM_virgin

# and then we apply these labels back onto our data!!!
data_UNCODED_cleaned_notcodedrows$Prominence <- doc_summary_SVM_virgin$CONSENSUS_CODE

#### Final step 1: we stitch back together our coded and not-coded rows of our dataset, to provide the final coded dataset

data_CODED_AUTOMATICALLY_cleaned <- rbind(data_UNCODED_cleaned_codedrows,data_UNCODED_cleaned_notcodedrows)

# load the dataset of groups that we want to retain
groups_to_include <- read.xlsx2("list of Uniqid for Tim.xlsx", 1)
groups_to_include <- groups_to_include$uniqid
# match the uniqIDs in our dataset with the ids specified in groups_to_include
groupMatches <- match(unique(data_CODED_AUTOMATICALLY_cleaned$UniqID),groups_to_include)
# find which IDs in our dataset are not found in the groups_to_include
na_matches <- which(is.na(groupMatches))
# delete the rows for any IDs that we *do not* want in the data 
toRem <- unique(data_CODED_AUTOMATICALLY_cleaned$UniqID)[na_matches]

data_CODED_AUTOMATICALLY_cleaned <- subset(data_CODED_AUTOMATICALLY_cleaned, UniqID %in% groups_to_include)

write.csv(data_CODED_AUTOMATICALLY_cleaned,"./NEW_FIXED_DATA_MARCH_2016/FINAL_AUTOMATICALLY_CODED_DATA_HANSARD.csv",row.names = F)

#### Final step 2: we append the removed rows (less than 15 words) to provide a final coded raw mentions dataset
data_ROWS_REMOVED_TO_REINSERT_LATER$Prominence <- 999

data_RAW_MENTIONS_AND_CODED_AUTOMATICALLY_cleaned <- rbind(data_CODED_AUTOMATICALLY_cleaned,data_ROWS_REMOVED_TO_REINSERT_LATER)

# ensure that *only* the groups of interest are included

# load the dataset of groups that we want to retain
groups_to_include <- read.xlsx2("list of Uniqid for Tim.xlsx", 1)
groups_to_include <- groups_to_include$uniqid
# match the uniqIDs in our dataset with the ids specified in groups_to_include
groupMatches <- match(unique(data_RAW_MENTIONS_AND_CODED_AUTOMATICALLY_cleaned$UniqID),groups_to_include)
# find which IDs in our dataset are not found in the groups_to_include
na_matches <- which(is.na(groupMatches))
# delete the rows for any IDs that we *do not* want in the data 
toRem <- unique(data_RAW_MENTIONS_AND_CODED_AUTOMATICALLY_cleaned$UniqID)[na_matches]

data_RAW_MENTIONS_AND_CODED_AUTOMATICALLY_cleaned <- subset(data_RAW_MENTIONS_AND_CODED_AUTOMATICALLY_cleaned, UniqID %in% groups_to_include)

write.csv(data_RAW_MENTIONS_AND_CODED_AUTOMATICALLY_cleaned,"./NEW_FIXED_DATA_MARCH_2016/FINAL_RAW_MENTIONS_AND_AUTOMATICALLY_CODED_DATA_HANSARD.csv",row.names = F)

#############################################################################################################################################

####### PART FOUR - CALCULATE MENTION FREQUENCIES BY GROUPS

hansard_20102016_mentionsCounts <- read.csv("Hansard_20102016.csv")
hansard_20102016_mentionsCounts$mentioncounts <- NA

QuickCleanFunc <- function(data) {
  data <- tolower(data) # there are sometimes misspellings in text due to punctuation in names of groups
  data <- gsub("[[:punct:]]", "", data) # so we will also match on the name without punctuation
  data <- gsub("\\s+"," ",data) # remove extra whitespaces
  return(data)
}

Sys.setlocale('LC_ALL','C') # we have to do this to avoid error on the weird characters in french name
cleanedNames <- QuickCleanFunc(hansard_20102016_mentionsCounts$searchname)
cleanedText <- QuickCleanFunc(FINAL_FIXED_HANSARD_DATA_DF$Text)

# We don't want to sum up any mentions of each group (only want mentions where group is the search query, i.e., name column==group name)
 
for (i in 1:nrow(hansard_20102016_mentionsCounts)) {
  hansard_20102016_mentionsCounts$mentioncounts[i] <- length(which(as.character(FINAL_FIXED_HANSARD_DATA_DF$Name)==as.character(hansard_20102016_mentionsCounts$searchname)[i]))
}

View(hansard_20102016_mentionsCounts)

hansard_20102016_mentionsCounts$searchname <- as.character(hansard_20102016_mentionsCounts$searchname)

# create columns to store the various prominence results
hansard_20102016_mentionsCounts$sumPromMentions <- NA
hansard_20102016_mentionsCounts$sumPromAG <- NA
hansard_20102016_mentionsCounts$sumPromALP <- NA
hansard_20102016_mentionsCounts$sumPromCOALITION <- NA
hansard_20102016_mentionsCounts$sumPromOTHER <- NA

# `sumPromMentions` is sum of prominent mentions per group
for (i in 1:nrow(hansard_20102016_mentionsCounts)) {
  hansard_20102016_mentionsCounts$sumPromMentions[i] <- length(
    which(
      data_CODED_AUTOMATICALLY_cleaned$Prominence[
        data_CODED_AUTOMATICALLY_cleaned$Name==hansard_20102016_mentionsCounts$searchname[i]
        ]=="2"
    )
  )
}

# `sumPromAG` is sum of prominent mentions by AUSTRALIAN GREENS party
for (i in 1:nrow(hansard_20102016_mentionsCounts)) {
  hansard_20102016_mentionsCounts$sumPromAG[i] <- length(
    which(
      data_CODED_AUTOMATICALLY_cleaned$Prominence[
        data_CODED_AUTOMATICALLY_cleaned$Name==hansard_20102016_mentionsCounts$searchname[i] &
          data_CODED_AUTOMATICALLY_cleaned$Party=="AG"
        ]=="2"
    )
  )
}

# `sumPromALP` is sum of prominent mentions by AUSTRALIAN LABOUR party
for (i in 1:nrow(hansard_20102016_mentionsCounts)) {
  hansard_20102016_mentionsCounts$sumPromALP[i] <- length(
    which(
      data_CODED_AUTOMATICALLY_cleaned$Prominence[
        data_CODED_AUTOMATICALLY_cleaned$Name==hansard_20102016_mentionsCounts$searchname[i] &
          data_CODED_AUTOMATICALLY_cleaned$Party=="ALP"
        ]=="2"
    )
  )
}

# `sumPromCOALITION` is sum of prominent mentions by COALITION party
for (i in 1:nrow(hansard_20102016_mentionsCounts)) {
  hansard_20102016_mentionsCounts$sumPromCOALITION[i] <- length(
    which(
      data_CODED_AUTOMATICALLY_cleaned$Prominence[
        data_CODED_AUTOMATICALLY_cleaned$Name==hansard_20102016_mentionsCounts$searchname[i] &
          data_CODED_AUTOMATICALLY_cleaned$Party=="COALITION"
        ]=="2"
    )
  )
}

# `sumPromOTHER` is sum of prominent mentions by OTHER parties
for (i in 1:nrow(hansard_20102016_mentionsCounts)) {
  hansard_20102016_mentionsCounts$sumPromOTHER[i] <- length(
    which(
      data_CODED_AUTOMATICALLY_cleaned$Prominence[
        data_CODED_AUTOMATICALLY_cleaned$Name==hansard_20102016_mentionsCounts$searchname[i] &
          (data_CODED_AUTOMATICALLY_cleaned$Party=="OTHER" | 
             data_CODED_AUTOMATICALLY_cleaned$Party=="NATS" | 
             data_CODED_AUTOMATICALLY_cleaned$Party=="NXT" | 
             data_CODED_AUTOMATICALLY_cleaned$Party=="PUP" | 
             data_CODED_AUTOMATICALLY_cleaned$Party=="NatsWA" | 
             is.na(data_CODED_AUTOMATICALLY_cleaned$Party))
        ]=="2"
    )
  )
}

# add a column that contains the raw_mention_counts
hansard_20102016_mentionsCounts$rawmentioncount <- NA

for (i in 1:nrow(hansard_20102016_mentionsCounts)) {
  hansard_20102016_mentionsCounts$rawmentioncount[i] <- length(which(as.character(data_RAW_MENTIONS_AND_CODED_AUTOMATICALLY_cleaned$Name)==as.character(hansard_20102016_mentionsCounts$searchname)[i]))
}

# We need to SWAP the rawmentions and mentions values in the columns around, as they are back to front
tempmentioncount <- hansard_20102016_mentionsCounts$mentioncounts
temprawmentioncount <- hansard_20102016_mentionsCounts$rawmentioncount

hansard_20102016_mentionsCounts$mentioncounts <- temprawmentioncount
hansard_20102016_mentionsCounts$rawmentioncount <- tempmentioncount

View(hansard_20102016_mentionsCounts)

write.csv(hansard_20102016_mentionsCounts,"hansard_20102016_mentionsCounts_COMPLETE_DATASET_prominent_mentions_by_party.csv",row.names = F)



