library(httr)
library(RCurl)
library(rvest)
library(XML)
library(xml2)
library(curl)
library(jsonlite)


##### RENTAL BEAST #####

#dummy data frame to rbind() results to 
rbd <- data.frame("brokerage_name" = as.character(),
                  "apartment_id" = as.character(),
                  "latitude" = as.character(),
                  "longitude" = as.character(),
                  "date_available" = as.character(),
                  "bedrooms" = as.character(),
                  "beds_count" = as.character(),
                  "bathrooms" = as.character(),
                  "rent" = as.character(),
                  "city" = as.character(),
                  "state" = as.character(),
                  "area" = as.character(),
                  "neighborhood" = as.character(),
                  "metro_id" = as.character(),
                  "neighborhood_id" = as.character(),
                  "sub_neighborhood_id" = as.character(),
                  "metro_name" = as.character(),
                  "nbhd_name" = as.character(),
                  "sub_nbhd_name" = as.character(),
                  "source" = as.character(),
                  "zip" = as.character(),
                  "address" = as.character(),
                  "apply_token_url" = as.character(),
                  "exclusive_by" = as.character(),
                  "mls_id" = as.character(),
                  "status" = as.character(),
                  "hide_rb_cp" = as.character(),
                  "hide_partner_cp" = as.character(),
                  "is_featured" = as.character(),
                  "match_type" = as.character(),
                  "Scrape_Date" = as.character()
)

#corpus of "all" zip codes in Boston
zips <- {c("02118",
           "02119",
           "02120",
           "02130",
           "02134",
           "02135",
           "02445",
           "02446",
           "02447",
           "02467",
           "02108",
           "02114",
           "02115",
           "02116",
           "02215",
           "02128",
           "02129",
           "02150",
           "02151",
           "02152",
           "02124",
           "02126",
           "02131",
           "02132",
           "02136",
           "02109",
           "02110",
           "02111",
           "02113",
           "02121",
           "02122",
           "02124",
           "02125",
           "02127",
           "02210"
)}

#for loop that iterates through all 35 zip codes and changes the API endpoint based on the zip

for(i in 1:length(zips)){ 
  #Base Query up to page number
  brpl.1 <- "https://www.rentalbeast.com/api/listing.json?include_disclaimer=true&include_near_close_matches=true&include_neighborhood_filter_list=true&is_for_consumer=true&max_record_count="
  #then the max count value. We start with 500 and test if there are more and then rerun the call
  mc <- 500
  brpl.2 <- "&mls_toggle_value=mixed&page_number=1&partner_key=default&sort_ascending=true&sort_field=dateavailable&state=Massachusetts&statuses=Active&zip="
  #Then the zip value gets passed twice to the last two chunks of the call
  brpl.3 <- "&zip_codes="
  #The zip again
  rbl <- paste(brpl.1, mc, brpl.2, zips[i], brpl.3, zips[i], sep = "")
  
  #Initial system sleep before the try catch call
  slp <- sample(1:6, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
  #Try catch call to see if the endpoint exists
  chka <-  tryCatch({
    fromJSON(rbl)
  },
  error = function(e){e}
  )
  
  if(inherits(chka, "error")) {
    print("URL Broken")
    next
  }
  
  #Another sleep before actually calling the API
  slp <- sample(1:6, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
  #System console output so the whole process doesn't just lag
  print(paste("PULLING LISTINGS FOR ZIP", zips[i],"--", i, "of", length(zips)))
  
  #The actual API call
  rtu <- fromJSON(rbl)
  #Check to make sure that data was pulled/exists for that zip code
  if (length(rtu$data) == 0){
    print("----")
    print(paste("NO DATA FOR ZIP", zips[i]))
    print("----")
    next
  }
  
  #check to see if we got all of the listings for that zip
  #if there are more than the base 500 records, we just change the number
  #to whatever the API has and repulls the appropriate amount of listings
  if (rtu$meta$total_records > mc){
    #change the listing total number
    mc <- rtu$meta$total_records
    #recreate the api url
    rbl <- paste(brpl.1, mc, brpl.2, zips[i], brpl.3, zips[i], sep = "")
    #repull the data
    rtu <- fromJSON(rbl)
  }
  #append data to a single row data frame
  glsx <- data.frame("brokerage_name" = rtu$data$brokerage_name,
                     "apartment_id" = rtu$data$apartment_id,
                     "latitude" = rtu$data$latitude,
                     "longitude" = rtu$data$longitude,
                     "date_available" = rtu$data$date_available,
                     "bedrooms" = rtu$data$bedrooms,
                     "beds_count" = rtu$data$beds_count,
                     "bathrooms" = rtu$data$bathrooms,
                     "rent" =rtu$data$rent,
                     "city" = rtu$data$city,
                     "state" = rtu$data$state,
                     "area" = rtu$data$area,
                     "neighborhood" = rtu$data$neighborhood,
                     "metro_id" = rtu$data$metro_id,
                     "neighborhood_id" = rtu$data$neighborhood_id,
                     "sub_neighborhood_id" = rtu$data$sub_neighborhood_id,
                     "metro_name" = rtu$data$metro_name,
                     "nbhd_name" = rtu$data$nbhd_name,
                     "sub_nbhd_name" = rtu$data$sub_neighborhood_id,
                     "source" = rtu$data$source,
                     "zip" = rtu$data$zip,
                     "address" = rtu$data$address,
                     "apply_token_url" = rtu$data$apply_token_url,
                     "exclusive_by" = rtu$data$exclusive_by,
                     "mls_id" = rtu$data$mls_id,
                     "status" = rtu$data$status,
                     "hide_rb_cp" = rtu$data$hide_rb_cp,
                     "hide_partner_cp" = rtu$data$hide_partner_cp,
                     "is_featured" = rtu$data$is_featured,
                     "match_type" = rtu$data$match_type,
                     "Scrape_Date" = Sys.time()
  )
  
  
  
  #rbind the single row to our dummy
  rbd <- rbind(rbd, glsx)
  
  
  
  
  #System console output to show me that it's working
  print("++++")
  print(paste("SUCCESS PULLING LISTINGS FOR ZIP", zips[i],"--", i, "of", length(zips)))
  print("++++")
}
#remove any duplicates (for whatever reason, there might be some [?])
rbd <- unique(rbd)
#Load in the last dataset
curs <- read.csv("[path to dir]/Rental Beast/Rental_Beast.csv", stringsAsFactors = FALSE)
#Append the new dataset to the old/current existing dataset
nrbd <- rbind(curs,rbd)
#Check for duplicates one last time
nrbd <- unique(nrbd)
#write the csv back to the same file
write.csv(nrbd, "[path to dir]/Rental Beast/Rental_Beast.csv", row.names = FALSE)



