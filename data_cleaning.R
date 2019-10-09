library(tidyverse)
library(rebus)

#read in data
convers <- read_csv("convers_table.csv")
areatypes <- read_csv("areatype_table.csv")
HRA01_raw <- read_csv("HRA01.csv")
HRA01_after <- read_csv("hra01_Infoshare.csv")
areas <- read_csv("areas_table.csv")
HRA01_testing <- read_csv("HRA01_testing.csv")


##use generic column names at first 
names(HRA01_raw) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                      "X10", "X11", "X12", "X13", "X14", "X15")


#go through the raw data and figure out how to break up the tables
#search for __ which indicates the beginning of a table?




##fill in the borough names before moving values?
boros <- HRA01_raw %>%
  filter(!is.na(X1)) %>%
  select(id, X1) %>%
  mutate(tot = ifelse(str_detect(X1, "SUM") | str_detect(X1, "__"), 1, 0),
         nxt = lead(id))



county <- rep(0, nrow(HRA01_raw))
for(i in 5:nrow(HRA01_raw)) {
  county[i] <- ifelse(!is.na(HRA01_raw$X1[i]), HRA01_raw$X1[i], county[i-1])
}

HRA01_raw1 <- cbind(HRA01_raw, county)
class(HRA01_raw1$county)

HRA01_raw1$county <- as.character(HRA01_raw1$county)



#add new_tbl  columns
HRA01_raw1 <- HRA01_raw1 %>%
  mutate(new_tbl = str_detect(X1, "__"))

#remove na rows
HRA01_raw1 <- HRA01_raw1 %>%
  filter(!is.na(X2) & X2 != "ISLAND")

#add id column
HRA01_raw1 <- HRA01_raw1 %>%
  mutate(id = row_number())

HRA01_raw1 <- HRA01_raw1 %>%
  mutate(X1 = ifelse(is.na(X1), county, X1))

#now add move column, now that the nas are gone from X1
HRA01_raw1 <- HRA01_raw1 %>%
  mutate(move = ifelse(str_detect(X1, "SUM"), 1, 0))

HRA01_raw1 %>%
  count(new_tbl)

tbl_breaks <- HRA01_raw1 %>% 
  filter(new_tbl == TRUE) %>%
  select(id)


#total columns need to move values one column to the right
HRA01_raw1 <- HRA01_raw1 %>%
  mutate(X15 = ifelse(move, X14, X15),
         X14 = ifelse(move, X13, X14),
         X13 = ifelse(move, X12, X13),
         X12 = ifelse(move, X11, X12),
         X11 = ifelse(move, X10, X11),
         X10 = ifelse(move, X9, X10),
         X9 = ifelse(move, X8, X9),
         X8 = ifelse(move, X7, X8),
         X7 = ifelse(move, X6, X7),
         X6 = ifelse(move, X5, X6),
         X5 = ifelse(move, X4, X5),
         X4 = ifelse(move, X3, X4),
         X3 = ifelse(move, X2, X3),
         X2 = ifelse(move, "total", X2))


#digits in census tract number
HRA01_raw1 <- HRA01_raw1 %>%
  mutate(ct_len = ifelse(X2 != "total", nchar(X2), 1))

#

HRA01_raw1 %>%
  filter(X1 %in% c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN")) %>%
  count(ct_len)


six <- HRA01_raw1 %>%
  filter(ct_len > 5) %>%
  filter(X1 %in% c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN")) 

six %>%
  count(X1)
# 
# 
#
#get the borough areaids to construct the census tract areaids
boro_codes <- areas %>%
  filter(areatypeid == "NYCCOUNTIES") %>%
  select(areaid, dispname)


c_tracts <- areas %>%
  filter(ownerid %in% boro_codes$areaid) %>%
  filter(str_detect(areaid, "CEN"))

c_tracts_bx <- areas %>%
  filter(ownerid == "005005CITY")%>%
  filter(str_detect(areaid, "CEN"))

c_tracts_bk <- areas %>%
  filter(ownerid == "047047CITY")%>%
  filter(str_detect(areaid, "CEN"))

c_tracts_m <- areas %>%
  filter(ownerid == "061061CITY")%>%
  filter(str_detect(areaid, "CEN"))

c_tracts_q <- areas %>%
  filter(ownerid == "081081CITY")%>%
  filter(str_detect(areaid, "CEN"))

c_tracts_si <- areas %>%
  filter(ownerid == "085085CITY")%>%
  filter(str_detect(areaid, "CEN"))
#var_info() %>% map_chr(function(x) trimws(x[1,5]))
#map2_chr(file1(), file2(), ~ paste0(trimws(file0()), .x, .y))

bx_pattern <- START %R% "0050"
bk_pattern <- START %R% "047" %R% or("0", "1", "9")
m_pattern <- START %R% "0610"
q_pattern <- START %R% "081" %R% or("0", "1", "9")
si_pattern <- START %R% "085" %R% or("0", "1", "9")

nrow(c_tracts_bx) == sum(str_count(c_tracts_bx$dispname, bx_pattern))
nrow(c_tracts_bk) == sum(str_count(c_tracts_bk$dispname, bk_pattern))
nrow(c_tracts_m) == sum(str_count(c_tracts_m$dispname, m_pattern))
nrow(c_tracts_q) == sum(str_count(c_tracts_q$dispname, q_pattern))
nrow(c_tracts_si) == sum(str_count(c_tracts_si$dispname, si_pattern))

bx_code <- "005"
bk_code <- "047"
m_code <- "061"
q_code <- "081"
s_code <- "085"

#add the borough pattern
HRA01_raw1 <- HRA01_raw1 %>%
  mutate(boro_pattern = case_when(county == "BRONX" ~ bx_code ,
                                  county == "BROOKLYN" ~ bk_code,
                                  county == "MANHATTAN" ~ m_code,
                                  county == "QUEENS" ~ q_code,
                                  county == "STATEN" ~ s_code))

#bx census tract lengths
HRA01_raw1 %>%
  filter(X1 == "BRONX") %>%
  count(ct_len)

#manhattan census tract lengths
HRA01_raw1 %>%
  filter(X1 == "MANHATTAN") %>%
  count(ct_len)

#CONSTRUCT THE CENSUS TRACT WITH BORO CODES FOR BRONX AND MANHATTAN
bx_and_man <- HRA01_raw1 %>%
  filter(X1 != "BROOKLYN" & X1 != "QUEENS" & X1 != "STATEN") %>%
  mutate(ct = case_when(ct_len == 1 ~ str_c(boro_pattern, "00", X2, ".00"),
                        ct_len == 2 ~ str_c(boro_pattern, "0", X2, ".00"),
                        ct_len == 3 ~ str_c(boro_pattern, X2, ".00"),
                        ct_len == 4 ~ str_c(boro_pattern, "0", substr(X2, 1, 2), ".", substr(X2, 3, 4)),
                        ct_len == 5 ~ str_c(boro_pattern, substr(X2, 1, 3), ".", substr(X2, 4, 5)))) %>%
  mutate(areaid = str_c(ct, substr(boro_pattern, 1, 3), "CENTRAC1"))


#QUEENS census tract lengths
HRA01_raw1 %>%
  filter(X1 == "QUEENS") %>%
  count(ct_len)

#just bronx for now
#CONSTRUCT THE CENSUS TRACT WITH BORO CODES FOR BRONX
bx_hra <- HRA01_raw1 %>%
  filter(X1 != "BROOKLYN" & X1 != "QUEENS" & X1 != "STATEN" & X1 != "MANHATTAN") %>%
  mutate(ct = case_when(ct_len == 1 ~ str_c(boro_pattern, "00", X2, ".00"),
                        ct_len == 2 ~ str_c(boro_pattern, "0", X2, ".00"),
                        ct_len == 3 ~ str_c(boro_pattern, X2, ".00"),
                        ct_len == 4 ~ str_c(boro_pattern, "0", substr(X2, 1, 2), ".", substr(X2, 3, 4)),
                        ct_len == 5 ~ str_c(boro_pattern, substr(X2, 1, 3), ".", substr(X2, 4, 5)))) %>%
  mutate(areaid = str_c(ct, substr(boro_pattern, 1, 3), "CENTRAC1"))

#get bronx zip codes from convers
bx_zipcodes <- convers %>%
  filter(countyid == "005" & toareatypeid == "NYCZIPCODES" & 
           str_detect(fromareaid, "CENTRAC")) 

#list of unique zip codes for BX
bx_zipz <- unique(bx_zipcodes$toareaid)

#map bx_zipz get centrac and overlap for each
#first write a function to tdo that for one
#get census tracts for zip code
get_cts_function <- function(zipcode) {
  q <-convers %>% filter(toareaid == zipcode) %>%
    select(fromareaid, overlap)
  return(list(zipcode, as.data.frame(q)))
}

str(get_cts_function("10451005ZIPCODES"))
get_cts_function("10458005ZIPCODES")
get_cts_function("10465005ZIPCODES")

#map that for all bx zips
bx_zip_ct <- map(bx_zipz, function(x) get_cts_function(x))

#dataframe of just raw bx data for first set
bx_data_1 <- bx_hra[3:348,] %>%
  select(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, id, ct, areaid) %>%
  rename(COUNTY = X1, CENSUS_TRACT = X2, PA_PERSONS = X3,
         PA_CASES = X4,
         SSI_PERS = X5, SSI_CASES = X6, MED_PERS = X7, MED_CASES = X8,
         MED_NURS = X9, MED_TPERS = X10, MED_TCASES = X11)

#try with one zip code first
bx_z1 <- bx_zip_ct[[14]]

bx_z1[[2]][,1] %>% map_df(function(x) filter(bx_data_1, areaid %in% x) %>% select(areaid, PA_PERSONS, PA_CASES, SSI_PERS, 
                                                                                  SSI_CASES, MED_PERS, MED_CASES, MED_NURS,
                                                                                  MED_TPERS, MED_TCASES)) %>%
  left_join(bx_z1[[2]], by = c("areaid" = "fromareaid"))  %>%
  mutate(PA_PERSONS = as.numeric(PA_PERSONS) * overlap, PA_CASES = as.numeric(PA_CASES) * overlap, 
         SSI_PERS = as.numeric(SSI_PERS) * overlap, SSI_CASES = as.numeric(SSI_CASES) * overlap, 
         MED_PERS = as.numeric(MED_PERS) * overlap, MED_CASES = as.numeric(MED_CASES) * overlap,
         MED_NURS = as.numeric(MED_NURS) * overlap, MED_TPERS = as.numeric(MED_TPERS) * overlap, 
         MED_TCASES = as.numeric(MED_TCASES) * overlap) %>%
  summarise(PA_PERSONS = sum(PA_PERSONS), PA_CASES = sum(PA_CASES), SSI_PERS = sum(SSI_PERS),
            SSI_CASES = sum(SSI_CASES), MED_PERS = sum(MED_PERS), MED_CASES = sum(MED_CASES),
            MED_NURS = sum(MED_NURS), MED_TPERS = sum(MED_TPERS), MED_TCASES = sum(MED_TCASES))




#map the first part (getting the census tracts for the zip codes) and save that as a list 
bx_list_ct <- bx_zip_ct %>% map(function(x) x[[2]][,1])
#create empty dataframe for bx zipcode totals for dataset1
bx_dz1 <- c()
#populate that dataframe with the zip code totals
bx_data_1_zips <- map2(bx_list_ct, bx_zip_ct, ~ 
                         filter(bx_data_1, areaid %in% .x) %>% 
                         select(areaid, PA_PERSONS, PA_CASES, SSI_PERS,
                                SSI_CASES, MED_PERS, MED_CASES, MED_NURS,
                                MED_TPERS, MED_TCASES)  %>%
                         left_join(.y[[2]], by = c("areaid" = "fromareaid")) %>% 
                         mutate(PA_PERSONS = as.numeric(PA_PERSONS) * overlap, PA_CASES = as.numeric(PA_CASES) * overlap, 
                                SSI_PERS = as.numeric(SSI_PERS) * overlap, SSI_CASES = as.numeric(SSI_CASES) * overlap, 
                                MED_PERS = as.numeric(MED_PERS) * overlap, MED_CASES = as.numeric(MED_CASES) * overlap,
                                MED_NURS = as.numeric(MED_NURS) * overlap, MED_TPERS = as.numeric(MED_TPERS) * overlap, 
                                MED_TCASES = as.numeric(MED_TCASES) * overlap) %>%
                         summarise(PA_PERSONS = sum(PA_PERSONS), PA_CASES = sum(PA_CASES), SSI_PERS = sum(SSI_PERS),
                                   SSI_CASES = sum(SSI_CASES), MED_PERS = sum(MED_PERS), MED_CASES = sum(MED_CASES),
                                   MED_NURS = sum(MED_NURS), MED_TPERS = sum(MED_TPERS), MED_TCASES = sum(MED_TCASES)) %>%
                         mutate(newareaid = .y[[1]])
) %>%
  map_df(function(x) rbind(bx_dz1, x))


#a moving average of census tract length to determine 
library(zoo)
HRA01_raw1 <- HRA01_raw1 %>%
  mutate(ma2 = rollmean(ct_len, 10, fill= NA),
         dfrnc = ct_len - ma2,
         last_2 = substr(X2, ct_len -1, ct_len))

rr <- HRA01_raw1 %>% filter(ct_len == 3 | ct_len == 4) %>%
  mutate(dfrnc = ct_len - ma2, X2 = as.numeric(X2)) %>%
  select(X1, X2, ct_len, ma2, dfrnc)

#just everywhere for now
#CONSTRUCT THE CENSUS TRACT WITH BORO CODES FOR everywhere
ct_hra <- HRA01_raw1 %>%
  mutate(ct = case_when(ct_len == 1 ~ str_c(boro_pattern, "000", X2, ".00"),
                        ct_len == 2 ~ str_c(boro_pattern, "00", X2, ".00"),
                        #ct_len == 3 ~ str_c(boro_pattern, "0", X2, ".00"),
                        #ct_len == 4 ~ str_c(boro_pattern, "0", substr(X2, 1, 2), ".", substr(X2, 3, 4)),
                        ct_len == 5 ~ str_c(boro_pattern, "00", substr(X2, 1, 3), ".", substr(X2, 4, 5)),
                        ct_len == 3 & (!(last_2 %in% c("01", "02")) | dfrnc <= 0.6) ~ str_c(boro_pattern, "00", X2, ".00"),
                        ct_len == 3 & last_2 %in% c("01", "02") & dfrnc > 0.6 ~ str_c(boro_pattern, "000", substr(X2, 1, 1), ".", substr(X2, 2, 3)),
                        ct_len == 4 & !(last_2 %in% c("01", "02")) & dfrnc <= 0.5 ~ str_c(boro_pattern, "0", X2, ".00"),
                        ct_len == 4 & last_2 %in% c("01", "02") & dfrnc > 0.5 ~ str_c(boro_pattern, "00", substr(X2, 1, 2), ".", substr(X2, 3, 4)))) %>%
  mutate(areaid = str_c(ct, substr(boro_pattern, 1, 3), "CENTRAC1"))

#manhattan 201 and 202 make an exception pls
rr2 <- rr %>% filter(ct_len == 3)


