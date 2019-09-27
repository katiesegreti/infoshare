#regions (select regionid and dispname)
region_selection <- dbGetQuery(con, 
                               "SELECT regionid, dispname 
           FROM [dbo].[regions]
           WHERE regionid != 'WRL'
                     ORDER BY regionidn")
region_choices <- with(region_selection, split(regionid, dispname))

# function to get regionid
selected_region <- function(region) {
  q <- dbGetQuery(con,
                  paste0("SELECT regionid
           FROM [dbo].[regions]
           WHERE dispname = '", region, "'"))
  return(q[1,])
}
# function to get areatypes for selected region
get_areatypes <- function(regionid) {
  dbGetQuery(con,
             paste0("SELECT areatypeid, dispname, disporder, disporder2
                                 FROM [dbo].[areatype]
                                 WHERE regionid = '", regionid,
                    "' ORDER BY disporder2")
  )
}
# funtion to get areatypeid
selected_areatype <- function(areatype, regionid) {
  q <- dbGetQuery(con,
                  paste0("SELECT areatypeid
                          FROM [dbo].[areatype]
                          WHERE dispname = '", areatype, "'
                           AND regionid = '", regionid,
                         "'"))
  return(q[1,])
}
#get disporder2 from areatypeid
at_disporder2 <- function(areatypeid) {
  q <- dbGetQuery(con,
                  paste0("SELECT disporder
                             FROM [dbo].[areatype]
                             WHERE areatypeid = '", areatypeid,"'"))
  return(q[,1])
}
# function to get areas for selected areatype & region
get_areas <- function(areatypeid) {
  q <- dbGetQuery(con,
                  paste0("SELECT areaid, dispname
                             FROM [dbo].[areas]
                             WHERE areatypeid = '", areatypeid,"'"))
  return(q)
}
# function to get areas for selected areatype & region
get_areas2 <- function(areatypeid, ownerid) {
  q <- dbGetQuery(con,
                  paste0("SELECT areaid, dispname
                             FROM [dbo].[areas]
                             WHERE areatypeid = '", areatypeid,"'
                            AND ownerid = '", ownerid, "'"))
  return(q)
}
#function to get the state's areaid to get the countie4s for that state
selected_area <- function(areaid, areatypeid) {
  q <- dbGetQuery(con,
                  paste0("SELECT areaid
                          FROM [dbo].[areas]
                          WHERE areaid = '", areaid, "'
                           AND areatypeid = '", areatypeid,
                         "'"))
  return(q[1,])
}
#function to get the disporder of the areatypes
get_disporder <- function(areatypeid) {
  q <- dbGetQuery(con,
                  paste0("SELECT disporder
                             FROM [dbo].[areatype]
                             WHERE areatypeid = '", areatypeid,"'"))
  return(q[1,])
}
# datafiles for list of choices
get_datafiles <- function(filecatid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[datfiles]
                          WHERE filecatid = '", filecatid, 
                         "' AND data_type = 'C'")
  )
  return(q[,1])
}
# filecats for list of categories (demographic, etc.)
get_filecats <- function(regionid) {
  q <- dbGetQuery(con,
                  paste0("SELECT filecatid, dispname
                          FROM [dbo].[filecats]
                          WHERE regionid = '", regionid, 
                         "' ORDER BY disporder")
  )
  return(q)
}
# get the filecatid
filecat_selected <- function(filecat, regionid) {
  q <- dbGetQuery(con,
                  paste0("SELECT filecatid
                          FROM [dbo].[filecats]
                          WHERE dispname LIKE '%", filecat, "%'
                           AND regionid = '", regionid, "'"))
  return(q[1,])
}
# datafiles for list of choices
get_datafiles <- function(filecatid) {
  q <- dbGetQuery(con,
                  paste0("SELECT *
                          FROM [dbo].[datfiles]
                          WHERE filecatid = '", filecatid, 
                         "' AND data_type = 'C'
                         ORDER BY disporder")
  )
  return(q)
}
#get datfileid
datfile_selected <- function(datfile, filecatid) {
  q <- dbGetQuery(con,
                  paste0("SELECT datfileid
                          FROM [dbo].[datfiles]
                          WHERE dispname LIKE '%", datfile, "%'
                           AND filecatid = '", filecatid, "'"))
  return(q[1,])
}
# get years
get_years <- function(datfileid) {
  q <- dbGetQuery(con,
                  paste0("SELECT yearid, dispname
                          FROM [dbo].[years]
                          WHERE datfileid = '", datfileid, 
                         "' ORDER BY disporder"))
  return(q)
}
# get  yearid (for datfiled with > one year)
year_selected <- function(year, datfileid) {
  q <- dbGetQuery(con,
                  paste0("SELECT yearid
                          FROM [dbo].[years]
                          WHERE dispname LIKE '%", year, "%'
                           AND datfileid = '", datfileid, "'"))
  return(q[1,])
}
#get yearid for datfileid with only one year
year_selected_1 <- function(datfileid) {
  q <- dbGetQuery(con,
                  paste0("SELECT yearid
                          FROM [dbo].[years]
                          WHERE datfileid = '", datfileid, 
                         "'"))
  return(q[1,])
}
#get cats from yearid
get_cats <- function(yearid) {
  q <- dbGetQuery(con,
                  paste0("SELECT catid, dispname
                          FROM [dbo].[cats]
                          WHERE yearid = '", yearid, 
                         "'  ORDER BY disporder"))
  return(q)
}
#get catid for yearid with > one cat
cat_selected <- function(cat, yearid) {
  q <- dbGetQuery(con,
                  paste0("SELECT catid
                          FROM [dbo].[cats]
                          WHERE dispname LIKE '%", cat, "%'
                           AND yearid = '", yearid, "'"))
  return(q[1,])
}
#get catid for yearid with only one cat
cat_selected_1 <- function(yearid) {
  q <- dbGetQuery(con,
                  paste0("SELECT catid
                          FROM [dbo].[cats]
                          WHERE yearid = '", yearid, 
                         "'"))
  return(q[1,])
}
#get groups from catid
get_groups <- function(catid) {
  q <- dbGetQuery(con,
                  paste0("SELECT groupid, dispname
                          FROM [dbo].[groups]
                          WHERE catid = '", catid, 
                         "' ORDER BY disporder"))
  return(q)
}
# get groupid
group_selected <- function(group, catid) {
  q <- dbGetQuery(con,
                  paste0("SELECT groupid
                          FROM [dbo].[groups]
                          WHERE dispname LIKE '%", group, "%'
                           AND catid = '", catid, "'"))
  return(q)
}
vars_info <- function(groupid) {
  dbGetQuery(con,
             paste0("SELECT v.varid, v.dispname, v.file2, v.field2, g.file1
           FROM [dbo].[groups] AS g
           LEFT JOIN [dbo].[types] AS t
           ON t.subtypeid = g.subtypeid
           LEFT JOIN [dbo].[vars] as v
           ON v.typeid = t.typeid
           WHERE g.groupid = '", groupid, 
                    "'"))
}
#vars_info for an array: returns a list of dataframes
vars_info_array <- function(groupids) {
  groupids %>% map(vars_info)
}
#get file0 from datfileid
get_file0 <- function(datfileid) {
  q <- dbGetQuery(con,
                  paste0("SELECT file0, dispname
                          FROM [dbo].[datfiles]
                          WHERE datfileid = '", datfileid, 
                         "' ORDER BY disporder"))
  return(q)
}

get_stacks_data <- function(db, fields, areaid) {
  q <- dbGetQuery(stacks,
                  paste0("SELECT ", fields, 
                         " FROM [dbo].[",db,"]
           WHERE AREAID = '", areaid, "'"))
  return(unname(data.frame(as.integer(q))))
}
#areatype name
areatype_name <- function(areatypeid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[areatype]
                          WHERE areatypeid = '", areatypeid, 
                         "' ORDER BY disporder"))
  return(q[1,1])
}
#group name
group_name <- function(groupid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[groups]
                          WHERE groupid = '", groupid, 
                         "' ORDER BY disporder"))
  return(trimws(q[1,1]))
}
#datfile name
datfile_name <- function(datfileid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[datfiles]
                          WHERE datfileid = '", datfileid, 
                         "' ORDER BY disporder"))
  return(trimws(q[1,1]))
}
year_name <- function(yearid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[years]
                          WHERE yearid = '", yearid, 
                         "' ORDER BY disporder"))
  return(trimws(q[1,1]))
}
#area name
area_name <- function(areaid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[areas]
                          WHERE areaid = '", areaid, 
                         "' ORDER BY disporder"))
  return(trimws(q[1,1]))
}
#region name
region_name <- function(regionid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[regions]
                          WHERE regionid = '", regionid, 
                         "' "))
  return(trimws(q[1,1]))
}
#source name
source_name <- function(datfileid) {
  q <- dbGetQuery(con,
                  paste0("SELECT source
                          FROM [dbo].[datfiles]
                          WHERE datfileid = '", datfileid, 
                         "' "))
  return(trimws(q[1,1]))
}
#source name (shorter)
source_name1 <- function(datfileid) {
  q <- dbGetQuery(con,
                  paste0("SELECT dispname
                          FROM [dbo].[datfiles]
                          WHERE datfileid = '", datfileid, 
                         "' "))
  return(trimws(q[1,1]))
}
