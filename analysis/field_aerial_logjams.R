
#install.packages(c('sf','ggplot2','dplyr'))
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)
#setwd('C:\\Users\\vleung\\Documents\\randall logjam location for robby')
setwd('C:\\Users\\vleung\\Documents\\projects\\Ch 2 Elwha Logjams\\data\\randall logjam location for robby')

##################################################
#Read Data
##################################################
 
#AERIAL DATA
#Filenames to parse
fnames = c('1999_','2008_','2013_','2015_','2015_PF','2016_')
#'2012_',
dfaerial = data.frame() #Will eventually hold all aerial input data
 for(fn in fnames){
       #     print(fn) #Print name of file we're currently working with to show progress
         dfin <- read_sf(paste0('LJ_',fn,'pts.shp')) #Read file
         st_geometry(dfin) = NULL                    #Eliminate geometry column
         #     print(colnames(dfin))                       #Print column names for debugging
           if('length' %in% colnames(dfin)){           #Is 'length' column present?
                 dfin = dfin %>% rename(Length=length)     #Rename 'length' column so we can bind dataframes together
             }
         dfin      = dfin %>% select(MEAS, Identifier, Length) #Get columns of interest
         dfin$year = gsub("_$", "", fn)              #Convert filename to year. Eliminate trailing `_`.
         dfaerial        = rbind(dfaerial,dfin)      #Add to cumulative dataframe
     }
 
#Clean up year names
dfaerial = dfaerial %>% 
       mutate(year=ifelse(year=='2015',    '2015  Preflood', year)) %>%
       mutate(year=ifelse(year=='2015_PF', '2015 Postflood', year))
 
#Clean up Identifier column by splitting it asunder (into two columns)
dfaerial = dfaerial %>% 
  mutate(LJnum=as.integer(gsub("_.*","",Identifier))) %>% #Split up Identifier column
  mutate(LJdim=gsub(".*_","",Identifier)) %>% #Split up Identifier column
  select(-Identifier)                         #Drop Identifier column since we've split it up

#Merge rows relating to a single logjam together
dfaerial = dfaerial %>% 
  group_by(year,LJnum) %>%
  summarize(
    riverkm=mean(MEAS)/1000/3.28,  #convert riverfeet to riverkilometers 
    area=prod(Length)/3.28/3.28 #Multiply width and length to get area
  ) %>% ungroup() %>%
  select(-LJnum) #We no longer need the log jam number

dfaerial$source = "Aerial"
  

#FIELD DATA
#get logjam field data files
fieldfilenames = c('rkm_aggtest2','lj2010_cleaned','lj2012_July','lj2014_July')
fieldyears = c(2001,2010,2012,2014)
fieldyearsn = c('2001','2010','2012','2014')
y = 1
dffield = data.frame()
for (ff in fieldfilenames){
  #    print(ff)
  dffieldin = data.frame()
  #rename columns & add year column (MEAS is distance along the channel in km)
  #dffieldin <- read.csv(paste0('C:\\Users\\vleung\\Documents\\',ff,'.csv'))
  dffieldin <- read.csv(paste0('C:\\Users\\vleung\\Documents\\projects\\Ch 2 Elwha Logjams\\data\\',ff,'.csv'))
  dffieldin = dffieldin %>% mutate(riverkm = rkm, area = size, year = fieldyearsn[y]) %>% select(riverkm, area, year)
  #     print(colnames(dffieldin))
  y = y+1
  dffield = rbind(dffield,dffieldin)
}

dffield$source="Field"

df = rbind(dfaerial,dffield)

#Used to ensure that columns in the output plots have appropriate
#width by adding empty data points for each year and river position
zeros = expand.grid(
  year=unique(df$year), 
  riverkm=seq(min(df$riverkm), max(df$riverkm), by=0.5),
  area=0,
  source="ZeroData"
)
df = rbind(df,zeros) #Add the padding data above to the data frame

#Add segment names
geomorphicbreaks      = c(0,  7.4,  12.1,  21.1, 24.8) 
geomorphicbreaklabels = c('Lower River','Aldwell', 'Middle River', 'Mills')
df$segname = cut(df$riverkm, breaks = geomorphicbreaks, labels = geomorphicbreaklabels)



##################################################
#Plot log jam area per river kilometer
##################################################

#Used for adjusting how many and which labels are shown on the x-axis
breaks = seq(-1,25, by = 1)+0.2
labels = floor(breaks)+1

p = ggplot(df, aes(x=floor(riverkm), fill=year, weight=area)) + 
  geom_bar(position="dodge") +
  scale_x_continuous(breaks = breaks, labels=labels) +
  xlab("River km") + ylab(bquote("Logjam area ["*m^2*"]")) +
  scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
                             "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
  
#Entire graph
p 
#Lower River
p + coord_cartesian(xlim=c(0,7.5),ylim=c(0,28000))+ggtitle("Lower River")
#Aldwell
p + coord_cartesian(xlim=c(7.4,12.1),ylim=c(0,28000))+ggtitle("Aldwell")


#Add this to add an annotated label to a graph at a given point
#annotate("text",x=3.25,y=20000,label="Fruit",color="black")





##################################################
#Plot normalized log jam area by river section
##################################################

#Break the data into river sections based on distances
section_lengths = c(
  'Lower River'=7.4,
  'Aldwell'=12.1-7.4,
  'Middle River'=21.1-12.1, 
  'Mills'=24.8-21.1
)

temp = df %>% 
  group_by(year,segname)    %>% 
  summarize(area=sum(area)) %>%
  ungroup()                 %>%
  mutate(area=area/section_lengths[segname])


ggplot(temp,aes(x=segname,fill=year,weight=area)) + 
  geom_bar(position="dodge") +
  xlab("River segment") + ylab(bquote("Logjam area ["*m^2*"]")) +
  scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
                           "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))














#####################################################
#DANGER - OLD, SCARY CODE!!! :-C :-O
#####################################################
# 
# 
# spacing    = 1
# RKM        = c(seq(0,     9, by = spacing))
# RKMnames   = c(seq(0.5,   9, by = spacing))
# ALDKM      = c(seq(7,    16, by = spacing))
# ALDKMnames = c(seq(7.5,  16, by = spacing))
# MRKM       = c(seq(12,   21, by = spacing))
# MRKMnames  = c(seq(12.5, 21, by = spacing))
# millsKM    = c(seq(21,   30, by = spacing))
# millsKMnames = c(seq(21.5,30,by = spacing))
# half_kmeters = c(seq(0,   30,by = spacing))
# 
# #Break the data into river sections based on distances
# geomorphicbreaks      = c(0,  7.4,  12.1,  21.1, 24.8) 
# geomorphicbreaklabels = c('Lower River','Aldwell', 'Middle River', 'MILLS')
#  
# #Distance along river (df$MEAS) is continuous. We use particular distance demarcations (breaks)
#    #to generate labels for those sections. Adjusting the breaks adjusts where the labels are applied.
# df$segname       = cut(df$MEAS, breaks = geomorphicbreaks, labels = geomorphicbreaklabels)
# df$rkmname       = cut(df$MEAS, breaks = RKM,              labels = RKMnames)
# df$aldkmname     = cut(df$MEAS, breaks = ALDKM,            labels = ALDKMnames)
# df$mrkmname      = cut(df$MEAS, breaks = MRKM,             labels = MRKMnames)
# df$millskmname   = cut(df$MEAS, breaks = millsKM,          labels = millsKMnames)
# df$halfkm        = cut(df$MEAS, breaks = half_kmeters,     labels = half_kmeters)
# 
# #Calculate per-logjam statistics
# ljaerialstats = df                 %>%
#        group_by(year,LJnum, rkmname,aldkmname, mrkmname,millskmname)       %>% 
#        summarise(
#            area    = prod(Length)/3.28/3.28, #Get area in square metres instead of square feet
#              segname = first(segname)
#          ) 
# 
# 
#    #Break the data into river sections based on distances
#  dffield$segname     = cut(dffield$MEAS, breaks = geomorphicbreaks, labels = geomorphicbreaklabels)
#  dffield$rkmname     = cut(dffield$MEAS, breaks = RKM,              labels = RKMnames)
#  dffield$aldkmname   = cut(dffield$MEAS, breaks = ALDKM,            labels = ALDKMnames)
#  dffield$mrkmname    = cut(dffield$MEAS, breaks = MRKM,             labels = MRKMnames)
#  
#  
#  
#  dffield$millskmname = cut(dffield$MEAS, breaks = millsKM,          labels = millsKMnames)
#  
#    allstats = bind_rows(dffield,ljaerialstats)
#  allstats$year = factor(allstats$year)
#  
#    #Calculate river segment stats
#    segaerialstats = allstats              %>% 
#        tidyr::complete(year,segname) %>% #Fill in missing values for these combinations of factors with NA
#        group_by(year,segname)        %>%
#        summarise(
#              area = sum(area, na.rm=TRUE)
#          )
#  rkmstats = allstats              %>% 
#        tidyr::complete(year,rkmname) %>% #Fill in missing values for these combinations of factors with NA
#        group_by(year,rkmname)        %>%
#        summarise(
#              area = sum(area, na.rm=TRUE)
#          )
#  aldkmstats = allstats              %>% 
#        tidyr::complete(year,aldkmname) %>% #Fill in missing values for these combinations of factors with NA
#        group_by(year,aldkmname)        %>%
#        summarise(
#              area = sum(area, na.rm=TRUE)
#          )
#  
#  
#  
#  mrkmstats = allstats              %>% 
#        tidyr::complete(year,mrkmname) %>% #Fill in missing values for these combinations of factors with NA
#        group_by(year,mrkmname)        %>%
#        summarise(
#              area = sum(area, na.rm=TRUE)
#          )
#  millskmstats = allstats              %>% 
#        tidyr::complete(year,millskmname) %>% #Fill in missing values for these combinations of factors with NA
#        group_by(year,millskmname)        %>%
#        summarise(
#              area = sum(area, na.rm=TRUE)
#          )
#  
#  rkmstats = rkmstats[complete.cases(rkmstats), ]
#  aldkmstats = aldkmstats[complete.cases(aldkmstats), ]
#  mrkmstats = mrkmstats[complete.cases(mrkmstats), ]
#  millskmstats = millskmstats[complete.cases(millskmstats), ]
#  
#  ggplot(temp, aes(x =name, y = area, fill = year)) +
#    geom_bar(stat="identity", position="dodge")               +
#    ylim(0,30000) +
#    xlab("River Rkm") + ylab("Area [m2]") + ggtitle("Lower river logjam area by rkm ") +
#    scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
#                               "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")) 
#   
# lr = ggplot(rkmstats, aes(x =rkmname, y = area, fill = year)) +
#        geom_bar(stat="identity", position="dodge")               +
#        ylim(0,30000) +
#        xlab("River Rkm") + ylab("Area [m2]") + ggtitle("Lower river logjam area by rkm ") +
#        scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
#                                   "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
#  
# ald = ggplot(aldkmstats, aes(x =aldkmname, y = area, fill = year)) +
#        geom_bar(stat="identity", position="dodge")               +
#        ylim(0,30000) +
#        xlab("River Rkm") + ylab("Area [m2]") + ggtitle("Aldwell logjam area by rkm ") +
#        scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
#                                    "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
#  
# mr = ggplot(mrkmstats, aes(x =mrkmname, y = area, fill = year)) +
#        geom_bar(stat="identity", position="dodge")               +
#        ylim(0,30000) +
#        xlab("River Rkm") + ylab("Area [m2]") + ggtitle("Middle River logjam area by rkm ") +
#        scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
#                                  "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
#  
# mills = ggplot(millskmstats, aes(x =millskmname, y = area, fill = year)) +
#        geom_bar(stat="identity", position="dodge")               +
#        ylim(0,30000) +
#        xlab("River Rkm") + ylab("Area [m2]") + ggtitle("Mills logjam area by rkm ") +
#        scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
#                                    "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
# lr
# ald
# mr
# mills
# 
#    t=ggplot(segaerialstats, aes(x=segname, y=area, fill=year))         + 
#        geom_bar(stat="identity", position="dodge")               +
#        xlab("River Segment") + ylab("Logjam area [m2]") + ggtitle("Total logjam area for each river section") +
#       #scale_fill_brewer(type="div", palette=4)
#        scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
#                   "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
# #t
#  
#    
#    #grid.arrange(lr,ald,mr,mills,
#    #            ncol = 2, nrow = 2)
#    ljsbyrkm = segaerialstats
#  ljsbyrkm$logjam_per_km = 0 
#  ljsbyrkm = ljsbyrkm %>% mutate(logjam_per_km=ifelse(segname=='Aldwell',area/4.7,logjam_per_km))
#  ljsbyrkm = ljsbyrkm %>% mutate(logjam_per_km=ifelse(segname=='Lower River',area/7.4,logjam_per_km))
#  ljsbyrkm = ljsbyrkm %>% mutate(logjam_per_km=ifelse(segname=='Middle River',area/9,logjam_per_km))
#  ljsbyrkm = ljsbyrkm %>% mutate(logjam_per_km=ifelse(segname=='MILLS',area/3.6,logjam_per_km))
#  
#  tt=ggplot(ljsbyrkm, aes(x=segname, y=logjam_per_km, fill=year))         + 
#        geom_bar(stat="identity", position="dodge")               +
#        xlab("River Segment") + ylab("Logjam area per rkm [m2/rkm]") + ggtitle("Logjam area per rkm for each river section") +
#        #scale_fill_brewer(type="div", palette=4)
#        scale_fill_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a","#7a0177",
#                                          "#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"))
# #tt
#  
#    
#  