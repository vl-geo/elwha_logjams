
#install.packages(c('sf','ggplot2','dplyr'))
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)
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
