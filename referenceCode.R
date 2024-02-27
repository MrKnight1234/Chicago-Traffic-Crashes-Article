# import that dataset.  
df <- read.csv('ChicagoTrafficCrashesData.csv')
#Link: https://www.kaggle.com/datasets/anoopjohny/traffic-crashes-crashes/data

# Head and tail to get rough idea of the data set
head(df)
tail(df)

# # Head and tail is outputted oddly, but the main purpose was see in case 
# the tail of some columns was filled with NANs or 0s meaning all data for 
# the columns did not end the same  

# Get summary of data. 
summary(df)

# From the following code output I looked at the numeric data first.
# first I found the POSTED_SPEED_LIMIT to be interesting as the data 
# seemed to be concentrated around 28 (referencing the mean of 28.4). 
# To drive this point further, the 1st and 3rd Qu. along with 
# the median are all 30.0. Overall I found this odd so I thought to 
# view this column on a plot to gain more insight.   

# Assign the colors then make a box plot to see this concentration around 30 much better. 
# Although it's the mostly the same information from the summary, It'll be more helpful 
# when showing this in the article because graphs are easier to understand than just numbers.
colors <- c('red', 'blue', 'cyan', 'yellow', 'green') # Assigning the colors
boxplot(df$POSTED_SPEED_LIMIT, xlab="Speed Limit", ylab="Speed Limit", main="Boxplot of the Speed limit at crash locations", col=colors, border="black")

# After inspecting the box plot, it seems that the lower the speed limit is, the more liker
# you are to get in a crash in Chicago. That statement seems like the makings of a good
# click bait title but I'd like to do more research and possibly provide some reasoning

# Create a subset of df where the speed limit is less than 30 and only for the longitude 
# and latitude cols. I think that location could be a major culprit behind these
# accidents at a these low speed limit, like if the roadways have many turns, are too narrow, 
# or even have many pedestrians. That's why I think that the location of where the speed 
# limits are can help see if how the road is setup can be causing the accident. 
df_spdLmt <- subset(df, POSTED_SPEED_LIMIT <= 30, select = c(LONGITUDE, LATITUDE))

# We can then take this and make a scatter plot and see where most of these accidents occur.
# If there is 'hot-spots' where there is high concentration, we can see if the roads
# are setup in a way that can increase the chances of accidents. After running it once
# without limits on x and y, it was too zoomed out so I had to add that the second time I 
# ran the code and modified the limits until it became more clear. 
plot(df_spdLmt$LONGITUDE, df_spdLmt$LATITUDE, ylab = "latitude", xlab = "longitude", main = "lat. and long. for accidents in 30mph speed limit", 
     col = "red", xlim = c(-87.5, -88),
     ylim = c(41.5, 42.5))

# After making it more zoomed in, the coverage is still pretty large as there is a 
# .8 latitude and .5 longitude coverage and it's concentrated in one area. But the scatter plot is most dense between
# 41.7 - 42.0 latitude and -87.65 - -87.75 longitude. I picked -87.7 longitude 41.8 latitude
# as a point in this area to get an idea of where this area is covering.
# Sky-view shows grid-like road patterns so many intersections. Moreover,
# street-view shows neighborhoods with very narrow streets with cars parked on each side
# of the road. Walking down, you can see pedestrian cross walks that can also allow
# for more traffic and subsequent accidents. Shows that these accidents at lower speed limits
# can be the result of bad road layouts and lack of space. 
# Reference link of the street: https://www.google.com/maps/@41.7997019,-87.7000749,3a,75y,179.38h,67.87t/data=!3m7!1e1!3m5!1sj-SRItm3CpMzSOBug66epg!2e0!6shttps:%2F%2Fstreetviewpixels-pa.googleapis.com%2Fv1%2Fthumbnail%3Fpanoid%3Dj-SRItm3CpMzSOBug66epg%26cb_client%3Dsearch.revgeo_and_fetch.gps%26w%3D96%26h%3D64%26yaw%3D341.17978%26pitch%3D0%26thumbfov%3D100!7i16384!8i8192?entry=ttu

# Although we have found one possible reason, I'd like one more that is more data
# oriented to maybe find a good predictor for when a accident is most likely to occur.

# I want to specifically look into hit-and-runs at this speed limit because I feel 
# like this data might be subject to selection bias and that may have cause the data to seem 
# to converge at a low speed limit of 30. I feel like some accidents (especially the ones on 
# higher speed limits) may not be reported because it in some hit and run situations there 
# was no recorded information (like a license plate) to find the person who caused the accident or 
# even they got a away quickly because of the high speed limit. It may also be assumed the driver is lying.
# This becomes more important prominent when you consider the majority of crashes were hit-and-runs, the code 
# below shows it.

# Table to of crash type column and then print it to show how 'drive away' or hit-and-runs are the majority.
# Showing that it is a big issue while driving in Chicago and can open the possibility of selection bias.
crashType <- table(df$CRASH_TYPE)
print(crashType)

# But before proving anything, I need to outline a possible method that can shed more light on the situation.
# Below I make a table and print the number of accidents that are above and below the 30 mph speed limit 
# with the Report_type. If the number of hit-and-run accidents above the 30 mph speed limit are less than 
# the normal 'towed' crashes and the opposite is true for below then must be something odd 
# happening and the data might be subject to bias as people causing hit-and-runs at higher speed limits can easily escape. 

crashPartOne <- table(subset(df, POSTED_SPEED_LIMIT <= 30, select = c(CRASH_TYPE))) #below or equal to 30 speed limit
print(crashPartOne)

crashPartTwo <- table(subset(df, POSTED_SPEED_LIMIT > 30, select = c(CRASH_TYPE))) #above 30 speed limit
print(crashPartTwo)

# It did not end up being that case. The table for above 30 mph speed limit for hit-and-runs was still greater and 
# the same for the table for less/equal to the 30 mph mark. There might be selection bias occurring BUT THAT DATA
# DOES NOT POINT TO SUCH A CONCLUSION. But there is some things we can extract from these two tables.
# We can say that because the table for the crash types had the highest
# count before 30 mph limit and we know the road layout of where these speed limits are located --> We can understand why
# the count is so high because of how narrow the roads are. I can also say with high certainty that the reason why the
# number of hit-and-runs is so high is because (as I've mentioned from the google maps part) there are a lot of cars parked
# on BOTH sides of these narrow roads making it easy to hit a parked car and just drive away so that the owner can report
# it as a hit-and-run. 


# Now I will move onto the categorical predictor. 




