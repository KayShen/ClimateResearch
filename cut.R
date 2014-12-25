library(zipcode)
library(mppa)
library(timeDate)
library(maps)
library(geosphere)
setwd("/Users/kshen4/Dropbox/Columbia/Climate Research/R_shinny/")


number_of_flood = 5
allData <- read.csv("/Users/kshen4/Dropbox/Columbia/Climate Research/data/flood.csv", header = T)
allData$Began <- as.Date(allData$Began, "%m/%d/%y")
data(zipcode)
State = rep(NA,nrow(allData))
State_name_list = rep(NA, 55)
counter = 1
for (i in 1:nrow(allData)){
    part = zipcode[(zipcode$latitude<=allData[i,]$latitude_up & zipcode$latitude>=allData[i,]$latitude_down & zipcode$longitude<=allData[i,]$longitude_up & zipcode$longitude >= allData[i,]$longitude_down),]$state
    part2 = na.omit(part)
    if (length(part2) != 0){
        State[i] = part2[1]
        if(State[i] %in% State_name_list){
        }
        else{
            State_name_list[counter] = State[i]
            counter = counter+1
        }
    }
}
allData = cbind(allData, State)
allData = na.omit(allData)
State_name_list = na.omit(State_name_list)
Began_numeric = julian(allData$Began)
allData = cbind(allData, Began_numeric)
del_list = rep(NA, 10)
k = 1
for (i in 1:length(State_name_list)){
    if (nrow(allData[allData$State == State_name_list[i],]) < number_of_flood){
        del_list[k] = State_name_list[i]
        k = k +1
    }
}
del_list = na.omit(del_list)
State_name_list = State_name_list[!State_name_list %in% del_list]


counter2 = 0
line_matrix = matrix(c("NA","NA"), nrow = 1, ncol = 2)

Start = min(allData$Began_numeric)
End = max(allData$Began_numeric)
#sink("/Users/kshen4/Desktop/p-value.txt")
cat("State1", "State2", "p_value",file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value.txt")
cat("\n", file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value.txt", append = TRUE)

cat("State1", "State2", "p_value",file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value_all.txt")
cat("\n", file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value_all.txt", append = TRUE)
for (i in 1:(length(State_name_list))){
    partData_1 = allData[allData$State == State_name_list[i],]
    if (i %% 6 == 1){
        dir = paste("/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/", as.character(i),"-5.pdf", sep = "")
        pdf(dir)
        par(mfrow = c(3,2))
    }
    d = density(partData_1$Began_numeric)
    plot(d, main = State_name_list[i])
    points(partData_1$Began_numeric, rep(0.000005, nrow(partData_1)), type = "h")
    if (i %% 6 == 0){
        dev.off()
    }
    if (i == length(State_name_list)){
        dev.off()
    }
    for (j in i:length(State_name_list)){
        if (j != i){
            partData_2 = allData[allData$State == State_name_list[j],]
#            cor = corrtest(sort(partData_1$Began_numeric), sort(partData_2$Began_numeric), start = Start, end = End)
            cor = corrtest(partData_1$Began_numeric, partData_2$Began_numeric, start = Start, end = End)
            cat(State_name_list[i], State_name_list[j], cor$p.value, file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value_all.txt", append = TRUE)
            cat("\n", file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value_all.txt", append = TRUE)
            if (cor$p.value<0.05){
                cat(State_name_list[i], State_name_list[j], cor$p.value, file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value.txt", append = TRUE)
                cat("\n", file = "/Users/kshen4/Dropbox/Columbia/Climate Research/Flood/result/p-value.txt", append = TRUE)
                counter2 = counter2 +1
                line_matrix = rbind(line_matrix,c(State_name_list[i],State_name_list[j]))

            }
        }

    }
}
line_matrix = line_matrix[-1,]
print(line_matrix)
#sink()
LAT = list()
LON = list()
for (i in 1:length(State_name_list)){
    LAT[State_name_list[i]] = mean(allData[allData$State == State_name_list[i],]$Start_longitude)
    LON[State_name_list[i]] = mean(allData[allData$State == State_name_list[i],]$Start_latitude)
}

map("state")
for (i in 1:nrow(line_matrix)){
    state_1 = line_matrix[i,1]
    print(1)
    state_2 = line_matrix[i,2]
    print(2)
    latlon_1 = c(as.double(LAT[state_1]),as.double(LON[state_1]))
    print(3)
    latlon_2 = c(as.double(LAT[state_2]),as.double(LON[state_2]))
    print(4)
    inter <- gcIntermediate(latlon_1,latlon_2,addStartEnd = TRUE)
    if (state_1 == "AK" || state_2 == "AK"){
        lines(inter, col = "green")
    }
    else if (state_1 == "HI" || state_2 == "HI"){
        lines(inter, col = "blue")
    }
    else{
        lines(inter, col = "red")
    }
}




#....End







loc_1_latitude = 36
loc_1_longitude = -122
loc_2_latitude = 40
loc_2_longitude = -86


loc_1_latitude_lower = loc_1_latitude - 0.1
loc_1_latitude_up = loc_1_latitude + 0.1
loc_1_longitude_lower = loc_1_longitude - 0.1
loc_1_longitude_up = loc_1_longitude + 0.1

loc_2_latitude_lower = loc_2_latitude - 0.1
loc_2_latitude_up = loc_2_latitude + 0.1
loc_2_longitude_lower = loc_2_longitude - 0.1
loc_2_longitude_up = loc_2_longitude + 0.1




partData_1 = allData[(allData$Start_latitude<=loc_1_latitude_up & allData$Start_latitude >= loc_1_latitude_lower & allData$Start_longitude <= loc_1_longitude_up & allData$Start_longitude >= loc_1_longitude_lower),]

partData_2 = allData[(allData$Start_latitude<=loc_2_latitude_up & allData$Start_latitude >= loc_2_latitude_lower & allData$Start_longitude <= loc_2_longitude_up & allData$Start_longitude >= loc_2_longitude_lower),]

all = rbind(partData_1, partData_2)
plot(all$Began, rep(0,nrow(all)),type = "n", ylim = c(0.5, 2.5), ylab = "y", xlab = "Date")
points(partData_1$Began, rep(1,nrow(partData_1)), col = "blue")
points(partData_2$Began, rep(2,nrow(partData_2)), col = "red")

partData_1 <- partData_1[order(partData_1$Began),]
partData_2 <- partData_2[order(partData_2$Began),]
dis_1 = rep(NA,nrow(partData_1))
self_dis_1 = rep(NA,nrow(partData_1))
for (i in 1:nrow(partData_1)){
    temp = partData_2[partData_2$Began <=partData_1[i,]$Began,]$Began
    n = length(temp)
    if (n == 0){
        dis_1[i] = NA
    }
    else{
        dis_1[i] = partData_1[i,]$Began - temp[n]
    }
    if(i != 1){
        self_dis_1[i] = partData_1[i,]$Began - partData_1[i-1,]$Began
    }
}


dis_2 = rep(NA,nrow(partData_2))
self_dis_2 = rep(NA,nrow(partData_2))
for (i in 1:nrow(partData_2)){
    temp = partData_1[partData_1$Began <=partData_2[i,]$Began,]$Began
    n = length(temp)
    if (n == 0){
        dis_2[i] = NA
    }
    else{
        dis_2[i] = partData_2[i,]$Began - temp[n]
    }
    if(i != 1){
        self_dis_2[i] = partData_2[i,]$Began - partData_2[i-1,]$Began
    }
}

par(mfrow = c(2,3))
plot(all$Began, rep(0,nrow(all)),type = "n", ylim = c(0.5, 2.5), ylab = "y", xlab = "Date")
points(partData_1$Began, rep(1,nrow(partData_1)), col = "blue")
points(partData_2$Began, rep(2,nrow(partData_2)), col = "red")
plot(dis_1,self_dis_1)
plot(dis_2,self_dis_2)
y = density(as.numeric(partData_1$Began))
y2 = density(as.numeric(partData_2$Began))
points(as.numeric(partData_1$Began), rep(0.00001, nrow(partData_1)), type = "h")

n = max(allData$Began) - min(allData$Began)
app_1 = rep(0,n)
for (i in 1:nrow(partData_2)){
    app_1[partData_2[i,]$Began - min(allData$Began)] = 1
}
y = density(app, kernel = c("gaussian"), n = 10833)

corrtest(partData_1$Began, partData_2$Began)
