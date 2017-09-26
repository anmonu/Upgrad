setwd("D:/Personal/PGDA/Predictive Analysis/Case Study/PA-I_Case_Study_HR_Analytics");

library(reshape2);
in_time_long <- melt(in_time, id.vars = "X");
-----------------------------------------------------------------------------------------

in_time <- read.csv('in_time.csv',stringsAsFactors = FALSE);
out_time <- read.csv('out_time.csv',stringsAsFactors = FALSE);

#eliminating the holiday columns (that have na values for all the employees)
in_time_working_days <-  in_time[,colSums(is.na(in_time))<nrow(in_time)];
out_time_working_days <-  out_time[,colSums(is.na(in_time))<nrow(in_time)];

#melting the data frames from wide format to long format
in_time_working_days_long <- melt(in_time_working_days, id.vars = "X");
out_time_working_days_long <- melt(out_time_working_days, id.vars = "X");

#Merging the in_time and out_time data frames

 nrow(in_time);
 nrow(out_time);
 ncol(in_time);
 ncol(out_time);
 
# Since the number of rows  are same in the in_time and out_time data frames, we can merge them

In_out_time_working_days <- merge(in_time_working_days_long, out_time_working_days_long, by = c("X", "variable"));

#Checking if there are any rows where in_time is na and out_time is present or vice-versa

filter(In_out_time_working_days, is.na(value.x), !is.na(value.y));
filter(In_out_time_working_days, is.na(value.y), !is.na(value.x));

#Setting the date time to correct formats
In_out_time_working_days$value.x <- as.POSIXct(In_out_time_working_days$value.x);
In_out_time_working_days$value.y <- as.POSIXct(In_out_time_working_days$value.y);

# Calculating the hours spent by employee in office

In_out_time_working_days$Office_hours <- round((In_out_time_working_days$value.y - In_out_time_working_days$value.x),2);

#Now calculating the average hours spent in office by an employee for the year - 2015
Average_office_hours <- aggregate(In_out_time_working_days$Office_hours, by=list(In_out_time_working_days$X), FUN=mean, na.rm=TRUE);

#Calculating leaves per employee
Leaves_per_employee <- aggregate(is.na(In_out_time_working_days$value.x), by=list(In_out_time_working_days$X), FUN=sum, na.rm=TRUE);

#Merging the leaves and average office hours information in 1 data frame at employee level
employee_attendance_summary <- merge(Average_office_hours,Leaves_per_employee, by = "Group.1");

#Naming the column headers of the dataframe appropriately
colnames(employee_attendance_summary)[1] = "Employee_id";
colnames(employee_attendance_summary)[2] = "Average_office_hours";
colnames(employee_attendance_summary)[3] = "Leaves_taken";

View(employee_attendance_summary);



