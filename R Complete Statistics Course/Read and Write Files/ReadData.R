# Save Text file with Tab Seperated by txt extension and CSV file with comma seperated by csv extension
help("read.csv")
?read.csv
?read.table


#Read CSV File
data_csv <- read.csv(file.choose(),header = TRUE)
data_csv

# Read CSV File as a Table but specify Seperator
data_table <- read.table(file.choose(),header = TRUE,sep = ",")
data_table


data_delim <- read.delim(file.choose(),header = T,sep = "\t")
data_delim


# Read CSV File as a Table but specify Seperator
data_table <- read.table(file.choose(),header = TRUE,sep = "\t")
data_table


# Read Excel File 
library(readxl)
LungCapDataExcel <- read_excel(file.choose(),range = "A1:F726", na = "0")
View(LungCapDataExcel)







