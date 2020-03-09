# We can write data in different format
?write.table

DataToExport <- read.csv(file.choose())

# To remove Index Column built by R use row.names=FALSE it will not generate Sequece of Row Number
write.table(DataToExport,file = "./TableExportWriter.csv" ,sep=",",row.names = FALSE)
write.table(DataToExport,file = "./TableExportWriter.csv" ,sep=" ",row.names = FALSE)



# To write data in csv File we dont need seperator
write.csv(DataToExport,file = "./CSVExportWriter.csv",row.names = FALSE)


# write.csv2 uses , for decimal point and : for seperator
write.csv2(DataToExport,file = "./CSVExportWriter.csv",row.names = FALSE)

