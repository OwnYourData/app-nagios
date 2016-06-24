# version 2016-05-24

# example
# $ Rscript import_nagios.R --ip=abbb::ba27:ebff:fef1:8e19 --host=climateplus_6540 --service=battery

suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(tools))
suppressWarnings(suppressPackageStartupMessages(library(jsonlite)))

# path handling
args <- commandArgs(trailingOnly = F)
scriptPath <- file_path_as_absolute(dirname(sub("--file=","",
                                                args[grep("--file",args)])))
setwd(scriptPath)

# include files
source(paste0(scriptPath, "/oyd_helpers.R"))

# get options -----------------------------------
custom_options <- as.data.frame(matrix(unlist(strsplit(gsub("--", "", 
        args[grepl("--.*=.*", args)]), "=")), ncol=2, byrow=TRUE))
# IP address
ip <- ""
if (nrow(custom_options[as.character(custom_options$V1) == "ip", ]) > 0) {
        ip <- as.character(
                custom_options[as.character(custom_options$V1) == "ip", "V2"])
}

# host
host <- ""
if (nrow(custom_options[as.character(custom_options$V1) == "host", ]) > 0) {
        host <- as.character(
                custom_options[as.character(custom_options$V1) == "host", "V2"])
}

# service
service <- ""
if (nrow(custom_options[as.character(custom_options$V1) == "service", ]) > 0) {
        service <- as.character(
                custom_options[as.character(custom_options$V1) == "service", "V2"])
}

if ((nchar(ip) == 0) |
    (nchar(host) == 0) |
    (nchar(service) == 0)){
        cat("Fehler: fehlende Argumente --ip, --host, --service\n")
        quit()
}

# get data --------------------------------------
#source <- "http://[abbb::ba27:ebff:fef1:8e19]/mysite/pnp4nagios/xport/json?host=climateplus_6540&srv=battery"
source <- paste0("http://[",
                 ip,
                 "]/mysite/pnp4nagios/xport/json?host=",
                 host,
                 "&srv=",
                 service)
hdl  <- GET(source, authenticate("omdadmin", "omd"))
if(validate(content(hdl, "text"))) {
        raw  <- fromJSON(content(hdl, "text"))
} else {
        cat("Fehler: keine gültigen Daten\n")
        quit()
}
df   <- as.data.frame(t(as.matrix(as.data.frame(raw$data$row$v))))
data <- as.numeric(as.character(df[,3]))

cat(paste0(length(data), " records read\n"))
#plot(data, type='l')