
library(readxl)
eipes_try <- read_excel("~/Downloads/ΕΡΕΥΝΑ ΓΙΑ ΤΗΝ ΙΑΤΡΙΚΗ ΠΑΙΔΕΙΑ ΚΑΙ ΕΡΓΑΣΙΑ (ΕΙΠΕς) (Απαντήσεις).xlsx")

eipes_try <- eipes_try[!is.na(eipes_try$`Χρονική σήμανση`),]


dim(eipes_try)

table(eipes_try$`Φύλο?`)
mean(eipes_try$Ηλικία)
hist(eipes_try$Ηλικία)
table(eipes_try$`Σε τι κατηγορία ανήκετε ;`)
table(eipes_try$`Ειδικότητα που έχετε επιλέξει?`)
table(eipes_try$`Κάνετε ειδικότητα στην?`)


class(eipes_try$`Χρονική σήμανση`)

# Sample POSIXct object
timestamp <- as.POSIXct(eipes_try$`Χρονική σήμανση`)

# Extract the date
eipes_try$date <- format(timestamp, "%d/%m/%Y")

# Determine AM or PM
eipes_try$am_pm <- ifelse(format(timestamp, "%p") == "AM", "AM", "PM")

