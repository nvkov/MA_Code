#library(sqldf)
#sqldf1 <- sqldf('select * from dftype left join specstype using (Typ)')

#library(plyr)
#plyr1 <- join(df.mini, specstype, by = c("Typ", "Leistung", "Kategorie", "Kraftstoff", "Emission", "Schaltung"))



df.mini$specs<- paste(df.mini$Typ, df$mini$Leistung, df.mini$Kraftstoff, collapse="x")
specs$specs<-paste(specs$Typ, specs$Leistung, specs$Kraftstoff, collapse="x")


df.mini$specs<- unlist(apply(df.mini[,c("Typ", "Leistung", "Kraftstoff" ), with=F], 1, paste, collapse="x"))
df.mini$specs<- gsub(" ", "", df.mini$specs)

specs$specs<- unlist(apply(specs[,c("Typ", "Leistung", "Kraftstoff" ), with=F], 1, paste, collapse="x"))
specs$specs<- gsub(" ", "", specs$specs)

df.mini$specs<- paste(df.mini$Typ, df.mini$Leistung, df.mini$Kraftstoff, collapse="x")
df.mini$Hubraum<- df.mini$Typ %l+% specs[,1:7, with=F]

#========Replace values for Hubraum:
df<- df[,Hubraum := replace(Hubraum, is.na(Hubraum), Mode(na.omit(Hubraum))), by=.(Leistung,Typ,Kategorie, Schaltung, Kraftstoff)]