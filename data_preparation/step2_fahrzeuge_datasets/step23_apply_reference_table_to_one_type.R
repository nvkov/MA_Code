specsA180<- specs[specs$Typ=="A180"]

df.mini<- df[is.na(df$Hubraum), ]
df.mini<- df.mini[1:30,]
df.mini<- df.mini[,c(names(specs[,1:7, with=F])), with=F]

keycols=c("Typ", "Leistung")
setkeyv(df.mini, keycols)
setkeyv(specs, keycols)
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