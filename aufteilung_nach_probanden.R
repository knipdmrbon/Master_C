#############################################################################################
#############################################################################################
#
######################
# Variable Description
######################
# d.raw -------------------> raw data; read directly from Unipark-file and not modified
# PP1_columns -------------> columnnames of all product ratings from product group 1
# PP2_columns -------------> columnnames of all product ratings from product group 2
# rook_columns ------------> columnnames of all questions related to rook scale (questions 1 to 9)
# verp_columns ------------> columnnames of all questions related to verplanken scale (questions 1 to 20)
# feelings_columns --------> columnnames of all feelings; that means questions 1 to 4 related to 
#                              warehouse and store respectively
# Ort_columns -------------> columnnames of room-attractiveness; questions 1 to 3 related to 
#                              warehouse and store respectively
# d.cleared ---------------> raw data: rows cleaned by surveys not finished or interupted due to technical issues
#                             columns: 
#                               Ort_PP1, ---> location of product group 1 (either store or warehouse)
#                               Ort_columns, feelings_columns, rook_columns, verp_columns,
#                               ort_1.bewertung, ---> start location of product rating (either store or warehouse)
#                               vpn ---> unique number for each participant
#                             
#############################################################################################
#############################################################################################


# Lade alle Pakete die benötigt werden für die Berechnungen
library(rstudioapi) # Paket zum setzen des "Working-Directories"
library(psych) # Paket für Cronbachs Alpha
library(ggplot2) # Paket für Grafiken
library(reshape2) # Paket für Datentransformation

# Setze das "Working Directory" auf den Ordner wo das .R-File und die Rohdaten liegen
# FAlls der folgende Befehl nicht funktioniert kann man das "Working Directory" auch 
# händisch setzen mittels CTRL + STRG + H
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Einlesen der Rohdaten (Datei muss im "Working Directory" liegen - siehe oben)
d.raw <- read.csv2("export_690239_2018_04_07.csv", header = TRUE, sep = ";", comment.char = "")


################################## Clear the data ##################################

# Übernehme nur die Daten, welche für die spätere Analyse notwendig sind

#### Produktpalette 1:
# Produktpalette 1 zuerst dem Probanden gezeigt (PP11) 
# Produktpalette 1 als 2. dem Probanden gezeigt (PP12)
PP <- c("PP11", "PP12")
# alle möglichen Produktabkürzungen für Produktpalette 1 (siehe Word-Dokument)
products <- c("Tg", "Pg", "Jb", "TuZ", "Rs", "Hh")
# Für jedes Produkt wurden 4 Fragen gestellt
questions <- 1:4
# Erzeuge alle möglichen Kombinationen der Produktpräsentation. (erzeuge alle Spaltenüberschriften
# für die Produktpalette 1). Die Kombinationen haben die Form: [PP11/PP12]_[Tg/Pg/Jb/TuZ/Rs/Hh]_[1/2/3/4]
PP1_columns <- apply(expand.grid(PP, products, questions), 1, function(x) paste(x[1], x[2], x[3], sep="_"))

#### Produktpalette 2
# Produktpalette 2 zuerst dem Probanden gezeigt (PP21) 
# Produktpalette 2 als 2. dem Probanden gezeigt (PP22)
PP <- c("PP21", "PP22")
# alle möglichen Produktabkürzungen für Produktpalette 2 (siehe Word-Dokument)
products <- c("Tw", "Pst", "Js", "TuK", "Rg", "Hb")
# Für jedes Produkt wurden 4 Fragen gestellt
questions <- 1:4
# Erzeuge alle möglichen Kombinationen der Produktpräsentation. (erzeuge alle Spaltenüberschriften
# für die Produktpalette 2). Die Kombinationen haben die Form: [PP21/PP22]_[Tw/Pst/Js/TuK/Rg/Hb]_[1/2/3/4]
PP2_columns <- apply(expand.grid(PP, products, questions), 1, function(x) paste(x[1], x[2], x[3], sep="_"))

#### Impulskauftendenz Rook
rook_columns <- paste0("rook_", 1:9)
#### Impulskauftendenz Verplanken
verp_columns <- paste0("verp_", 1:20)

#### Spaltenüberschriften der Gefühle
feelings_columns <- as.vector(sapply(c("GefV","GefL"),function(x) {paste(x, 1:4, sep="_")}))

#### Attraktivität des Befragungsortes 
# Raumatmosphäre & Lageratmosphäre
Ort <- c("VAtm", "LAtm")
# Für jeden Ort wurden 3 Fragen gestellt
questions <- 1:3
# Erzeuge alle möglichen Kombinationen der für die Atmosphäre - sowohl für Verkaufsraum
# als auch für Lager. Die möglichen Kombinationen sind: [VAtm,LAtm]_[1/2/3]
Ort_columns <- apply(expand.grid(Ort, questions), 1, function(x) paste(x[1], x[2], sep="_"))

# übernehme aus den Rohdaten nur die Spalten für die wir oben definiert haben 
# Spaltenüberschriften generiert haben. Weiters wissen wir dass alle Zeilen mit
# dispcode 31 beendete Fragebögen sind.
d.cleared <- d.raw[d.raw$dispcode == 31, c(PP1_columns, PP2_columns, "Ort_PP1", Ort_columns, feelings_columns, rook_columns, verp_columns, "ort_1.bewertung", "vpn")]

# Fehlende Werte werden mit -77 gekennzeichnet. Ersetze -77 mit NA (not available)
d.cleared[d.cleared == -77] <- NA

# Problem: Wurde Produktpalette 1 als erstes gezeigt so haben wir Werte für die Variablen
# PP11_.... Die entsprechenden Werte für die Variablen PP12 sind daher NA gesetzt. Für die
# spätere Analyse fassen wir die Werte zusammen: d.h. wir überschreiben alle NA - Werte in 
# PP11_... mit den entsprechenden Werten aus PP12_...
tmp <- d.cleared[, grepl("PP11", names(d.cleared))]
tmp2 <- d.cleared[, grepl("PP12", names(d.cleared))]
tmp[is.na(tmp)] <- tmp2[is.na(tmp)]
d.tmp <- tmp
# Analaoge Zusammenführung der Daten wie oben
tmp <- d.cleared[, grepl("PP21", names(d.cleared))]
tmp2 <- d.cleared[, grepl("PP22", names(d.cleared))]
tmp[is.na(tmp)] <- tmp2[is.na(tmp)]

# Zusammenführen der bereinigten Daten und Anreicherung mit Ort_PP1 sowie Atmosphären-Fragen.
# Aus dem Ort_PP1 kann abgeleitet werden ob eine Bewertung im Verkaufsraum oder im Lager stattgefunden hat.
d.cleared <- cbind(d.tmp, tmp , d.cleared[, c("Ort_PP1", Ort_columns, feelings_columns, rook_columns, verp_columns, "ort_1.bewertung", "vpn")])

# Die ersten beiden Zeilen im Datenset enthalten nur Testdaten und müssen für die spätere Analyse ausgeschlossen werden
d.cleared <- d.cleared[-(1:2),]

# Der letzte Buchstabe aus Ort_PP1 gibt an ob Produktpalette 1 im Verkaufsraum (=v) oder im Lager (=h) ausgestellt war.
location_string <- substr(as.character(d.cleared$Ort_PP1), nchar(as.character(d.cleared$Ort_PP1)), nchar(as.character(d.cleared$Ort_PP1))) 
d.cleared$PP1_location <- ifelse(location_string == "v", "Store", "Warehouse")

# Wurde PP1 im Verkaufsraum ausgestellt so ist klar dass Produktpalette 2 im Lager ausgestellt wurde und vice versa. 
d.cleared$PP2_location <- ifelse(d.cleared$PP1_location == "Store", "Warehouse", "Store")


#################################### Datenkorrektur ############################################################
# Bei Lageratmosphäre (LAtm1) ist es in Unipark zu dem Problem gekommen, dass eine 8 in den Daten eigentlich eine 6 sein sollte.
# Bei Verkaufsraumatmosphäre (VAtm1) ist es in Unipark zu dem Problem gekommen, dass eine 9 in den Daten eigentlich eine 6 sein sollte.
# siehe Codebuch (Seite 38 bzw. 36) für weitere Informationen.

d.cleared[d.cleared$LAtm_1 == 8, "LAtm_1"] <- 6
d.cleared[d.cleared$VAtm_1 == 9, "VAtm_1"] <- 6

# Umkodieren von Item 8 rook
d.cleared$rook_8 <- 6 - d.cleared$rook_8

# Umkodieren von Items 1, 3, 7, 8, 9, 11, 13, 15
d.cleared[,paste0("verp_",c(1, 3, 7, 8, 9, 11, 13, 15))] <- sapply(c(1, 3, 7, 8, 9, 11, 13, 15),
                                                                   function(x){d.cleared[,paste0("verp_",x)] <- 8 - d.cleared[,paste0("verp_",x)]})

############################################ prepare the data for the GLM - model #################################################

d.cleared$VAtm_mean <- rowMeans(d.cleared[, grepl("VAtm_[1-3]", names(d.cleared))])
d.cleared$LAtm_mean <- rowMeans(d.cleared[, grepl("LAtm_[1-3]", names(d.cleared))])
d.cleared$rook_mean <- rowMeans(d.cleared[, grepl("rook_[1-9]", names(d.cleared))])
d.cleared$verp_mean <- rowMeans(d.cleared[, grepl("verp_([0-9]|[1-2][0-9])", names(d.cleared))])
d.cleared$impuls_mean <- rowMeans(d.cleared[, c("rook_mean", "verp_mean")])
d.cleared$PP1_means <- rowMeans(d.cleared[, grepl("PP11_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])
d.cleared$PP2_means <- rowMeans(d.cleared[, grepl("PP21_[a-zA-Z]{2,3}_[1-3]", names(d.cleared))])
d.cleared$PP1_location <- as.factor(d.cleared$PP1_location)
d.cleared$PP2_location <- as.factor(d.cleared$PP2_location)

Columns_Verkaufsraum_PP1 <- c("vpn", "PP1_means", "PP1_location", "VAtm_mean", "GefV_1", "GefV_2", "GefV_3", "GefV_4", "impuls_mean", "ort_1.bewertung")
Columns_Lager_PP1 <- c("vpn", "PP1_means", "PP1_location", "LAtm_mean", "GefL_1", "GefL_2", "GefL_3", "GefL_4", "impuls_mean", "ort_1.bewertung")
Columns_Verkaufsraum_PP2 <- c("vpn", "PP2_means", "PP2_location", "VAtm_mean", "GefV_1", "GefV_2", "GefV_3", "GefV_4", "impuls_mean", "ort_1.bewertung")
Columns_Lager_PP2 <- c("vpn", "PP2_means", "PP2_location", "LAtm_mean", "GefL_1", "GefL_2", "GefL_3", "GefL_4", "impuls_mean", "ort_1.bewertung")

d.Columns_Verkaufsraum_PP1 <- d.cleared[d.cleared$PP1_location == "Store", Columns_Verkaufsraum_PP1]
d.Columns_Lager_PP1 <- d.cleared[d.cleared$PP1_location == "Warehouse", Columns_Lager_PP1]
d.Columns_Verkaufsraum_PP2 <- d.cleared[d.cleared$PP2_location == "Store", Columns_Verkaufsraum_PP2]
d.Columns_Lager_PP2 <- d.cleared[d.cleared$PP2_location == "Warehouse", Columns_Lager_PP2]
                                        
columns_names <- c("vpn", "Y_Produktbewertung_gemittelt", "Location", "Raumattraktivität", "Gefühl_1", "Gefühl_2", "Gefühl_3", "Gefühl_4", "impuls_mean", "ort_1.bewertung")

names(d.Columns_Verkaufsraum_PP1) <- columns_names
names(d.Columns_Lager_PP1) <- columns_names
names(d.Columns_Verkaufsraum_PP2) <- columns_names
names(d.Columns_Lager_PP2) <- columns_names

d.GLM_input <- rbind(d.Columns_Verkaufsraum_PP1,d.Columns_Lager_PP1, d.Columns_Verkaufsraum_PP2,d.Columns_Lager_PP2 )
d.GLM_input$Gefühl_mean <- rowMeans(d.GLM_input[,paste0("Gefühl_",1:4)])


d.GLM_input$ort_1.bewertung <- as.factor(d.GLM_input$ort_1.bewertung)

# GLM - Model with all components
fit.all_comp <- glm(Y_Produktbewertung_gemittelt ~ Location+Raumattraktivität+Gefühl_1+Gefühl_2+Gefühl_3+Gefühl_4+impuls_mean+ort_1.bewertung,data=d.GLM_input,family=gaussian)
summary(fit.all_comp)

# GLM - Model Impulskauftendenz with Interaction
fit.loc_impuls <- glm(Y_Produktbewertung_gemittelt ~ Location*impuls_mean,data=d.GLM_input,family=gaussian)
summary(fit.loc_impuls)

# GLM - Model Impulskauftendenz with Interaction
fit.attr_impuls <- glm(Y_Produktbewertung_gemittelt ~ Raumattraktivität*impuls_mean,data=d.GLM_input,family=gaussian)
summary(fit.attr_impuls)

write.csv(d.GLM_input, file = "GLM_input.csv", row.names = FALSE)

############################################ END GLM - model #################################################

############################################ Regression/Moderation  #################################################

# Regression for Store
fit.store <- lm(Y_Produktbewertung_gemittelt ~ Raumattraktivität, data = d.GLM_input[d.GLM_input$Location == "Store",])
summary(fit.store)

# Is the same as the p-value of the coefficient used in the regression
cor.test(d.GLM_input[d.GLM_input$Location == "Store", "Y_Produktbewertung_gemittelt"],
         d.GLM_input[d.GLM_input$Location == "Store", "Raumattraktivität"],
         method = "pearson")

# regression-plot for Produktbewertung VS Raumattraktivität
ggplot(d.GLM_input[d.GLM_input$Location == "Store",],
       aes(x = Raumattraktivität, y = Y_Produktbewertung_gemittelt)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm)   # Add linear regression line (by default includes 95% confidence region)

# Regression for Warehouse
fit.warehouse <- lm(Y_Produktbewertung_gemittelt ~ Raumattraktivität, data = d.GLM_input[d.GLM_input$Location == "Warehouse",])
summary(fit.warehouse)

# Is the same as the p-value of the coefficient used in the regression
cor.test(d.GLM_input[d.GLM_input$Location == "Warehouse", "Y_Produktbewertung_gemittelt"],
         d.GLM_input[d.GLM_input$Location == "Warehouse", "Raumattraktivität"],
         method = "pearson")

# regression-plot for Produktbewertung VS Raumattraktivität
ggplot(d.GLM_input[d.GLM_input$Location == "Warehouse",],
       aes(x = Raumattraktivität, y = Y_Produktbewertung_gemittelt)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm)   # Add linear regression line (by default includes 95% confidence region)

# Regression with Moderation
fit.moderation <- lm(Y_Produktbewertung_gemittelt ~ Raumattraktivität * Gefühl_mean, data = d.GLM_input)
summary(fit.moderation)

############################################ END Regression #################################################

############################################ Correlation of Gefühle #################################################
cor(d.GLM_input[,paste0("Gefühl_", 1:4)])

############################################ Correlation of Gefühle 1 -4 #################################################
cor(d.GLM_input[,paste0("Gefühl_", 1:4)])
cor(d.GLM_input[d.GLM_input$Location == "Store",paste0("Gefühl_", 1:4)])
cor(d.GLM_input[d.GLM_input$Location == "Warehouse",paste0("Gefühl_", 1:4)])

############################################ Correlation of Gefühle_mean & Produktbewertung #################################################
cor(d.GLM_input$Gefühl_mean, d.GLM_input$Y_Produktbewertung_gemittelt)
cor(d.GLM_input[d.GLM_input$Location == "Store", "Gefühl_mean"],
    d.GLM_input[d.GLM_input$Location == "Store", "Y_Produktbewertung_gemittelt"])
cor(d.GLM_input[d.GLM_input$Location == "Warehouse", "Gefühl_mean"],
    d.GLM_input[d.GLM_input$Location == "Warehouse", "Y_Produktbewertung_gemittelt"])
############################################ Correlation of Gefühle_mean & Attraktivität #################################################
cor(d.GLM_input$Gefühl_mean, d.GLM_input$Raumattraktivität)
cor(d.GLM_input[d.GLM_input$Location == "Store", "Gefühl_mean"],
    d.GLM_input[d.GLM_input$Location == "Store", "Raumattraktivität"])
cor(d.GLM_input[d.GLM_input$Location == "Warehouse", "Gefühl_mean"],
    d.GLM_input[d.GLM_input$Location == "Warehouse", "Raumattraktivität"])



