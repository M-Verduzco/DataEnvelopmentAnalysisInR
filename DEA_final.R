library(Benchmarking)
library(readxl)
library(tibble)

#LIMPIEZA Y CARGA DE DATOS
data <- read.csv("execum62a210f4bd249.csv", na.strings=c("-","NA", " ", ""))
data[is.na(data)] <- 0
View(data)

data <- data[c(-1, -2, -3, -4, -5, -6, -7), c(-3, -5, -7, -9, -11, -13, -15, -17, -19, -21)]

colnames(data)[1] <- "INSTITUCIONES"
colnames(data)[2] <- "DOCENTES"
colnames(data)[3] <- "PROGRAMAS"
colnames(data)[4] <- "SNI"
colnames(data)[5] <- "PRODEP"
colnames(data)[6] <- "WOS"
colnames(data)[7] <- "SCOPUS"
colnames(data)[8] <- "PATENTES_SOLICITADAS"
colnames(data)[9] <- "PATENTES_OTORGADAS"
colnames(data)[10] <- "PNPC"
colnames(data)[11] <- "COPAES"

rownames(data)[1:20] <- 1:20

Tipo <- c(0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0)

data <- add_column(data, Tipo, .after = "INSTITUCIONES")

data [data == "NA"] <- 0
data [data == "-"] <- 0
data[is.na(data)] <- 0

View(data)
data<- transform(data, DOCENTES=as.numeric(DOCENTES), PROGRAMAS=as.numeric(PROGRAMAS), SNI=as.numeric(SNI), 
                 PRODEP=as.numeric(PRODEP),WOS=as.numeric(WOS), SCOPUS=as.numeric(SCOPUS), 
                 PATENTES_SOLICITADAS=as.numeric(PATENTES_SOLICITADAS),
                 PATENTES_OTORGADAS=as.numeric(PATENTES_OTORGADAS), PNPC=as.numeric(PNPC), COPAES=as.numeric(COPAES))

#Eficiencia 
v_Inputs <- with(data, cbind(DOCENTES, PROGRAMAS)) #entradas
u_Outputs <- with(data, cbind(SNI, PRODEP, WOS, SCOPUS, PATENTES_SOLICITADAS, PATENTES_OTORGADAS, PNPC, COPAES))  #salidas
universidades <- dea(v_Inputs, u_Outputs, SLACK = TRUE, DUAL = TRUE, RTS = "crs", ORIENTATION = "in")
RES <- data.frame(data$INSTITUCIONES, data$Tipo, universidades$eff, universidades$lambda)

#Modificar entradas
Personal_2<- data.frame(data$DOCENTES*RES$universidades.eff)
Programa_2<- data.frame(data$PROGRAMAS*RES$universidades.eff)

Mod_inputs<- cbind(data$INSTITUCIONES,data$Tipo,Personal_2,Programa_2, data$SNI, data$PRODEP,data$WOS, data$SCOPUS, 
                   data$PATENTES_SOLICITADAS,data$PATENTES_OTORGADAS,data$PNPC,data$COPAES)

colnames(Mod_inputs)<- c("INSTITUCIONES_IN", "Tipo_IN", "DOCENTES_IN", "PROGRAMA_IN", "SNI_IN", "PRODEP_IN", "WOS_IN","SCOPUS_IN",
                         "PATENTES_SOLICITADAS_IN", "PATENTES_APROBADAS_IN", 
                         "PNPC_IN", "COPAES_IN")

#Eficiencia nuevas entradas
v_Inputs <- with(Mod_inputs, cbind(DOCENTES_IN, PROGRAMA_IN)) #entradas
u_Outputs <- with(Mod_inputs, cbind(SNI_IN, PRODEP_IN, WOS_IN, SCOPUS_IN, PATENTES_SOLICITADAS_IN, PATENTES_APROBADAS_IN, 
                                    PNPC_IN, COPAES_IN)) #salidas
universidadesI <- dea(v_Inputs, u_Outputs, SLACK = TRUE, DUAL = TRUE, RTS = "crs", ORIENTATION = "in")
RESIN <- data.frame(data$INSTITUCIONES, data$Tipo, universidadesI$eff, universidadesI$lambda)

#Modificar salidas

SNI_2<- data.frame(data$SNI*(1/RES$universidades.eff))
PRODEP_2<- data.frame(data$PRODEP*(1/RES$universidades.eff))
WOS<- data.frame(data$WOS*(1/RES$universidades.eff))
SCOPUS<- data.frame(data$SCOPUS*(1/RES$universidades.eff))
P.Solicitudes<- data.frame(data$PATENTES_SOLICITADAS*(1/RES$universidades.eff))
P.Aprobadas<- data.frame(data$PATENTES_OTORGADAS*(1/RES$universidades.eff))
PNPC_2<- data.frame(data$PNPC*(1/RES$universidades.eff))
COPAES_2<- data.frame(data$COPAES*(1/RES$universidades.eff))

Mod_output<- cbind(data$INSTITUCIONES,data$Tipo,data$DOCENTES,data$PROGRAMAS, SNI_2, PRODEP_2,WOS, SCOPUS, 
                   P.Solicitudes, P.Aprobadas,PNPC_2,COPAES_2)
colnames(Mod_output)<- c("Instituciones", "Tipo", "Personal", "Programas", "SNI", "PRODEP", "WOS", "SCOPUS", "P.Solicitudes", 
                         "P.Aprobadas","PNPC", "COPAES")


#Eficiencia nuevas salidas
v_Inputs <- with(Mod_output, cbind(Personal,Programas)) #entradas
u_Outputs <- with(Mod_output, cbind(SNI, PRODEP, WOS, SCOPUS, P.Solicitudes, P.Aprobadas, PNPC, COPAES)) #salidas
universidadesO <- dea(v_Inputs, u_Outputs, SLACK = TRUE, DUAL = TRUE, RTS = "crs", ORIENTATION = "in")
REOUT <- data.frame(data$INSTITUCIONES, data$Tipo, universidadesO$eff, universidadesO$lambda)

