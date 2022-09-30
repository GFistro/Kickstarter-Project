library(extrafont)
loadfonts(device = "win")

theme_fistro <- function() {
  theme_minimal() %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      
      plot.title = element_text(
        family = "NimbusSan",
        size = 17,
        hjust = 0.9,
        vjust = -3),
      
      axis.text = element_text(
        family = "NimbusSan"),
      
      plot.background = element_rect(fill = "#FFFFF7")
    )
}




theme_fistro_tick <- function() {
  theme_minimal() %+replace%
    theme(
      panel.grid.major = element_blank(),
      plot.title = element_text(
        family = "NimbusSan",
        size = 17,
        hjust = 0.9,
        vjust = -3),
      
      axis.text = element_text(
        family = "NimbusSan"),
      
      plot.background = element_rect(fill = "#FFFFF7")
    )
}





fonttable <- read.table(header=TRUE, sep=",", stringsAsFactors=FALSE,
                        text='
Short,Canonical
mono,Courier
sans,Helvetica
serif,Times
,AvantGarde
,Bookman
,Helvetica-Narrow
,NewCenturySchoolbook
,Palatino
,URWGothic
,URWBookman
,NimbusMon
URWHelvetica,NimbusSan
,NimbusSanCond
,CenturySch
,URWPalladio
URWTimes,NimbusRom
')

fonttable$pos <- 1:nrow(fonttable)

library(reshape2)
fonttable <- melt(fonttable, id.vars="pos", measure.vars=c("Short","Canonical"),
                  variable.name="NameType", value.name="Font")

# Make a table of faces. Make sure factors are ordered correctly
facetable <- data.frame(Face = factor(c("plain","bold","italic","bold.italic"),
                                      levels = c("plain","bold","italic","bold.italic")))

fullfonts <- merge(fonttable, facetable)


library(ggplot2)
pf <- ggplot(fullfonts, aes(x=NameType, y=pos)) + 
  geom_text(aes(label=Font, family=Font, fontface=Face)) +
  facet_wrap(~ Face, ncol=2)
pf
