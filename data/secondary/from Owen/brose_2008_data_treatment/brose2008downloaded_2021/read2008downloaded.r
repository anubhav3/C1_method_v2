rm(list=ls())

library(tidyverse)

source("functions/standard.food.web.functions.r")
brose.data = read.delim("brose2008downloaded_2021/downloaded_data/bodysizes_2008.txt",
                        as.is=T,
                        #na.strings = "-999",
                        strip.white = TRUE) %>%
  mutate(Link.reference = str_sub(Link.reference, 1, 33))


## make adjustments
#brose.data <- brose.data[,1:31]
#brose.data[brose.data[,10]=="larvae ",10] = "larvae"
#brose.data[brose.data[,21]=="larvae ",21] = "larvae"

## write link.references to file
all.link.references <- unique(brose.data$Link.reference)
##write(all.link.references, file="all.link.references.txt")

mass.webs <- c(1, 2, 4, 8)##. Others have all lengths.

names(brose.data)

con_mass_col_num <- which(names(brose.data)=="Mean.mass..g..consumer")
res_mass_col_num <- which(names(brose.data)=="Mean.mass..g..resource")
con_length_col_num <- which(names(brose.data)=="Mean.length..m..consumer")
res_length_col_num <- which(names(brose.data)=="Mean.length..m..resource")


## give webs that will be analysed
use.these <- data.frame(web.id=c(1, 2, 3, 10, 11, 12, 14, 15, 16, 19),
                        web.name=c("Benguela Pelagic",
                            "Broadstone Stream",
                            "Skipwith Pond",
                            "Tuesday Lake",
                            "Broom",
                            "Sierra Lakes",
                            "Mill Stream",
                            "Caricaie Lakes",
                            "Grasslands",
                            "Weddell Sea"),
                        c.size.column = c(con_mass_col_num,
                                          con_mass_col_num,
                                          con_length_col_num,
                                          con_mass_col_num,
                                          con_length_col_num,
                                          con_length_col_num,
                                          con_length_col_num,
                                          con_mass_col_num,
                                          con_length_col_num,
                                          con_length_col_num),
                        r.size.column = c(res_mass_col_num,
                                          res_mass_col_num,
                                          res_length_col_num,
                                          res_mass_col_num,
                                          res_length_col_num,
                                          res_length_col_num,
                                          res_length_col_num,
                                          res_mass_col_num,
                                          res_length_col_num,
                                          res_length_col_num))
                        
                        
                        #c.size.column=rep(con_size_col_num,10),
                        #r.size.column=rep(res_size_col_num,10)

                        #c.size.column=c(32, 32, 15, 32, 15, 15, 15, 32, 15, 15),
                        #r.size.column=c(33, 33, 25, 33, 25, 25, 25, 33, 25, 25))

                                        #c.size.column=c(32, 32, 32, 32, 32, 32, 32, 32, 32, 32),
                                        #r.size.column=c(33, 33, 33, 33, 33, 33, 33, 33, 33, 33))

## the webs used are identified by their Link.reference
use.these <- transform(use.these, refs=unique(brose.data$Link.reference)[use.these[,1]])

use.these <- use.these[-10,]
## only keep the data that will be analysed
brose.data <- brose.data[!is.na(match(brose.data$Link.reference, use.these$refs)),]

web.info <- list()
## get web info
for(i in 1:length(use.these[,1])){
    
    web.data <- brose.data[brose.data$Link.reference==unique(brose.data$Link.reference)[i],]

    link.ref = unique(web.data$Link.reference)
    lab1 = rep("link.ref", length(link.ref))

    size.ref = unique(web.data$Body.size.reference)
    lab2 = rep("size.ref", length(size.ref))
    location = unique(web.data$Geographic.location)
    lab3 = rep("location", length(location))
    g.hab = unique(web.data$General.habitat)
    lab4 = rep("g.hab", length(g.hab))
    s.hab = unique(web.data$Specific.habitat)
    lab5 = rep("s.hab", length(s.hab))
    link.method = unique(web.data$Link.method)
    lab6 = rep("link.method", length(link.method))
    size.method = unique(web.data$Body.size.method)
    lab7 = rep("size.method", length(size.method))
    
    web.info[[i]] = c(link.ref, size.ref, location, g.hab, s.hab, link.method, size.method)
    names(web.info[[i]]) <- c(lab1, lab2, lab3, lab4, lab5, lab6, lab7)

}


species.masses <- list()
species.met.cat <- list()
feeding.interaction <- list()
web <- list()
for(i in 1:length(use.these[,1])){
    
    web.data <- brose.data[brose.data$Link.reference==unique(brose.data$Link.reference)[i],]

    if(i==8)
        web.data <- web.data[web.data$Specific.habitat=="Grand Caricaie; marsh dominated by Schoenus nigricans, mown; Scmown2;",]    

    ## Type.of.feeding.interaction
    ##web.data$Type.of.feeding.interaction
    
    ## use consumer common name as name
    if(sum(web.data[,11]==-999)==0)
        c.column = 11
    ## use consumer taxonomy as name
    if(sum(web.data[,9]==-999)==0)
        c.column = 9
    
    ## use common name as name
    if(sum(web.data[,22]==-999)==0)
        r.column = 22
    ## use taxonomy as name
    if(sum(web.data[,20]==-999)==0)
        r.column = 20
    
    ## don't use lifestage
    if(length(unique(web.data[,10]))==1 &  length(unique(web.data[,21]))==1){
        consumer.name = as.character(web.data[,c.column])
        resource.name = as.character(web.data[,r.column])
    }
    ## use lifestage
    if(length(unique(web.data[,10]))!=1 |  length(unique(web.data[,21]))!=1){
        consumer.name = as.character(paste(web.data[,10], web.data[,c.column]))            
        resource.name = as.character(paste(web.data[,21], web.data[,r.column]))
    }

    ## use everything as name
    consumer.name = as.character(paste(web.data[,9], web.data[,10], web.data[,11]))
    resource.name = as.character(paste(web.data[,20], web.data[,21], web.data[,22]))

    ## all species names
    species <- unique(c(consumer.name, resource.name))

    ## get species' masses
    species.masses[[i]] <- rbind(data.frame(species=consumer.name, mass=web.data[,use.these[i,3]]),
                                 data.frame(species=resource.name, mass=web.data[,use.these[i,4]]))   

    ## get species' metabolic categories
    species.met.cat[[i]] <- rbind(data.frame(species=consumer.name, Met.cat= web.data$Metabolic.category.consumer),
                                 data.frame(species=resource.name, Met.cat=web.data$Metabolic.category.consumer))   
    species.met.cat[[i]] <- aggregate(species.met.cat[[i]][,2], list(species=species.met.cat[[i]][,1]), function(x) x[1])
    species.met.cat[[i]] <- species.met.cat[[i]][order(species.met.cat[[i]][,1]),]
    
    ## some names are have multiple masses: take mean
    species.masses[[i]] <- aggregate(species.masses[[i]][,2], list(species=species.masses[[i]][,1]), function(x) mean(x))
    
    ## make matrix form web
    web[[i]] = List.to.matrix(cbind(consumer.name, resource.name), predator.first=TRUE)

    
    ## sort the web matrix by size
    species.masses[[i]] = species.masses[[i]][order(species.masses[[i]][,1]),]
    web[[i]] = web[[i]][order(dimnames(web[[i]])[[1]]), order(dimnames(web[[i]])[[1]])]
    web[[i]] = web[[i]][order(species.masses[[i]][,2]), order(species.masses[[i]][,2])]
    species.masses[[i]]  = species.masses[[i]][order(species.masses[[i]][,2]),]

    ## and sort met.cats by size
    species.met.cat[[i]] <- species.met.cat[[i]][order(species.masses[[i]][order(species.masses[[i]][,1]),][,2]),]
    
    ## check for zero masses
    ##write.table(web.data[web.data[,use.these[i,4]]==0 | web.data[,use.these[i,3]]==0,],
    ##      file=paste("zero.mass", use.these[i,2], ".txt", sep="."), sep="\t", quote=F)


    ## keep type of feeding interaction
    ## Tuesday lake contains multiple records of the same resource consumer pair, get rid of these for the purpose of recording feeding interactions
    feeding.interaction[[i]] <- cbind(consumer.name, resource.name, web.data$Type.of.feeding.interaction)
    feeding.interaction[[i]] <- feeding.interaction[[i]][!duplicated(paste(consumer.name, resource.name)),]
    
    ## For Sierra Lakes, remove the non adult trout species
    if(i==6){
        rm.these1 <- grep("trout", species.masses[[i]][,1])

        rm.these2 <- grep("adult", species.masses[[i]][,1])
        rm.these <- rm.these1[is.na(match(rm.these1, rm.these2))]
        species.masses[[i]] <- species.masses[[i]][-rm.these,]
        species.met.cat[[i]] <- species.met.cat[[i]][-rm.these,]
        web[[i]] <- web[[i]][-rm.these,-rm.these]

        rm.these1 <- grep("trout", feeding.interaction[[i]][,1])
        rm.these2 <- grep("adult", feeding.interaction[[i]][,1])
        rm.these <- rm.these1[is.na(match(rm.these1, rm.these2))]
        rm.these3 <- grep("trout", feeding.interaction[[i]][,2])
        rm.these4 <- grep("adult", feeding.interaction[[i]][,2])
        rm.these <- rm.these3[is.na(match(rm.these3, rm.these4))]
        feeding.interaction[[i]] <- feeding.interaction[[i]][-rm.these,]

    }
    

    
}


for(i in 1:length(web)){
    web.info[[i]] <- c(web.info[[i]], round(Get.web.stats(web[[i]], which.stats=0), 2))
    ##write(web.info[[i]], file=paste(use.these[i,2], ".info.txt", sep=""))
}



pdf("brose2008downloaded_2021/predation_matrices.pdf", width=12, height=8,
    paper = "a4r")
layout(matrix(1:10, 2, 5, byrow=T), respect=T)
for(i in 1:length(use.these[,1])){
  Plot.matrix(web[[i]], title=as.character(use.these[i,2]))
  S = as.numeric(web.info[[i]]["S"])
  L = as.numeric(web.info[[i]]["L"])
  C = as.numeric(web.info[[i]]["C"])
  mtext(side=1, text=paste("S = ", S,
                           "L = ", L,
                           "C = ", C, sep="; "))
}
dev.off()
web.info1 <- use.these
web.info2 <- web.info
web.matrix <- web

remove.these.objects <- ls()[is.na(match(ls(), c("web.info1", "web.info2", "web.matrix", "species.masses", "species.met.cat", "feeding.interaction")))]
rm(list=remove.these.objects)
for(i in 1:9){
    ## next line is true if we have the length of all species in the web
    if(sum(!is.na(species.masses[[i]][,2]))==length(species.masses[[i]][,2]))
        size.unit <- "length"
    all.web.info <- list(web.name=as.character(web.info1[i,2]),
                         species.names=as.character(species.masses[[i]][,1]),
                         species.sizes=species.masses[[i]][,2],
                         species.met.cat=species.met.cat[[i]][,2],
                         feeding.interaction=feeding.interaction[[i]],
                         species.abundance=NA,
                         predation.matrix=web.matrix[[i]])
    ##setwd(paste(path, "data\\by.webs", web.info1[i,2], sep="//"))    
    #setwd(paste("~/work/research/4.in.review/allometric.web/data/by.webs/all.species", web.info1[i,2], sep="/"))    
    saveRDS(all.web.info, file=paste("brose2008downloaded_2021/extracted_foodwebs/", as.character(web.info1[i,2]), ".web.RDS", sep=""))    
}
