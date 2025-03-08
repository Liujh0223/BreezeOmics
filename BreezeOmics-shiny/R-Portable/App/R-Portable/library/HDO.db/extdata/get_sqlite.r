setwd("E:\\enrichplot_export\\DOSE数据更新\\create_dodb\\2022_7_15数据更新")
packagedir <- getwd()
sqlite_path <- paste(packagedir, sep=.Platform$file.sep, "inst", "extdata")
if(!dir.exists(sqlite_path)){dir.create(sqlite_path,recursive = TRUE)}
dbfile <- file.path(sqlite_path, "HDO.sqlite")
unlink(dbfile)

###################################################
### create database
###################################################
## Create the database file
library(RSQLite)
drv <- dbDriver("SQLite")
db <- dbConnect(drv, dbname=dbfile)
## dbDisconnect(db)

source("E:\\enrichplot_export\\HDO.db\\inst\\extdata\\parse-obo.R")
library(dplyr)
## DOTERM
doobo <- parse_do("HumanDO.obo")

doterm <- doobo$doinfo[, c(1,2)]
colnames(doterm) <- c("doid", "term")
HDOTERM <- doterm
dbWriteTable(conn = db, "do_term", HDOTERM, row.names=FALSE, overwrite = TRUE)

######
# dbListTables(db)
# dbListFields(conn = db, "do_term")
# dbReadTable(conn = db,"do_term")
######

## ALIAS
alias <- doobo$alias
colnames(alias) <- c("doid", "alias")
ALIAS <- alias
dbWriteTable(conn = db, "do_alias", ALIAS, row.names=FALSE, overwrite = TRUE)

## SYNONYM
synonym <- doobo$synonym
colnames(synonym) <- c("doid", "synonym")
SYNONYM <- synonym
dbWriteTable(conn = db, "do_synonym", SYNONYM, row.names=FALSE, overwrite = TRUE)

## DOPARENTS
HDOPARENTS <- doobo$rel
colnames(HDOPARENTS) <- c("doid", "parent")
dbWriteTable(conn = db, "do_parent", HDOPARENTS, row.names=FALSE)


## DOCHILDREN
HDOCHILDREN <- doobo$rel[, c(2,1)]
HDOCHILDREN <- HDOCHILDREN[order(HDOCHILDREN[, 1]), ]
colnames(HDOCHILDREN) <- c("doid", "children")
dbWriteTable(conn = db, "do_children", HDOCHILDREN, row.names=FALSE)

## DOANCESTOR
ancestor_list <- split(HDOPARENTS[, 2], HDOPARENTS[, 1])
getAncestor <- function(id) {
    ans_temp <- which(HDOPARENTS[, 1] %in% ancestor_list[[id]])
    ids <- HDOPARENTS[ans_temp, 2]
    content <- c(ancestor_list[[id]], ids)
    while(!all(is.na(ids))) {
        ans_temp <- which(HDOPARENTS[, 1] %in% ids)
        ids <- HDOPARENTS[ans_temp, 2]
        content <- c(content, ids)
    }
    content[!is.na(content)]
}

for (id in names(ancestor_list)) {
    ancestor_list[[id]] <- getAncestor(id)
}
ancestordf <- stack(ancestor_list)[, c(2, 1)]
ancestordf[, 1] <- as.character(ancestordf[, 1])
ancestordf <- unique(ancestordf)
HDOANCESTOR <- ancestordf
colnames(HDOANCESTOR) <- c("doid", "ancestor")
dbWriteTable(conn = db, "do_ancestor", HDOANCESTOR, row.names=FALSE)


# DOOFFSPRING
HDOOFFSPRING <- ancestordf[, c(2, 1)]
HDOOFFSPRING <- HDOOFFSPRING[order(HDOOFFSPRING[, 1]), ]
colnames(HDOOFFSPRING) <- c("doid", "offspring")
dbWriteTable(conn = db, "do_offspring", HDOOFFSPRING, row.names=FALSE)

metadata <-rbind(c("DBSCHEMA","HDO_DB"),
        c("DBSCHEMAVERSION","1.0"),
        c("HDOSOURCENAME","Disease Ontology"),
        c("HDOSOURCURL","https://github.com/DiseaseOntology/HumanDiseaseOntology/blob/main/src/ontology/HumanDO.obo"),
        c("HDOSOURCEDATE","20220706"),
        c("Db type", "HDODb"))
        #c("DOVERSION","2806"))	

metadata <- as.data.frame(metadata)
colnames(metadata) <- c("name", "value") 
dbWriteTable(conn = db, "metadata", metadata, row.names=FALSE, overwrite = TRUE)



map.counts<-rbind(c("TERM", nrow(HDOTERM)),
        # c("OBSOLETE","$obsolete_counts"),
        c("CHILDREN", nrow(HDOCHILDREN)),
        c("PARENTS", nrow(HDOPARENTS)),
        c("ANCESTOR", nrow(HDOANCESTOR)),
        c("OFFSPRING", nrow(HDOOFFSPRING)))


map.counts <- as.data.frame(map.counts)
colnames(map.counts) <- c("map_name","count")
# dbWriteTable(conn = db, "map.counts", map.counts, row.names=FALSE)
dbWriteTable(conn = db, "map_counts", map.counts, row.names=FALSE, overwrite = TRUE)

dbListTables(db)
dbListFields(conn = db, "metadata")
dbReadTable(conn = db,"metadata")


map.metadata <- rbind(c("TERM", "Disease Ontology", "https://github.com/DiseaseOntology/HumanDiseaseOntology/blob/main/src/ontology/HumanDO.obo","20220706"),
            # c("OBSOLETE", "Disease Ontology", "https://github.com/DiseaseOntology/HumanDiseaseOntology/blob/main/src/ontology/HumanDO.obo","20220706"),
            c("CHILDREN", "Disease Ontology", "https://github.com/DiseaseOntology/HumanDiseaseOntology/blob/main/src/ontology/HumanDO.obo","20220706"),
            c("PARENTS", "Disease Ontology", "https://github.com/DiseaseOntology/HumanDiseaseOntology/blob/main/src/ontology/HumanDO.obo","20220706"),
            c("ANCESTOR", "Disease Ontology", "https://github.com/DiseaseOntology/HumanDiseaseOntology/blob/main/src/ontology/HumanDO.obo","20220706"),
            c("OFFSPRING", "Disease Ontology", "https://github.com/DiseaseOntology/HumanDiseaseOntology/blob/main/src/ontology/HumanDO.obo","20220706"))	
map.metadata <- as.data.frame(map.metadata)
colnames(map.metadata) <- c("map_name","source_name","source_url","source_date")
dbWriteTable(conn = db, "map_metadata", map.metadata, row.names=FALSE, overwrite = TRUE)


dbListTables(db)
dbListFields(conn = db, "map_metadata")
dbReadTable(conn = db,"map_metadata")
dbDisconnect(db)

