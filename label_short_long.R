simplifyLabel <- function(old, sep='/', mark='*', replace=NULL) { 
    new <- str_split(old,sep)
    new <- sort(unique(new[[1]]))
    new <- paste(new, collapse=sep)
    if (! is.null(replace)) {
        for (key in names(replace)) {
                new <- str_replace( new, pattern=key, replace[[key]] )
            }
        }
    if (! is.null(mark)) { 
        if (new!=old) {
            new <- paste(c(new,mark), collapse='')
        }
    }
    new
}

lab <- read.csv("data/MP_label_rules.csv")
labels <- labelAttractors(attr, lab, net$genes)
replace <- list('bM1M2'='M1M2', 'il6/M0/M1/M2b'='M1M2', 'il6/M0/M2/M2b'='M2b', 'il6/M0/M2/M2c'='M2c', 'il6/M0/M1/M2'='M1M2', 
                'M1M2/M2/M2d'='M1M2', 'il6/M1/M2b'='M1M2', 'il6/M0/M1'='M1', 'il6/M0/M2'='M2', 'M0/M1/M2b'='M1M2', 'M0/M2/M2b'='M2b', 
                'M0/M2/M2c'='M2c', 'M1/M2/M2b'='M1M2', 'M1/M2/M2c'='M1M2', 'M0/M1/M2'='M1M2', 'il6/M2b'='M2b', 'M1M2/M2'='M1M2', 
                'il6/M0'='il6', 'il6/M1'='M1', 'il6/M1'='M1', 'M1/M2b'='M1M2', 'M2/M2d'='M2d', 'M0/M1'='M1', 'M0/M2'='M2', 'M1/M2'='M1M2')

clean.labels <- list()
for (i in 1:length(labels)) {
    clean.labels[i] <- simplifyLabel(labels[[i]], replace=replace)
    }
clean.labels