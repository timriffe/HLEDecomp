
# Author: tim
###############################################################################


setwd("/home/tim/git/HLEDecomp/HLEDecomp")
library(reshape2)
library(data.table)
library(xtable)

source("Code/R/Preamble.R")
source("REVES/Pres/R/barplotfunctions.R")
version <- "06"
mspec   <- paste0("mspec", version)
path <- file.path("Data","Tables",mspec)
dir.create(path, recursive = TRUE)


path2     <- file.path("Data", "Results", mspec, "dec", paste0("dec2_all.rds"))
dec2      <- readRDS(path2)
dec2      <- data.table(dec2)
dec2$deci <- paste(dec2$year1,dec2$year2,sep="-")

make_xtable <- function(dec2,.sex="f",.edu="all_edu", .deci="1996-2006",version="06"){
	mspec   <- paste0("mspec", version)
	Tabi <- get_raw_table(dec2,.sex=.sex,.edu=.edu, .deci=.deci,version=version)
	name <- paste0(paste(.sex,.deci,ifelse(.edu=="all_edu","all",.edu),sep="-"),".tex")
	path <- file.path("Data","Tables",mspec)
	
	print(xtable(Tabi),
			hline.after = c(-1,0,4,5),
		    only.contents = TRUE,
			file = file.path(path,name),
			booktabs = TRUE)
}

# make tables
make_xtable(dec2,.sex="f",.edu="all_edu", .deci="1996-2006",version=version )
make_xtable(dec2,.sex="f",.edu="all_edu", .deci="2006-2014",version=version )
make_xtable(dec2,.sex="m",.edu="all_edu", .deci="1996-2006",version=version )
make_xtable(dec2,.sex="m",.edu="all_edu", .deci="2006-2014",version=version )


get_raw_table <- function(dec2,.sex="f",.edu="all_edu", .deci="1996-2006",version="06"){
	mspec   <- paste0("mspec", version)
	decg <- dec2[,list(
					value=sum(value),
					state1 = unique(state1),
					state2=unique(state2),
					year1 =unique(year1),
					year2 = unique(year2)),by=list(sex,edu,transition,deci,statedec)]
	Tabi <- acast(decg[sex == .sex & deci == .deci & edu == .edu], 
			transition~statedec, value.var = "value")
	Tabi <- rbind(Tabi, colSums(Tabi))
	rownames(Tabi)[5] <- "Total"
	rownames(Tabi) <- c("Onset","Die healthy","Recovery","Die disabled","Total")
	colnames(Tabi) <- c("DFLE","DLE","LE")
	Tabi
}
Tabi <- get_raw_table(dec2,.sex="f",.edu="all_edu", .deci="1996-2006",version=version)

library(RColorBrewer)
display.brewer.all()
#decg <- dec2[,list(
#				value=sum(value),
#				state1 = unique(state1),
#				state2=unique(state2),
#				year1 =unique(year1),
#				year2 = unique(year2)),by=list(sex,edu,transition,deci,statedec)]
ramp <- colorRampPalette(rev(brewer.pal(11,"RdBu")),space = "Lab")
ramp <- colorRampPalette(brewer.pal(11,"PRGn"),space = "Lab")

assign_colors <- function(Tabi, ramp, breaks = seq(-2.4,2.4,by=.1),){
	cols <- ramp(length(breaks)-1)
	COLS <- apply(Tabi,2,function(x){
				gsub(pattern="#",replacement="",as.character(cut(x,breaks=breaks,labels=cols)))
			})
	
	TextCol <- ifelse(abs(Tabi) > .8,"FFFFFF","000000")
	out <- COLS
	for (i in 1:ncol(out)){
		out[,i] <- paste0("\\cellcolor[HTML]{",
				COLS[,i],
				"}{\\color[HTML]{",
				TextCol[,i],
				"}", 
				round(Tabi[,i],2),"}")
	}
    out
}
make_xtable <- function(dec2,.sex="f",.edu="all_edu", .deci="1996-2006",version="06",ramp,breaks=seq(-1.5,1.5,by=.1)){
	mspec   <- paste0("mspec", version)
	Tabi <- get_raw_table(dec2,.sex=.sex,.edu=.edu, .deci=.deci,version=version)
	rn <- rownames(Tabi)
	Tabi <- assign_colors(Tabi,ramp=ramp,breaks=breaks)
	rownames(Tabi) <- rn
	name <- paste0(paste(.sex,.deci,ifelse(.edu=="all_edu","all",.edu),sep="-"),".tex")
	path <- file.path("Data","Tables",mspec)
	
	print(xtable(Tabi),
			hline.after = c(-1,0,4,5),
			only.contents = TRUE,
			file = file.path(path,name),
			booktabs = TRUE,
			sanitize.text.function = identity)
}

print(xtable(Tabi),only.contents = TRUE,booktabs = TRUE,sanitize.text.function = identity)
# make tables
make_xtable(dec2,.sex="f",.edu="all_edu", .deci="1996-2006",version=version,ramp=ramp )
make_xtable(dec2,.sex="f",.edu="all_edu", .deci="2006-2014",version=version,ramp=ramp )
make_xtable(dec2,.sex="m",.edu="all_edu", .deci="1996-2006",version=version,ramp=ramp )
make_xtable(dec2,.sex="m",.edu="all_edu", .deci="2006-2014",version=version,ramp=ramp )
