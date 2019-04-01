
# Author: tim
###############################################################################


getsub <- function(Mat){
	Mat[row(Mat) == (col(Mat) + 1)]
}

# for a single year-sex-edu
wmean <- function(x,w){
	sum(x*w) / sum(w)
}

fac2ch <- function(f){
	as.character(f)
}

# determine relevant column names, per DS's standard naming scheme
# m11, m12, m13 etc
getcols <- function(ntrans = 3, self = TRUE, dead = "4"){
	if (self){
		return(paste0("m",c(t(outer(1:ntrans,1:ntrans,paste0)))))
	} else {
		cols <- outer(1:ntrans,c(1:ntrans,dead),paste0)
		cols <- sort(cols[lower.tri(cols) | upper.tri(cols)])
		cols <- paste0("m",cols)
		return(cols)
	}
}

getcolsall <- function(ntrans,dead=ntrans+1){
	sort(paste0("m",outer(1:ntrans,c(1:ntrans,dead),paste0)))
}

guess_ntrans <- function(DAT){
	sum(grepl(colnames(DAT),pattern="m1")) - 1
	# sum(nchar(colnames(DAT)) == 3 &
	#	substring(colnames(DAT), 1, 1) == "m" &
	#	substring(colnames(DAT), 2, 2) == "1") - 1
}


# functions for figures. geometric color blending
# I love this function, but not presently used.
blend <- function(col1, col2){
	rgb1   <- col2rgb(col1)
	rgb2   <- col2rgb(col2)
	rgbnew <- sqrt((rgb1^2+rgb2^2)/2)
	spatstat::rgb2hex(c(rgbnew))
}
# not presently used!
get_colors <- function(){
	cols1    <- RColorBrewer::brewer.pal(9,"YlOrBr")[5:7]
	cols2    <- RColorBrewer::brewer.pal(11,"RdYlBu")[c(9,3,1)]
	cols3    <- RColorBrewer::brewer.pal(11,"RdYlGn")[c(10,9,1)]
	cols1[3] <- blend(cols1[3],"#6E3A07")
	cols2[3] <- blend(cols2[3],"#6E3A07")
	cols3[3] <- blend(cols3[3],"#6E3A07")
	cols     <- c(cols1,cols2,cols3)
	cols
}


# for heat tables
# this produces a character matrix with latex table entries specifying a
# background color and font color (black or white) with decent contrast.
# value rounded to 2 digits. It's not flexible, just a once-off function.
assign_colors <- function(Tabi, ramp, breaks = seq(-2.4,2.4,by=.1)){
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
				sprintf("%.2f", round(Tabi[,i],2)),"}")
	}
	out
}


# once-off diagnostic plots
plot_prev <- function(prev, type = "bar", scale = FALSE, time = 1995, to = 1,col,...){
	a <- seq(50,110,by=2)
	if (type == "bar"){
		prevm <- as.matrix(prev[prev$time == time, 1:3])[-1, ]
		rownames(prevm) <- NULL
		if (scale){
			prevm <- prevm / rowSums(prevm)
		}
		a10 <-seq(50,110,by=10) - 50
		p10 <- seq(.2,1,by=.2)
		barplot(t(prevm),axes=FALSE,space = 0,width=2,border=NA,xlab = "Age",ylab = ifelse(scale,"Proportion","Prevalence"),col=col,...)
		segments(a10,0,a10,1,col="#FFFFFF50",lwd=.5)
		segments(0,p10,nrow(prevm)*2,p10,col="#FFFFFF50",lwd=.5)
		text(a10,0,a10+50,pos=1,xpd=TRUE)
		text(0,seq(0,1,by=.2),seq(0,1,by=.2),pos=2,xpd=TRUE)
	}
	if (type == 'l'){
		prevt <- matrix(prev[, to], ncol = 3)[-1, ]
		ma    <- colSums(prevt * (a+1)) / colSums(prevt)
		matplot(a, prevt, type = 'l',col=col,xlab = "Age",ylab = "prevalence",...)
		segments(ma,0,ma,1,lwd=1,col=col)
	}
	
}

# this does the barplot representation, which has been superceded by heat tables
barmargins <- function(dec.i,ylim){
	
	sets          <- paste(dec.i$year1,"vs",dec.i$year2)
	code          <- unique(sets)
	recvec        <- 1:length(code)
	names(recvec) <- code
	dec.i$decnr   <- recvec[sets]
	
	sex           <- unique(dec.i$sex)
	sex           <- ifelse(sex == "1.men" ,"m", ifelse(sex == "2.wmn", "f", "b"))
	
	edus          <- c("all_edu", "primary" , "secondary", "terciary"  )
	edus1         <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
	names(edus)   <- edus1
	edu           <- edus[unique(educlevel)]
	
	
	trmargins     <- acast(dec.i, transition ~ state ~ decnr, sum, value.var = "value")
	
	
	trp <- trn <-trmargins
	trp[trp < 0] <- NA
	trn[trn > 0] <- NA
	
	lab.extra <- rownames(trmargins)
	names(lab.extra) <- c("onset 1","onset 2", "mort 1", "rec 1", "onset 2", "mort 2", "rec 2", "rec part", "mort 3")
	
	if (missing(ylim)){
		ylim <- range(pretty(c(apply(trn,3,rowSums,na.rm=TRUE), apply(trp,3,rowSums,na.rm=TRUE))))	
	}
	
	barplot(t(trp[,,1]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = paste(code[1],sex,edu),
			ylab = "contribution to difference in e50", space = 0)
	text(1:length(lab.extra) - .5,ylim[1] - .25,names(lab.extra),srt=90,xpd=TRUE,cex = .8,pos=2)
	barplot(t(trn[,,1]),add=TRUE,space=0)
	
# 1995 vs 2014
	barplot(t(trp[,,2]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = paste(code[2],sex,edu),
			ylab = "contribution to difference in e50",space=0)
	text(1:length(lab.extra) - .5,ylim[1] - .25,names(lab.extra),srt=90,xpd=TRUE,cex = .8,pos=2)
	barplot(t(trn[,,2]),add=TRUE,space=0)
	
# 2004 vs 2014
	barplot(t(trp[,,3]), ylim = range(trmargins), legend.text=c("HLE","ADL1","ADL2p"), main = paste(code[3],sex,edu),
			ylab = "contribution to difference in e50",space=0)
	text(1:length(lab.extra) - .5,ylim[1] - .25,names(lab.extra),srt=90,xpd=TRUE,cex = .8,pos=2)
	barplot(t(trn[,,3]),add=TRUE,space=0)
}
