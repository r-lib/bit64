pkgname <- "bit64"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bit64')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("as.character.integer64")
### * as.character.integer64

flush(stderr()); flush(stdout())

### Name: as.character.integer64
### Title: Coerce from integer64
### Aliases: as.character.integer64 as.double.integer64
###   as.integer.integer64 as.logical.integer64 as.bitstring
###   as.bitstring.integer64 as.factor.integer64 as.ordered.integer64
### Keywords: classes manip

### ** Examples

  as.character(lim.integer64())
  as.bitstring(lim.integer64())



cleanEx()
nameEx("as.data.frame.integer64")
### * as.data.frame.integer64

flush(stderr()); flush(stdout())

### Name: as.data.frame.integer64
### Title: integer64: Coercing to data.frame column
### Aliases: as.data.frame.integer64
### Keywords: classes manip

### ** Examples

  as.data.frame.integer64(as.integer64(1:12))
  data.frame(a=1:12, b=as.integer64(1:12))



cleanEx()
nameEx("as.integer64.character")
### * as.integer64.character

flush(stderr()); flush(stdout())

### Name: as.integer64.character
### Title: Coerce to integer64
### Aliases: as.integer64 as.integer64.integer64 as.integer64.NULL
###   as.integer64.character as.integer64.double as.integer64.integer
###   as.integer64.logical as.integer64.factor
### Keywords: classes manip

### ** Examples

  as.integer64(as.character(lim.integer64()))



cleanEx()
nameEx("benchmark64")
### * benchmark64

flush(stderr()); flush(stdout())

### Name: benchmark64
### Title: Function for measuring algorithmic performance of high-level and
###   low-level integer64 functions
### Aliases: benchmark64 optimizer64
### Keywords: misc

### ** Examples

	message("this small example using system.time does not give serious timings\nthis we do this only to run regression tests")
	benchmark64(nsmall=2^10, nbig=2^16, timefun=system.time)
	optimizer64(nsmall=2^10, nbig=2^16, timefun=system.time, plot=FALSE)
## Not run: 
##D 	message("for real measurement of sufficiently large datasets run this on your machine")
##D 	benchmark64()
##D 	optimizer64()
## End(Not run)
	message("let's look at the performance results on Core i7 Lenovo T410 with 8 GB RAM")
	data(benchmark64.data)
	print(benchmark64.data)
	matplot(log2(benchmark64.data[-1,1]/benchmark64.data[-1,]), pch=c("3", "6", "h", "s", "o", "a")
	, xlab="tasks [last=session]", ylab="log2(relative speed) [bigger is better]")
	matplot(t(log2(benchmark64.data[-1,1]/benchmark64.data[-1,])), type="b", axes=FALSE
	, xlab="context", ylab="log2(relative speed) [bigger is better]", lwd=c(rep(1, 14), 3))
	axis(1, labels=c("32-bit", "64-bit", "hash", "sortorder", "order", "hash+sortorder"), at=1:6)
	axis(2)
	data(optimizer64.data)
	print(optimizer64.data)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(2,1))
par(cex=0.7)
for (i in 1:nrow(optimizer64.data)){
	for (j in 1:2){
	  tim <- optimizer64.data[[i,j]]
		barplot(t(tim))
		if (rownames(optimizer64.data)[i]=="match")
			title(paste("match", colnames(optimizer64.data)[j], "in", colnames(optimizer64.data)[3-j]))
		else if (rownames(optimizer64.data)[i]=="%in%")
			title(paste(colnames(optimizer64.data)[j], "%in%", colnames(optimizer64.data)[3-j]))
		else
			title(paste(rownames(optimizer64.data)[i], colnames(optimizer64.data)[j]))
	}
}
par(mfrow=c(1,1))



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("benchmark64.data")
### * benchmark64.data

flush(stderr()); flush(stdout())

### Name: benchmark64.data
### Title: Results of performance measurement on a Core i7 Lenovo T410 8 GB
###   RAM under Windows 7 64bit
### Aliases: benchmark64.data
### Keywords: datasets

### ** Examples

	data(benchmark64.data)
	print(benchmark64.data)
	matplot(log2(benchmark64.data[-1,1]/benchmark64.data[-1,]), pch=c("3", "6", "h", "s", "o", "a")
	, xlab="tasks [last=session]", ylab="log2(relative speed) [bigger is better]")
	matplot(t(log2(benchmark64.data[-1,1]/benchmark64.data[-1,])), type="b", axes=FALSE
	, xlab="context", ylab="log2(relative speed) [bigger is better]", lwd=c(rep(1, 14), 3))
	axis(1, labels=c("32-bit", "64-bit", "hash", "sortorder", "order", "hash+sortorder"), at=1:6)
	axis(2)



cleanEx()
nameEx("bit64-package")
### * bit64-package

flush(stderr()); flush(stdout())

### Name: bit64-package
### Title: A S3 class for vectors of 64bit integers
### Aliases: bit64-package bit64 integer64 is.integer64
###   is.integer.integer64 is.vector.integer64 length<-.integer64
###   print.integer64
### Keywords: package classes manip

### ** Examples

message("Using integer64 in vector")
x <- integer64(8)    # create 64 bit vector
x
is.atomic(x)         # TRUE
is.integer64(x)      # TRUE
is.numeric(x)        # TRUE
is.integer(x)        # FALSE - debatable
is.double(x)         # FALSE - might change
x[] <- 1:2           # assigned value is recycled as usual
x[1:6]               # subscripting as usual
length(x) <- 13      # changing length as usual
x
rep(x, 2)            # replicate as usual
seq(as.integer64(1), 10)     # seq.integer64 is dispatched on first given argument
seq(to=as.integer64(10), 1)  # seq.integer64 is dispatched on first given argument
seq.integer64(along.with=x)  # or call seq.integer64 directly
x <- c(x,runif(length(x), max=100)) # c.integer64 is dispatched only if *first* argument is integer64 ...
x                                   # ... and coerces everything to integer64 - including double
names(x) <- letters  # use names as usual
x

message("Using integer64 in array - note that 'matrix' currently does not work")
message("as.vector.integer64 removed as requested by the CRAN maintainer")
message("as consequence 'array' also does not work anymore")
message("we still can create a matrix or array by assigning 'dim'")
y <- rep(as.integer64(NA), 12)
dim(y) <- c(3,4)
dimnames(y) <- list(letters[1:3], LETTERS[1:4])
y["a",] <- 1:2       # assigning as usual
y
y[1:2,-4]            # subscripting as usual
cbind(E=1:3, F=runif(3, 0, 100), G=c("-1","0","1"), y)  # cbind.integer64 dispatched on any argument and coerces everything to integer64

message("Using integer64 in data.frame")
str(as.data.frame(x))
str(as.data.frame(y))
str(data.frame(y))
str(data.frame(I(y)))
d <- data.frame(x=x, y=runif(length(x), 0, 100))
d
d$x

message("Using integer64 with csv files")
fi64 <- tempfile()
write.csv(d, file=fi64, row.names=FALSE)
e <- read.csv(fi64, colClasses=c("integer64", NA))
unlink(fi64)
str(e)
identical.integer64(d$x,e$x)

message("Serializing and unserializing integer64")
dput(d, fi64)
e <- dget(fi64)
identical.integer64(d$x,e$x)
e <- d[,]
save(e, file=fi64)
rm(e)
load(file=fi64)
identical.integer64(d,e)

### A couple of unit tests follow hidden in a dontshow{} directive ###
  ## Don't show: 
message("Testing identical.integer64")
i64 <- as.double(NA); class(i64) <- "integer64"
stopifnot(identical(unclass(i64-1), unclass(i64+1)))
stopifnot(identical(i64-1, i64+1))
stopifnot(!identical.integer64(i64-1, i64+1))

message("Testing dispatch of 'c' method")
stopifnot(identical.integer64(c(integer64(0), NA), as.integer64(NA)))
message("Dispatch on the second argument fails and we want to be notified once that changes")
stopifnot(!identical.integer64(c(NA, integer64(0)), as.integer64(NA)))

message("Testing minus and plus")
d64 <- c(-.Machine$double.base^.Machine$double.digits, -.Machine$integer.max, -1, 0, 1, .Machine$integer.max, .Machine$double.base^.Machine$double.digits)
i64 <- as.integer64(d64)
stopifnot(identical.integer64(i64-1+1,i64))
stopifnot(identical.integer64(i64+1-1,i64))

message("Testing minus and plus edge cases and 'rev'\n")
stopifnot(identical.integer64(lim.integer64()+1-1, c(lim.integer64()[1], NA)))
stopifnot(identical.integer64(rev(lim.integer64())-1+1, c(lim.integer64()[2], NA)))

message("Testing 'range.integer64', multiplication and integer division")
i64 <- integer64(63)
i64[1] <- 1
for (i in 2:63)
	i64[i] <- 2*i64[i-1]
stopifnot(identical.integer64(i64 * rev(i64), rep(i64[63], 63)))
for (i in 63:2)
	i64[i-1] <- i64[i]%/%2
stopifnot(identical.integer64(i64 * rev(i64), rep(i64[63], 63)))
for (i in 63:2)
	i64[i-1] <- i64[i]/2
stopifnot(identical.integer64(i64 * rev(i64), rep(i64[63], 63)))
stopifnot(identical.integer64(c( -i64[63] - (i64[63]-1), i64[63]+(i64[63]-1) ), lim.integer64()))

stopifnot(identical.integer64(i64[-1]%/%2*as.integer64(2), i64[-1]))
stopifnot(identical.integer64(i64[-1]%/%2L*as.integer64(2), i64[-1]))
stopifnot(identical.integer64(i64[-1]/2*as.integer64(2), i64[-1]))
stopifnot(identical.integer64(i64[-1]/2*as.integer64(2), i64[-1]))

stopifnot(identical.integer64(i64[-63]*2%/%2, i64[-63]))
stopifnot(identical.integer64(i64[-63]*2L%/%2L, i64[-63]))
stopifnot(identical.integer64(as.integer64(i64[-63]*2/2), i64[-63]))
stopifnot(identical.integer64(as.integer64(i64[-63]*2L/2L), i64[-63]))

message("Testing sqrt, power and log")
stopifnot(identical.integer64( as.integer64(sqrt(i64[-1][c(FALSE, TRUE)])*sqrt(i64[-1][c(FALSE, TRUE)])), i64[-1][c(FALSE, TRUE)] ))

stopifnot(identical.integer64(as.integer64(2)^(0:62), i64))
stopifnot(identical.integer64(as.integer64(0:62), as.integer64(log2(i64))))
stopifnot(identical.integer64(as.integer64(log(as.integer64(2)^(0:62), 2)), as.integer64(0:62)))
stopifnot(identical.integer64(as.integer64(round(log(as.integer64(3)^(0:39), 3))), as.integer64(0:39)))
stopifnot(identical.integer64(as.integer64(round(log(as.integer64(10)^(0:18), 10))), as.integer64(0:18)))
stopifnot(identical.integer64(as.integer64(round(log10(as.integer64(10)^(0:18)))), as.integer64(0:18)))

stopifnot(identical.integer64((as.integer64(2)^(1:62))^(1/1:62), as.integer64(rep(2, 62))))
stopifnot(identical.integer64((as.integer64(3)^(1:39))^(1/1:39), as.integer64(rep(3, 39))))
stopifnot(identical.integer64((as.integer64(10)^(1:18))^(1/1:18), as.integer64(rep(10, 18))))

message("Testing c and rep")
stopifnot(identical.integer64( as.integer64(rep(1:3, 1:3)), rep(as.integer64(1:3), 1:3)))
stopifnot(identical.integer64( as.integer64(rep(1:3, 3)), rep(as.integer64(1:3), 3)))
 
x <- as.double(c(NA,NA,NA))
class(x) <- "integer64"
x <- x + -1:1
stopifnot(identical.integer64(rep(x, 3), c(x,x,x) ))
stopifnot(identical.integer64(c.integer64(list(x,x,x), recursive=TRUE), c(x,x,x) ))

message("Testing seq")
stopifnot(identical.integer64(seq(as.integer64(1), 10, 2), as.integer64(seq(1, 10, 2)) ))
stopifnot(identical.integer64(seq(as.integer64(1), by=2, length.out=5), as.integer64(seq(1, by=2, length.out=5)) ))
stopifnot(identical.integer64(seq(as.integer64(1), by=2, length.out=6), as.integer64(seq(1, by=2, length.out=6)) ))
stopifnot(identical.integer64(seq.integer64(along.with=3:5), as.integer64(seq(along.with=3:5)) ))
stopifnot(identical.integer64(seq(as.integer64(1), to=-9), as.integer64(seq(1, to=-9)) ))

message("Testing cbind and rbind")
stopifnot(identical.integer64( cbind(as.integer64(1:3), 1:3), {x <- rep(as.integer64(1:3), 2); dim(x)<-c(3,2);x}))
stopifnot(identical.integer64( rbind(as.integer64(1:3), 1:3), t({x <- rep(as.integer64(1:3), 2); dim(x)<-c(3,2);x})))

message("Testing coercion")
stopifnot(identical( as.double(as.integer64(c(NA, seq(0, 9, 0.25)))), as.double(as.integer(c(NA, seq(0, 9, 0.25))))))
stopifnot(identical( as.character(as.integer64(c(NA, seq(0, 9, 0.25)))), as.character(as.integer(c(NA, seq(0, 9, 0.25))))))
stopifnot(identical( as.integer(as.integer64(c(NA, seq(0, 9, 0.25)))), as.integer(c(NA, seq(0, 9, 0.25)))))
stopifnot(identical( as.logical(as.integer64(c(NA, seq(0, 9, 0.25)))), as.logical(as.integer(c(NA, seq(0, 9, 0.25))))))
stopifnot(identical( as.integer(as.integer64(c(NA, FALSE, TRUE))), as.integer(c(NA, FALSE, TRUE))))
stopifnot(identical( as.integer64(as.integer(as.integer64(-9:9))), as.integer64(-9:9)))
stopifnot(identical( as.integer64(as.double(as.integer64(-9:9))), as.integer64(-9:9)))
stopifnot(identical( as.integer64(as.character(as.integer64(-9:9))), as.integer64(-9:9)))
stopifnot(identical( as.integer64(as.character(lim.integer64())), lim.integer64()))

message("-- testing logical operators --")
stopifnot(identical.integer64(!c(NA, -1:1), !c(as.integer64(NA), -1:1)))
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)&rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))&as.integer64(rep(c(NA, -1:1), rep(4, 4)))))
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)|rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))|as.integer64(rep(c(NA, -1:1), rep(4, 4)))))
stopifnot(identical.integer64(xor(rep(c(NA, -1:1), 4),rep(c(NA, -1:1), rep(4, 4))), xor(as.integer64(rep(c(NA, -1:1), 4)),as.integer64(rep(c(NA, -1:1), rep(4, 4))))))

message("-- testing comparison operators --")
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)==rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))==as.integer64(rep(c(NA, -1:1), rep(4, 4)))))
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)!=rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))!=as.integer64(rep(c(NA, -1:1), rep(4, 4)))))
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)>rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))>as.integer64(rep(c(NA, -1:1), rep(4, 4)))))
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)>=rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))>=as.integer64(rep(c(NA, -1:1), rep(4, 4)))))
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)<rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))<as.integer64(rep(c(NA, -1:1), rep(4, 4)))))
stopifnot(identical.integer64(rep(c(NA, -1:1), 4)<=rep(c(NA, -1:1), rep(4, 4)), as.integer64(rep(c(NA, -1:1), 4))<=as.integer64(rep(c(NA, -1:1), rep(4, 4)))))

message("-- testing vector functions --")
stopifnot(identical.integer64( is.na(as.integer64(c(NA, -1:1))), is.na(c(NA, -1:1)) ))
stopifnot(identical.integer64( format(as.integer64(c(NA, -1:1))), format(c(NA, -1:1)) ))
stopifnot(identical.integer64( abs(as.integer64(c(NA, -1:1))), as.integer64(abs(c(NA, -1:1))) ))
stopifnot(identical.integer64( sign(as.integer64(c(NA, -1:1))), as.integer64(sign(c(NA, -1:1))) ))
stopifnot(identical.integer64( ceiling(as.integer64(c(NA, -1:1))), as.integer64(ceiling(c(NA, -1:1))) ))
stopifnot(identical.integer64( floor(as.integer64(c(NA, -1:1))), as.integer64(floor(c(NA, -1:1))) ))
stopifnot(identical.integer64( trunc(as.integer64(c(NA, -1:1))), as.integer64(trunc(c(NA, -1:1))) ))
stopifnot(identical.integer64( signif(as.integer64(c(NA, -1:1))), as.integer64(c(NA, -1:1)) ))

message("Testing summary functions")
stopifnot(identical(all(as.integer(1)), all(as.integer64(1))))
stopifnot(identical(all(as.integer(0)), all(as.integer64(0))))
stopifnot(identical(all(as.integer(NA)), all(as.integer64(NA))))
stopifnot(identical(all(as.integer(NA), na.rm=TRUE), all(as.integer64(NA), na.rm=TRUE)))
stopifnot(identical(all(as.integer(1), NA), all(as.integer64(1), NA)))
stopifnot(identical(all(as.integer(0), NA), all(as.integer64(0), NA)))
stopifnot(identical(all(as.integer(1), NA, na.rm=TRUE), all(as.integer64(1), NA, na.rm=TRUE)))
stopifnot(identical(all(as.integer(0), NA, na.rm=TRUE), all(as.integer64(0), NA, na.rm=TRUE)))
stopifnot(identical(all(as.integer(c(1, NA))), all(as.integer64(c(1, NA)))))
stopifnot(identical(all(as.integer(c(0, NA))), all(as.integer64(c(0, NA)))))
stopifnot(identical(all(as.integer(c(1, NA)), na.rm=TRUE), all(as.integer64(c(1, NA)), na.rm=TRUE)))
stopifnot(identical(all(as.integer(c(0, NA)), na.rm=TRUE), all(as.integer64(c(0, NA)), na.rm=TRUE)))

stopifnot(identical(any(as.integer(1)), any(as.integer64(1))))
stopifnot(identical(any(as.integer(0)), any(as.integer64(0))))
stopifnot(identical(any(as.integer(NA)), any(as.integer64(NA))))
stopifnot(identical(any(as.integer(NA), na.rm=TRUE), any(as.integer64(NA), na.rm=TRUE)))
stopifnot(identical(any(as.integer(1), NA), any(as.integer64(1), NA)))
stopifnot(identical(any(as.integer(0), NA), any(as.integer64(0), NA)))
stopifnot(identical(any(as.integer(1), NA, na.rm=TRUE), any(as.integer64(1), NA, na.rm=TRUE)))
stopifnot(identical(any(as.integer(0), NA, na.rm=TRUE), any(as.integer64(0), NA, na.rm=TRUE)))
stopifnot(identical(any(as.integer(c(1, NA))), any(as.integer64(c(1, NA)))))
stopifnot(identical(any(as.integer(c(0, NA))), any(as.integer64(c(0, NA)))))
stopifnot(identical(any(as.integer(c(1, NA)), na.rm=TRUE), any(as.integer64(c(1, NA)), na.rm=TRUE)))
stopifnot(identical(any(as.integer(c(0, NA)), na.rm=TRUE), any(as.integer64(c(0, NA)), na.rm=TRUE)))

stopifnot(identical.integer64(as.integer64(sum(c(2, 3, NA))), sum(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(sum(c(2, 3, NA), na.rm=TRUE)), sum(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(sum(c(2, 3, NA))), sum(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(sum(c(2, 3, NA), na.rm=TRUE)), sum(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(sum(2, 3, NA)), sum(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(sum(2, 3, NA, na.rm=TRUE)), sum(as.integer64(2), 3, NA, na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(sum(2, 3, NA)), sum(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(sum(2, 3, NA, na.rm=TRUE)), sum(as.integer64(2), 3, NA, na.rm=TRUE)))

stopifnot(identical.integer64(as.integer64(prod(c(2, 3, NA))), prod(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(prod(c(2, 3, NA), na.rm=TRUE)), prod(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(prod(c(2, 3, NA))), prod(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(prod(c(2, 3, NA), na.rm=TRUE)), prod(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(prod(2, 3, NA)), prod(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(prod(2, 3, NA, na.rm=TRUE)), prod(as.integer64(2), 3, NA, na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(prod(2, 3, NA)), prod(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(prod(2, 3, NA, na.rm=TRUE)), prod(as.integer64(2), 3, NA, na.rm=TRUE)))

stopifnot(identical.integer64(as.integer64(min(c(2, 3, NA))), min(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(min(c(2, 3, NA), na.rm=TRUE)), min(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(min(c(2, 3, NA))), min(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(min(c(2, 3, NA), na.rm=TRUE)), min(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(min(2, 3, NA)), min(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(min(2, 3, NA, na.rm=TRUE)), min(as.integer64(2), 3, NA, na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(min(2, 3, NA)), min(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(min(2, 3, NA, na.rm=TRUE)), min(as.integer64(2), 3, NA, na.rm=TRUE)))

stopifnot(identical.integer64(as.integer64(max(c(2, 3, NA))), max(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(max(c(2, 3, NA), na.rm=TRUE)), max(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(max(c(2, 3, NA))), max(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(max(c(2, 3, NA), na.rm=TRUE)), max(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(max(2, 3, NA)), max(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(max(2, 3, NA, na.rm=TRUE)), max(as.integer64(2), 3, NA, na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(max(2, 3, NA)), max(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(max(2, 3, NA, na.rm=TRUE)), max(as.integer64(2), 3, NA, na.rm=TRUE)))

stopifnot(identical.integer64(as.integer64(range(c(2, 3, NA))), range(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(range(c(2, 3, NA), na.rm=TRUE)), range(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(range(c(2, 3, NA))), range(as.integer64(c(2, 3, NA)))))
stopifnot(identical.integer64(as.integer64(range(c(2, 3, NA), na.rm=TRUE)), range(as.integer64(c(2, 3, NA)), na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(range(2, 3, NA)), range(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(range(2, 3, NA, na.rm=TRUE)), range(as.integer64(2), 3, NA, na.rm=TRUE)))
stopifnot(identical.integer64(as.integer64(range(2, 3, NA)), range(as.integer64(2), 3, NA)))
stopifnot(identical.integer64(as.integer64(range(2, 3, NA, na.rm=TRUE)), range(as.integer64(2), 3, NA, na.rm=TRUE)))

message("-- testing cummulative functions --")
stopifnot(identical.integer64(as.integer64(cumsum(c(2, 3, NA, 1, 4))), cumsum(as.integer64(c(2, 3, NA, 1, 4)))))
stopifnot(identical.integer64(as.integer64(cumprod(c(2, 3, NA, 1, 4))), cumprod(as.integer64(c(2, 3, NA, 1, 4)))))
stopifnot(identical.integer64(as.integer64(cummin(c(2, 3, NA, 1, 4))), cummin(as.integer64(c(2, 3, NA, 1, 4)))))
stopifnot(identical.integer64(as.integer64(cummax(c(2, 3, NA, 1, 4))), cummax(as.integer64(c(2, 3, NA, 1, 4)))))

message("testing diff")
d64 <- diffinv(rep(.Machine$integer.max, 100), lag=2, differences=2)
i64 <- as.integer64(d64)
identical(diff(d64, lag=2, differences=2), as.double(diff(i64, lag=2, differences=2)))

  
## End Don't show

  ## Not run: 
##D message("== Differences between integer64 and int64 ==")
##D require(bit64)
##D require(int64)
##D 
##D message("-- integer64 is atomic --")
##D is.atomic(integer64())
##D is.atomic(int64())
##D str(integer64(3))
##D str(int64(3))
##D 
##D message("-- The following performance numbers are measured under RWin64  --")
##D message("-- under RWin32 the advantage of integer64 over int64 is smaller --")
##D 
##D message("-- integer64 needs 7x/5x less RAM than int64 under 64/32 bit OS (and twice the RAM of integer as it should be) --")
##D as.vector(object.size(int64(1e6))/object.size(integer64(1e6)))
##D as.vector(object.size(integer64(1e6))/object.size(integer(1e6)))
##D 
##D message("-- integer64 creates 2000x/1300x faster than int64 under 64/32 bit OS (and 3x the time of integer) --")
##D t32 <- system.time(integer(1e8))
##D t64 <- system.time(integer64(1e8))
##D T64 <- system.time(int64(1e7))*10  # using 1e8 as above stalls our R on an i7 8 GB RAM Thinkpad
##D T64/t64
##D t64/t32
##D 
##D i32 <- sample(1e6)
##D d64 <- as.double(i32)
##D 
##D message("-- the following timings are rather conservative since timings of integer64 include garbage collection -- due to looped calls")
##D message("-- integer64 coerces 900x/100x faster than int64 under 64/32 bit OS (and 2x the time of coercing to integer) --")
##D t32 <- system.time(for(i in 1:1000)as.integer(d64))
##D t64 <- system.time(for(i in 1:1000)as.integer64(d64))
##D T64 <- system.time(as.int64(d64))*1000
##D T64/t64
##D t64/t32
##D td64 <- system.time(for(i in 1:1000)as.double(i32))
##D t64 <- system.time(for(i in 1:1000)as.integer64(i32))
##D T64 <- system.time(for(i in 1:10)as.int64(i32))*100
##D T64/t64
##D t64/td64
##D 
##D message("-- integer64 serializes 4x/0.8x faster than int64 under 64/32 bit OS (and less than 2x/6x the time of integer or double) --")
##D t32 <- system.time(for(i in 1:10)serialize(i32, NULL))
##D td64 <- system.time(for(i in 1:10)serialize(d64, NULL))
##D i64 <- as.integer64(i32); 
##D t64 <- system.time(for(i in 1:10)serialize(i64, NULL))
##D rm(i64); gc()
##D I64 <- as.int64(i32); 
##D T64 <- system.time(for(i in 1:10)serialize(I64, NULL))
##D rm(I64); gc()
##D T64/t64
##D t64/t32
##D t64/td64
##D 
##D 
##D message("-- integer64 adds 250x/60x faster than int64 under 64/32 bit OS (and less than 6x the time of integer or double) --")
##D td64 <- system.time(for(i in 1:100)d64+d64)
##D t32 <- system.time(for(i in 1:100)i32+i32)
##D i64 <- as.integer64(i32); 
##D t64 <- system.time(for(i in 1:100)i64+i64)
##D rm(i64); gc()
##D I64 <- as.int64(i32); 
##D T64 <- system.time(for(i in 1:10)I64+I64)*10
##D rm(I64); gc()
##D T64/t64
##D t64/t32
##D t64/td64
##D 
##D message("-- integer64 sums 3x/0.2x faster than int64 (and at about 5x/60X the time of integer and double) --")
##D td64 <- system.time(for(i in 1:100)sum(d64))
##D t32 <- system.time(for(i in 1:100)sum(i32))
##D i64 <- as.integer64(i32); 
##D t64 <- system.time(for(i in 1:100)sum(i64))
##D rm(i64); gc()
##D I64 <- as.int64(i32); 
##D T64 <- system.time(for(i in 1:100)sum(I64))
##D rm(I64); gc()
##D T64/t64
##D t64/t32
##D t64/td64
##D 
##D message("-- integer64 diffs 5x/0.85x faster than integer and double (int64 version 1.0 does not support diff) --")
##D td64 <- system.time(for(i in 1:10)diff(d64, lag=2L, differences=2L))
##D t32 <- system.time(for(i in 1:10)diff(i32, lag=2L, differences=2L))
##D i64 <- as.integer64(i32); 
##D t64 <- system.time(for(i in 1:10)diff(i64, lag=2L, differences=2L))
##D rm(i64); gc()
##D t64/t32
##D t64/td64
##D 
##D 
##D message("-- integer64 subscripts 1000x/340x faster than int64 (and at the same speed / 10x slower as integer) --")
##D ts32 <- system.time(for(i in 1:1000)sample(1e6, 1e3))
##D t32<- system.time(for(i in 1:1000)i32[sample(1e6, 1e3)])
##D i64 <- as.integer64(i32); 
##D t64 <- system.time(for(i in 1:1000)i64[sample(1e6, 1e3)])
##D rm(i64); gc()
##D I64 <- as.int64(i32); 
##D T64 <- system.time(for(i in 1:100)I64[sample(1e6, 1e3)])*10
##D rm(I64); gc()
##D (T64-ts32)/(t64-ts32)
##D (t64-ts32)/(t32-ts32)
##D 
##D message("-- integer64 assigns 200x/90x faster than int64 (and 50x/160x slower than integer) --")
##D ts32 <- system.time(for(i in 1:100)sample(1e6, 1e3))
##D t32 <- system.time(for(i in 1:100)i32[sample(1e6, 1e3)] <- 1:1e3)
##D i64 <- as.integer64(i32); 
##D i64 <- system.time(for(i in 1:100)i64[sample(1e6, 1e3)] <- 1:1e3)
##D rm(i64); gc()
##D I64 <- as.int64(i32); 
##D I64 <- system.time(for(i in 1:10)I64[sample(1e6, 1e3)] <- 1:1e3)*10
##D rm(I64); gc()
##D (T64-ts32)/(t64-ts32)
##D (t64-ts32)/(t32-ts32)
##D 
##D 
##D tdfi32 <- system.time(dfi32 <- data.frame(a=i32, b=i32, c=i32))
##D tdfsi32 <- system.time(dfi32[1e6:1,])
##D fi32 <- tempfile()
##D tdfwi32 <- system.time(write.csv(dfi32, file=fi32, row.names=FALSE))
##D tdfri32 <- system.time(read.csv(fi32, colClasses=rep("integer", 3)))
##D unlink(fi32)
##D rm(dfi32); gc()
##D 
##D i64 <- as.integer64(i32); 
##D tdfi64 <- system.time(dfi64 <- data.frame(a=i64, b=i64, c=i64))
##D tdfsi64 <- system.time(dfi64[1e6:1,])
##D fi64 <- tempfile()
##D tdfwi64 <- system.time(write.csv(dfi64, file=fi64, row.names=FALSE))
##D tdfri64 <- system.time(read.csv(fi64, colClasses=rep("integer64", 3)))
##D unlink(fi64)
##D rm(i64, dfi64); gc()
##D 
##D I64 <- as.int64(i32); 
##D tdfI64 <- system.time(dfI64<-data.frame(a=I64, b=I64, c=I64))
##D tdfsI64 <- system.time(dfI64[1e6:1,])
##D fI64 <- tempfile()
##D tdfwI64 <- system.time(write.csv(dfI64, file=fI64, row.names=FALSE))
##D tdfrI64 <- system.time(read.csv(fI64, colClasses=rep("int64", 3)))
##D unlink(fI64)
##D rm(I64, dfI64); gc()
##D 
##D message("-- integer64 coerces 40x/6x faster to data.frame than int64(and factor 1/9 slower than integer) --")
##D tdfI64/tdfi64
##D tdfi64/tdfi32
##D message("-- integer64 subscripts from data.frame 20x/2.5x faster than int64 (and 3x/13x slower than integer) --")
##D tdfsI64/tdfsi64
##D tdfsi64/tdfsi32
##D message("-- integer64 csv writes about 2x/0.5x faster than int64 (and about 1.5x/5x slower than integer) --")
##D tdfwI64/tdfwi64
##D tdfwi64/tdfwi32
##D message("-- integer64 csv reads about 3x/1.5 faster than int64 (and about 2x slower than integer) --")
##D tdfrI64/tdfri64
##D tdfri64/tdfri32
##D 
##D rm(i32, d64); gc()
##D 
##D 
##D message("-- investigating the impact on garbage collection: --")
##D message("-- the fragmented structure of int64 messes up R's RAM --")
##D message("-- and slows down R's gargbage collection just by existing --")
##D 
##D td32 <- double(21)
##D td32[1] <- system.time(d64 <- double(1e7))[3]
##D for (i in 2:11)td32[i] <- system.time(gc(), gcFirst=FALSE)[3]
##D rm(d64)
##D for (i in 12:21)td32[i] <- system.time(gc(), gcFirst=FALSE)[3]
##D 
##D t64 <- double(21)
##D t64[1] <- system.time(i64 <- integer64(1e7))[3]
##D for (i in 2:11)t64[i] <- system.time(gc(), gcFirst=FALSE)[3]
##D rm(i64)
##D for (i in 12:21)t64[i] <- system.time(gc(), gcFirst=FALSE)[3]
##D 
##D T64 <- double(21)
##D T64[1] <- system.time(I64 <- int64(1e7))[3]
##D for (i in 2:11)T64[i] <- system.time(gc(), gcFirst=FALSE)[3]
##D rm(I64)
##D for (i in 12:21)T64[i] <- system.time(gc(), gcFirst=FALSE)[3]
##D 
##D matplot(1:21, cbind(td32, t64, T64), pch=c("d","i","I"), log="y")
##D   
## End(Not run)




cleanEx()
nameEx("bit64S3")
### * bit64S3

flush(stderr()); flush(stdout())

### Name: bit64S3
### Title: Turn functions S3 generic for bit64
### Aliases: bit64S3
### Keywords: methods

### ** Examples

	## Not run: 
##D 		bit64S3()
##D 	
## End(Not run)



cleanEx()
nameEx("c.integer64")
### * c.integer64

flush(stderr()); flush(stdout())

### Name: c.integer64
### Title: Concatenating integer64 vectors
### Aliases: c.integer64 cbind.integer64 rbind.integer64
### Keywords: classes manip

### ** Examples

  c(as.integer64(1), 2:6)
  cbind(1:6, as.integer(1:6))
  rbind(1:6, as.integer(1:6))



cleanEx()
nameEx("cache")
### * cache

flush(stderr()); flush(stdout())

### Name: cache
### Title: Atomic Caching
### Aliases: cache newcache jamcache setcache getcache remcache print.cache
###   still.identical
### Keywords: environment

### ** Examples

	x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
	y <- x
	still.identical(x,y)
	y[1] <- NA
	still.identical(x,y)
	mycache <- newcache(x)
	ls(mycache)
	mycache
	rm(mycache)
	jamcache(x)
	cache(x)
	x[1] <- NA
	cache(x)
	getcache(x, "abc")
	setcache(x, "abc", 1)
	getcache(x, "abc")
	remcache(x)
	cache(x)



cleanEx()
nameEx("cumsum.integer64")
### * cumsum.integer64

flush(stderr()); flush(stdout())

### Name: cumsum.integer64
### Title: Cumulative Sums, Products, Extremes and lagged differences
### Aliases: cummin.integer64 cummax.integer64 cumsum.integer64
###   cumprod.integer64 diff.integer64
### Keywords: classes manip

### ** Examples

  cumsum(rep(as.integer64(1), 12))
  diff(as.integer64(c(0,1:12)))
  cumsum(as.integer64(c(0, 1:12)))
  diff(cumsum(as.integer64(c(0,0,1:12))), differences=2)



cleanEx()
nameEx("duplicated.integer64")
### * duplicated.integer64

flush(stderr()); flush(stdout())

### Name: duplicated.integer64
### Title: Determine Duplicate Elements of integer64
### Aliases: duplicated.integer64
### Keywords: logic manip

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
duplicated(x)

stopifnot(identical(duplicated(x),  duplicated(as.integer(x))))



cleanEx()
nameEx("extract.replace.integer64")
### * extract.replace.integer64

flush(stderr()); flush(stdout())

### Name: extract.replace.integer64
### Title: Extract or Replace Parts of an integer64 vector
### Aliases: [.integer64 [[.integer64 [[<-.integer64 [<-.integer64
### Keywords: classes manip

### ** Examples

  as.integer64(1:12)[1:3]
  x <- as.integer64(1:12)
  dim(x) <- c(3,4)
  x
  x[]
  x[,2:3]



cleanEx()
nameEx("format.integer64")
### * format.integer64

flush(stderr()); flush(stdout())

### Name: format.integer64
### Title: Unary operators and functions for integer64 vectors
### Aliases: format.integer64 is.na.integer64 !.integer64 sign.integer64
###   abs.integer64 sqrt.integer64 log.integer64 log2.integer64
###   log10.integer64 floor.integer64 ceiling.integer64 trunc.integer64
###   round.integer64 signif.integer64
### Keywords: classes manip

### ** Examples

  sqrt(as.integer64(1:12))



cleanEx()
nameEx("hashcache")
### * hashcache

flush(stderr()); flush(stdout())

### Name: hashcache
### Title: Big caching of hashing, sorting, ordering
### Aliases: hashcache sortcache sortordercache ordercache
### Keywords: environment

### ** Examples

	x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
 sortordercache(x)



cleanEx()
nameEx("hashmap")
### * hashmap

flush(stderr()); flush(stdout())

### Name: hashmap
### Title: Hashing for 64bit integers
### Aliases: hashfun hashfun.integer64 hashmap hashmap.integer64 hashpos
###   hashpos.cache_integer64 hashrev hashrev.cache_integer64 hashfin
###   hashfin.cache_integer64 hashrin hashrin.cache_integer64 hashdup
###   hashdup.cache_integer64 hashuni hashuni.cache_integer64 hashmapuni
###   hashmapuni.integer64 hashupo hashupo.cache_integer64 hashmapupo
###   hashmapupo.integer64 hashtab hashtab.cache_integer64 hashmaptab
###   hashmaptab.integer64
### Keywords: programming manip

### ** Examples

x <- as.integer64(sample(c(NA, 0:9)))
y <- as.integer64(sample(c(NA, 1:9), 10, TRUE))
hashfun(y)
hx <- hashmap(x)
hy <- hashmap(y)
ls(hy)
hashpos(hy, x)
hashrev(hx, y)
hashfin(hy, x)
hashrin(hx, y)
hashdup(hy)
hashuni(hy)
hashuni(hy, keep.order=TRUE)
hashmapuni(y)
hashupo(hy)
hashupo(hy, keep.order=TRUE)
hashmapupo(y)
hashtab(hy)
hashmaptab(y)

stopifnot(identical(match(as.integer(x),as.integer(y)),hashpos(hy, x)))
stopifnot(identical(match(as.integer(x),as.integer(y)),hashrev(hx, y)))
stopifnot(identical(as.integer(x) %in% as.integer(y), hashfin(hy, x)))
stopifnot(identical(as.integer(x) %in% as.integer(y), hashrin(hx, y)))
stopifnot(identical(duplicated(as.integer(y)), hashdup(hy)))
stopifnot(identical(as.integer64(unique(as.integer(y))), hashuni(hy, keep.order=TRUE)))
stopifnot(identical(sort(hashuni(hy, keep.order=FALSE)), sort(hashuni(hy, keep.order=TRUE))))
stopifnot(identical(y[hashupo(hy, keep.order=FALSE)], hashuni(hy, keep.order=FALSE)))
stopifnot(identical(y[hashupo(hy, keep.order=TRUE)], hashuni(hy, keep.order=TRUE)))
stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=TRUE)), hashupo(hy, keep.order=TRUE)))
stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=FALSE)), hashupo(hy, keep.order=FALSE)))
stopifnot(identical(hashuni(hy, keep.order=FALSE), hashtab(hy)$values))
stopifnot(identical(as.vector(table(as.integer(y), useNA="ifany")), hashtab(hy)$counts[order.integer64(hashtab(hy)$values)]))
stopifnot(identical(hashuni(hy, keep.order=TRUE), hashmapuni(y)))
stopifnot(identical(hashupo(hy, keep.order=TRUE), hashmapupo(y)))
stopifnot(identical(hashtab(hy), hashmaptab(y)))

	## Not run: 
##D 	message("explore speed given size of the hasmap in 2^hashbits and size of the data")
##D 	message("more hashbits means more random access and less collisions")
##D 	message("i.e. more data means less random access and more collisions")
##D 	bits <- 24
##D 	b <- seq(-1, 0, 0.1)
##D 	tim <- matrix(NA, length(b), 2, dimnames=list(b, c("bits","bits+1")))
##D     for (i in 1:length(b)){
##D 	  n <- as.integer(2^(bits+b[i]))
##D 	  x <- as.integer64(sample(n))
##D 	  tim[i,1] <- repeat.time(hashmap(x, hashbits=bits))[3]
##D 	  tim[i,2] <- repeat.time(hashmap(x, hashbits=bits+1))[3]
##D 	  print(tim)
##D       matplot(b, tim)
##D 	}
##D 	message("we conclude that n*sqrt(2) is enough to avoid collisions")
##D 	
## End(Not run)



cleanEx()
nameEx("identical.integer64")
### * identical.integer64

flush(stderr()); flush(stdout())

### Name: identical.integer64
### Title: Identity function for class 'integer64'
### Aliases: identical.integer64
### Keywords: classes manip

### ** Examples

  i64 <- as.double(NA); class(i64) <- "integer64"
  identical(i64-1, i64+1)
  identical.integer64(i64-1, i64+1)



cleanEx()
nameEx("is.sorted.integer64")
### * is.sorted.integer64

flush(stderr()); flush(stdout())

### Name: is.sorted.integer64
### Title: Small cache access methods
### Aliases: is.sorted.integer64 na.count.integer64 nvalid.integer64
###   nunique.integer64 nties.integer64
### Keywords: environment methods

### ** Examples

	x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
 length(x)
 na.count(x)
 nvalid(x)
 nunique(x)
 nties(x)
 table.integer64(x)
 x



cleanEx()
nameEx("keypos")
### * keypos

flush(stderr()); flush(stdout())

### Name: keypos
### Title: Extract Positions in redundant dimension table
### Aliases: keypos keypos.integer64
### Keywords: manip univar

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
keypos(x)

stopifnot(identical(keypos(x),  match.integer64(x, sort(unique(x), na.last=FALSE))))



cleanEx()
nameEx("match.integer64")
### * match.integer64

flush(stderr()); flush(stdout())

### Name: match.integer64
### Title: 64-bit integer matching
### Aliases: match.integer64 %in%.integer64
### Keywords: manip logic

### ** Examples

x <- as.integer64(c(NA, 0:9), 32)
table <- as.integer64(c(1:9, NA))
match.integer64(x, table)
"%in%.integer64"(x, table)

x <- as.integer64(sample(c(rep(NA, 9), 0:9), 32, TRUE))
table <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
stopifnot(identical(match.integer64(x, table), match(as.integer(x), as.integer(table))))
stopifnot(identical("%in%.integer64"(x, table), as.integer(x) %in% as.integer(table)))

## Not run: 
##D 	message("check when reverse hash-lookup beats standard hash-lookup")
##D 	e <- 4:24
##D 	timx <- timy <- matrix(NA, length(e), length(e), dimnames=list(e,e))
##D 	for (iy in seq_along(e))
##D 	for (ix in 1:iy){
##D 		nx <- 2^e[ix]
##D 		ny <- 2^e[iy]
##D 		x <- as.integer64(sample(ny, nx, FALSE))
##D 		y <- as.integer64(sample(ny, ny, FALSE))
##D 		#hashfun(x, bits=as.integer(5))
##D 		timx[ix,iy] <- repeat.time({
##D 		hx <- hashmap(x)
##D 		py <- hashrev(hx, y)
##D 		})[3]
##D 		timy[ix,iy] <- repeat.time({
##D 		hy <- hashmap(y)
##D 		px <- hashpos(hy, x)
##D 		})[3]
##D 		#identical(px, py)
##D 		print(round(timx[1:iy,1:iy]/timy[1:iy,1:iy], 2), na.print="")
##D 	}
##D 
##D 	message("explore best low-level method given size of x and table")
##D 	B1 <- 1:27
##D 	B2 <- 1:27
##D 	tim <- array(NA, dim=c(length(B1), length(B2), 5), dimnames=list(B1, B2, c("hashpos","hashrev","sortpos1","sortpos2","sortpos3")))
##D 	for (i1 in B1)
##D 	for (i2 in B2)
##D 	{
##D 	  b1 <- B1[i1]
##D 	  b2 <- B1[i2]
##D 	  n1 <- 2^b1
##D 	  n2 <- 2^b2
##D 	  x1 <- as.integer64(c(sample(n2, n1-1, TRUE), NA))
##D 	  x2 <- as.integer64(c(sample(n2, n2-1, TRUE), NA))
##D 	  tim[i1,i2,1] <- repeat.time({h <- hashmap(x2);hashpos(h, x1);rm(h)})[3]
##D 	  tim[i1,i2,2] <- repeat.time({h <- hashmap(x1);hashrev(h, x2);rm(h)})[3]
##D 	  s <- x2[]; o <- seq_along(s); ramsortorder(s, o)
##D 	  tim[i1,i2,3] <- repeat.time(sortorderpos(s, o, x1, method=1))[3]
##D 	  tim[i1,i2,4] <- repeat.time(sortorderpos(s, o, x1, method=2))[3]
##D 	  tim[i1,i2,5] <- repeat.time(sortorderpos(s, o, x1, method=3))[3]
##D 	  rm(s,o)
##D 	  print(apply(tim, 1:2, function(ti)if(any(is.na(ti)))NA else which.min(ti)))
##D 	}
## End(Not run)



cleanEx()
nameEx("optimizer64.data")
### * optimizer64.data

flush(stderr()); flush(stdout())

### Name: optimizer64.data
### Title: Results of performance measurement on a Core i7 Lenovo T410 8 GB
###   RAM under Windows 7 64bit
### Aliases: optimizer64.data
### Keywords: datasets

### ** Examples

data(optimizer64.data)
print(optimizer64.data)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(2,1))
par(cex=0.7)
for (i in 1:nrow(optimizer64.data)){
	for (j in 1:2){
	  tim <- optimizer64.data[[i,j]]
		barplot(t(tim))
		if (rownames(optimizer64.data)[i]=="match")
			title(paste("match", colnames(optimizer64.data)[j], "in", colnames(optimizer64.data)[3-j]))
		else if (rownames(optimizer64.data)[i]=="%in%")
			title(paste(colnames(optimizer64.data)[j], "%in%", colnames(optimizer64.data)[3-j]))
		else
			title(paste(rownames(optimizer64.data)[i], colnames(optimizer64.data)[j]))
	}
}
par(mfrow=c(1,1))



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("plusclass")
### * plusclass

flush(stderr()); flush(stdout())

### Name: plusclass
### Title: integer64: Maintaining S3 class attribute
### Aliases: plusclass minusclass
### Keywords: classes manip internal

### ** Examples

  plusclass("inheritingclass","integer64")
  minusclass(c("inheritingclass","integer64"), "integer64")



cleanEx()
nameEx("prank")
### * prank

flush(stderr()); flush(stdout())

### Name: prank
### Title: (P)ercent (Rank)s
### Aliases: prank prank.integer64
### Keywords: univar

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
prank(x)

x <- x[!is.na(x)]
stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))



cleanEx()
nameEx("qtile")
### * qtile

flush(stderr()); flush(stdout())

### Name: qtile
### Title: (Q)uan(Tile)s
### Aliases: qtile qtile.integer64 quantile.integer64 median.integer64
###   mean.integer64 summary.integer64
### Keywords: univar

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
qtile(x, probs=seq(0, 1, 0.25))
quantile(x, probs=seq(0, 1, 0.25), na.rm=TRUE)
median(x, na.rm=TRUE)
summary(x)

x <- x[!is.na(x)]
stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))



cleanEx()
nameEx("ramsort.integer64")
### * ramsort.integer64

flush(stderr()); flush(stdout())

### Name: ramsort.integer64
### Title: Low-level intger64 methods for in-RAM sorting and ordering
### Aliases: ramsort.integer64 shellsort.integer64 quicksort.integer64
###   mergesort.integer64 radixsort.integer64 ramorder.integer64
###   shellorder.integer64 quickorder.integer64 mergeorder.integer64
###   radixorder.integer64 ramsortorder.integer64 shellsortorder.integer64
###   quicksortorder.integer64 mergesortorder.integer64
###   radixsortorder.integer64
### Keywords: programming manip

### ** Examples

  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  x
  message("ramsort example")
  s <- x[]
  ramsort(s)
  message("s has been changed in-place - whether or not ramsort uses an in-place algorithm")
  s
  message("ramorder example")
  s <- x[]
  o <- seq_along(s)
  ramorder(s, o)
  message("o has been changed in-place - s remains unchanged")
  s
  o
  s[o]
  message("ramsortorder example")
  o <- seq_along(s)
  ramsortorder(s, o)
  message("s and o have both been changed in-place - this is much faster")
  s
  o



cleanEx()
nameEx("rank.integer64")
### * rank.integer64

flush(stderr()); flush(stdout())

### Name: rank.integer64
### Title: Sample Ranks from integer64
### Aliases: rank.integer64
### Keywords: univar

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
rank.integer64(x)

stopifnot(identical(rank.integer64(x),  rank(as.integer(x), na.last="keep", ties.method = "average")))



cleanEx()
nameEx("rep.integer64")
### * rep.integer64

flush(stderr()); flush(stdout())

### Name: rep.integer64
### Title: Replicate elements of integer64 vectors
### Aliases: rep.integer64
### Keywords: classes manip

### ** Examples

  rep(as.integer64(1:2), 6)
  rep(as.integer64(1:2), c(6,6))
  rep(as.integer64(1:2), length.out=6)



cleanEx()
nameEx("seq.integer64")
### * seq.integer64

flush(stderr()); flush(stdout())

### Name: seq.integer64
### Title: integer64: Sequence Generation
### Aliases: seq.integer64
### Keywords: classes manip

### ** Examples

  # colon not activated: as.integer64(1):12
  seq(as.integer64(1), 12, 2)
  seq(as.integer64(1), by=2, length.out=6)



cleanEx()
nameEx("sort.integer64")
### * sort.integer64

flush(stderr()); flush(stdout())

### Name: sort.integer64
### Title: High-level intger64 methods for sorting and ordering
### Aliases: sort.integer64 order.integer64
### Keywords: programming manip

### ** Examples

  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  x
  sort(x)
  message("the following has default optimize='time' which is faster but requires more RAM, this calls 'ramorder'")
  order.integer64(x)
  message("slower with less RAM, this calls 'ramsortorder'")
  order.integer64(x, optimize="memory")



cleanEx()
nameEx("sortnut")
### * sortnut

flush(stderr()); flush(stdout())

### Name: sortnut
### Title: Searching and other uses of sorting for 64bit integers
### Aliases: sortnut sortnut.integer64 ordernut ordernut.integer64 sortfin
###   sortfin.integer64 orderpos orderpos.integer64 orderfin
###   orderfin.integer64 sortorderpos sortorderpos.integer64 orderdup
###   orderdup.integer64 sortorderdup sortorderdup.integer64 sortuni
###   sortuni.integer64 orderuni orderuni.integer64 sortorderuni
###   sortorderuni.integer64 orderupo orderupo.integer64 sortorderupo
###   sortorderupo.integer64 ordertie ordertie.integer64 sortordertie
###   sortordertie.integer64 sorttab sorttab.integer64 ordertab
###   ordertab.integer64 sortordertab sortordertab.integer64 orderkey
###   orderkey.integer64 sortorderkey sortorderkey.integer64 orderrnk
###   orderrnk.integer64 sortorderrnk sortorderrnk.integer64 sortqtl
###   sortqtl.integer64 orderqtl orderqtl.integer64
### Keywords: programming manip

### ** Examples

 message("check the code of 'optimizer64' for examples:")
 print(optimizer64)



cleanEx()
nameEx("sum.integer64")
### * sum.integer64

flush(stderr()); flush(stdout())

### Name: sum.integer64
### Title: Summary functions for integer64 vectors
### Aliases: all.integer64 any.integer64 min.integer64 max.integer64
###   range.integer64 lim.integer64 sum.integer64 prod.integer64
### Keywords: classes manip

### ** Examples

  lim.integer64()
  range(as.integer64(1:12))



cleanEx()
nameEx("table.integer64")
### * table.integer64

flush(stderr()); flush(stdout())

### Name: table.integer64
### Title: Cross Tabulation and Table Creation for integer64
### Aliases: table.integer64
### Keywords: category

### ** Examples

message("pure integer64 examples")
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
y <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
z <- sample(c(rep(NA, 9), letters), 32, TRUE)
table.integer64(x)
table.integer64(x, order="counts")
table.integer64(x, y)
table.integer64(x, y, return="data.frame")

message("via as.integer64.factor we can use 'table.integer64' also for factors")
table.integer64(x, as.integer64(as.factor(z)))

message("via as.factor.integer64 we can also use 'table' for integer64")
table(x)
table(x, exclude=NULL)
table(x, z, exclude=NULL)



cleanEx()
nameEx("tiepos")
### * tiepos

flush(stderr()); flush(stdout())

### Name: tiepos
### Title: Extract Positions of Tied Elements
### Aliases: tiepos tiepos.integer64
### Keywords: manip univar

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
tiepos(x)

stopifnot(identical(tiepos(x),  (1:length(x))[duplicated(x) | rev(duplicated(rev(x)))]))



cleanEx()
nameEx("unipos")
### * unipos

flush(stderr()); flush(stdout())

### Name: unipos
### Title: Extract Positions of Unique Elements
### Aliases: unipos unipos.integer64
### Keywords: manip logic

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
unipos(x)
unipos(x, order="values")

stopifnot(identical(unipos(x),  (1:length(x))[!duplicated(x)]))
stopifnot(identical(unipos(x),  match.integer64(unique(x), x)))
stopifnot(identical(unipos(x, order="values"),  match.integer64(unique(x, order="values"), x)))
stopifnot(identical(unique(x),  x[unipos(x)]))
stopifnot(identical(unique(x, order="values"),  x[unipos(x, order="values")]))



cleanEx()
nameEx("unique.integer64")
### * unique.integer64

flush(stderr()); flush(stdout())

### Name: unique.integer64
### Title: Extract Unique Elements from integer64
### Aliases: unique.integer64
### Keywords: manip logic

### ** Examples

x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
unique(x)
unique(x, order="values")

stopifnot(identical(unique(x),  x[!duplicated(x)]))
stopifnot(identical(unique(x),  as.integer64(unique(as.integer(x)))))
stopifnot(identical(unique(x, order="values"),  as.integer64(sort(unique(as.integer(x)), na.last=FALSE))))



cleanEx()
nameEx("xor.integer64")
### * xor.integer64

flush(stderr()); flush(stdout())

### Name: xor.integer64
### Title: Binary operators for integer64 vectors
### Aliases: &.integer64 |.integer64 xor.integer64 !=.integer64
###   ==.integer64 <.integer64 <=.integer64 >.integer64 >=.integer64
###   +.integer64 -.integer64 *.integer64 ^.integer64 /.integer64
###   %/%.integer64 %%.integer64 binattr
### Keywords: classes manip

### ** Examples

  as.integer64(1:12) - 1



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
