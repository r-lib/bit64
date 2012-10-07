# /*
# R-Code for patching S3 generics
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */

#! \name{bit64S3}
#! \alias{bit64S3}
#! \title{
#!   Turn functions S3 generic for bit64 
#! }
#! \description{
#! 	Turn those base functions S3 generic which are used in bit64
#! }
#! \usage{
#! 	bit64S3()
#! }
#! \details{
#!    Some S3 methods for \code{\link{integer64}} cannot be dispatched because the respective function in base R is not generic. Calling \code{bit64S3} turns them generic by assiging them with name extension \code{.default} to \code{\link{globalenv}} and creating a generic in \code{\link{globalenv}}. The following functions are turned generic: 
#!    \preformatted{
#! 	   \code{\link{:}}
#! 	   \code{\link{is.double}}
#! 	   \code{\link{match}}
#! 	   \code{\link{\%in\%}}
#! 	   \code{\link{unique}}
#! 	   \code{\link{table}}
#! 	   \code{\link{rank}}
#! 	   \code{\link{order}}
#!    }
#! }
#! \value{
#! 	\code{\link{invisible}}
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \note{
#! 	\code{\link{is.double}} returns \code{FALSE} for \code{\link{integer64}} after calling \code{bit64S3}.
#! }
#! \seealso{
#! 	\code{\link{bit64}}, \code{\link{S3}}
#! }
#! \examples{
#! 	\dontrun{
#! 		bit64S3()
#! 	}
#! }
#! \keyword{ methods }

bit64S3 <- function(){
	if (!exists(":.default")){
		assign(":.default", get(":"), envir=globalenv())
		assign(":", function(from,to)UseMethod(":"), envir=globalenv())
	}
	assign(":.integer64", function(from, to)seq.integer64(from=from, to=to), envir=globalenv())
		
	if (!exists("is.double.default")){
	  assign("is.double.default", function(x) base::is.double(x), envir=globalenv())
	  assign("is.double", function(x)UseMethod("is.double"), envir=globalenv())
	}
	assign("is.double.integer64", function(x)FALSE, envir=globalenv())

	if (!exists("match.default")){
		assign("match.default", get("match"), envir=globalenv())
		assign("match", function(x, ...)UseMethod("match"), envir=globalenv())
	}
	if (!exists("%in%.default")){
		assign("%in%.default", get("%in%"), envir=globalenv())
		assign("%in%", function(x, ...)UseMethod("%in%"), envir=globalenv())
	}
	if (!exists("unique.default")){
		assign("unique.default", get("unique"), envir=globalenv())
		assign("unique", function(x, ...)UseMethod("unique"), envir=globalenv())
	}
	if (!exists("table.default")){
		assign("table.default", get("table"), envir=globalenv())
		assign("table", function(...)UseMethod("table"), envir=globalenv())
	}
	if (!exists("rank.default")){
		assign("rank.default", get("rank"), envir=globalenv())
		assign("rank", function(x, ...)UseMethod("rank"), envir=globalenv())
	}
	if (!exists("order.default")){
		assign("order.default", get("order"), envir=globalenv())
		assign("order", function(...)UseMethod("order"), envir=globalenv())
	}
	invisible()
}

