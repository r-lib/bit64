#include <R.h>
#include <Rdefines.h>

SEXP r_ram_truly_identical(
  SEXP x_
, SEXP y_
)
{
	SEXP ret_;
	Rboolean ret;
	if(!isVectorAtomic(x_)){
		error("SEXP is not atomic vector");
			return R_NilValue;
	}
	if (TYPEOF(x_)!=TYPEOF(y_)){
		error("vectors don't have identic type");
		return R_NilValue;
	}
	//somehow is DATAPTR not declared: ret = DATAPTR(x_)==DATAPTR(y_) ? TRUE : FALSE;
    switch (TYPEOF(x_)) {
    case CHARSXP:
		ret = CHAR(x_)==CHAR(y_) ? TRUE : FALSE;
	break;
    case LGLSXP:
		ret = LOGICAL(x_)==LOGICAL(y_) ? TRUE : FALSE;
    case INTSXP:
		ret = INTEGER(x_)==INTEGER(y_) ? TRUE : FALSE;
	break;
    case REALSXP:
		ret = REAL(x_)==REAL(y_) ? TRUE : FALSE;
	break;
    case CPLXSXP:
		ret = COMPLEX(x_)==COMPLEX(y_) ? TRUE : FALSE;
	break;
    case STRSXP:
		ret = STRING_PTR(x_)==STRING_PTR(y_) ? TRUE : FALSE;
	break;
    case VECSXP:
		ret = VECTOR_PTR(x_)==VECTOR_PTR(y_) ? TRUE : FALSE;
    case RAWSXP:
		ret = RAW(x_)==RAW(y_) ? TRUE : FALSE;
	break;
    default:
		error("unimplemented type in truly.identical");
		return R_NilValue;
    }
	if (LENGTH(x_)!=LENGTH(y_)){
		ret = FALSE;
	}
	PROTECT( ret_ = allocVector(LGLSXP, 1) );
	INTEGER(ret_)[0] = ret;
	UNPROTECT(1);
	return ret_;
}

