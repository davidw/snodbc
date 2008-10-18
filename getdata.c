/* getdata.c - C implementations of common operations for snodbc, by
 * David N. Welton <davidw@dedasys.com
 *
 * This code is released under the same terms as Tcl. */

#include <stdlib.h>
#include <tcl.h>

#define DONT_TD_VOID 1
#include <sql.h>

/* These get set up in the init function, as we reuse them many
 * times. */

static Tcl_Obj *options = NULL;
static Tcl_Obj *nulloption = NULL;
static Tcl_Obj *encodingoption = NULL;

/*
 *-----------------------------------------------------------------------------
 *
 * odbc_tcl_getencoding --
 *
 * 	Fill in the encoding variable pointer, by looking at the
 * 	$options(-encoding) array element
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

static int
odbc_tcl_getencoding(Tcl_Interp *interp, Tcl_Encoding *encoding) {
    Tcl_Obj *encodingObj = Tcl_ObjGetVar2(interp, options, encodingoption, 0);
    if (Tcl_GetEncodingFromObj(interp, encodingObj, encoding) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}


/*
 *-----------------------------------------------------------------------------
 *
 * odbc_getdata --
 *
 * 	Fetch the data for a single column, putting it in the Tcl
 * 	result.  This is where most of the real work is done.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

static int
odbc_getdata(Tcl_Interp *interp, SQLHSTMT StatementHandle, int ColumnNumber, Tcl_Encoding encoding) {
    int bufsize = 2048;
    int retval = 0;

    SQLSMALLINT TargetType;
    char TargetValue[2048];
    SQLLEN BufferLength;
    SQLLEN StrLen_or_Ind;

    char ColumnName[bufsize];
    SQLSMALLINT NameLength;
    SQLSMALLINT DataType;
    SQLLEN ColumnSize;
    SQLSMALLINT DecimalDigits;
    SQLSMALLINT Nullable;

    int addsize = 0;

    Tcl_Obj *tclres = NULL;
    Tcl_Obj *data = NULL;

    retval = SQLDescribeCol(StatementHandle,
			    ColumnNumber, ColumnName,
			    bufsize, &NameLength,
			    &DataType, &ColumnSize,
			    &DecimalDigits, &Nullable);

    /* From ctype_from_sqltype in classes.tcl */
    if (DataType>=-10 && DataType <= -8) {
	TargetType = -8;
    } else if (DataType>=-4 && DataType <=-2 || DataType == -370) {
	TargetType = -2;
    } else {
	TargetType = 1;
    }

    /* We loop through so as to be able to deal with any data that's
     * larger than our buffer. */
    while (1) {
	retval = SQLGetData(StatementHandle,
			    ColumnNumber, TargetType,
			    TargetValue, bufsize,
			    &StrLen_or_Ind);

	if (retval == -1) {
	    Tcl_AddErrorInfo(interp, "problem with SQLGetData");
	    return TCL_ERROR;
	} else if (retval == 100) {
	    Tcl_SetObjResult(interp, Tcl_NewStringObj("", -1));
	    return TCL_OK;
	}
	if (StrLen_or_Ind == -1) {
	    Tcl_SetObjResult(interp,
			     Tcl_ObjGetVar2(interp, options, nulloption, 0));
	    return TCL_OK;
	}

	addsize = (StrLen_or_Ind >= bufsize) ? bufsize - 1 : StrLen_or_Ind;
	data = Tcl_NewByteArrayObj(TargetValue, addsize);
	if (tclres == NULL) {
	    tclres = data;
	} else {
	    Tcl_AppendObjToObj(tclres, data);
	}

	if (StrLen_or_Ind < bufsize) {
	    break;
	}
    }

    switch (TargetType) {
	case -8:
	{
	    char *bytes;
	    int len;
	    bytes = Tcl_GetByteArrayFromObj(tclres, &len);
	    Tcl_SetUnicodeObj(tclres, bytes, len);
	    break;
	}
	case 1:
	{
	    Tcl_DString ds;
	    char *stringPtr;
	    int len;

	    stringPtr = (char *) Tcl_GetByteArrayFromObj(tclres, &len);
	    Tcl_ExternalToUtfDString(encoding, stringPtr, len, &ds);

	    Tcl_SetStringObj(tclres,
			     Tcl_DStringValue(&ds), Tcl_DStringLength(&ds));
	    Tcl_DStringFree(&ds);
	    break;
	}
    }

    Tcl_SetObjResult(interp, tclres);

    return TCL_OK;

}

/*
 *-----------------------------------------------------------------------------
 *
 * odbc_fetchrow --
 *
 * 	Loops through all the columns and fetches the associated data.
 *
 * Results:
 *	Puts the resulting list in the interpreter's result.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

static int odbc_fetchrow(SQLHSTMT StatementHandle, Tcl_Interp *interp, int numColumns) {
    int i;
    int retval;
    Tcl_Obj *tclres;
    Tcl_Encoding encoding;

    tclres = Tcl_NewStringObj("", -1);

    if (odbc_tcl_getencoding(interp, &encoding) != TCL_OK) {
	return TCL_ERROR;
    }

    for (i = 1; i <= numColumns; i++) {
	retval = odbc_getdata(interp, StatementHandle, i, encoding);
	if (retval != TCL_OK) {
	    Tcl_DecrRefCount(tclres);
	    return retval;
	}
	retval = Tcl_ListObjAppendElement(interp, tclres, Tcl_GetObjResult(interp));
	if (retval != TCL_OK) {
	    Tcl_DecrRefCount(tclres);
	    return retval;
	}
    }
    Tcl_SetObjResult(interp, tclres);
    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * odbc_fetch_cmd --
 *
 * 	Performs a fetch, and then fetches the rows.  Implements the
 * 	'cfetch' command, which is named thusly to avoid confusion
 * 	with the 'fetch' method in snodbc's 'classes.tcl'.
 *
 * Results:
 * 	Stores the results as a list in the interpreter's result slot.
 *	Returns a standard Tcl result.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

static int
odbc_fetch_cmd(ClientData clientData, Tcl_Interp *interp,
	       int objc, Tcl_Obj *CONST objv[]) {

    SQLHSTMT StatementHandle;
    int numColumns = 0;
    int i = 0;
    int retval = TCL_OK;

    if (objc < 3) {
	Tcl_WrongNumArgs(interp, 1, objv, "handle numColumns");
	return TCL_ERROR;
    }

    if (Tcl_GetLongFromObj(interp, objv[1], &StatementHandle) != TCL_OK) {
	return TCL_ERROR;
    }

    if (Tcl_GetIntFromObj(interp, objv[2], &numColumns) != TCL_OK) {
	return TCL_ERROR;
    }

    retval = SQLFetch(StatementHandle);
    if (retval == 1) {
	Tcl_Eval(interp, "$self Diagnose");
	return TCL_ERROR;
    } else if (retval == -1) {
	Tcl_Eval(interp, "$self Diagnose");
	Tcl_SetErrorCode(interp, "error", NULL);
	return TCL_ERROR;
    } else if (retval == 100) { /* No data  */
	Tcl_SetObjResult(interp, Tcl_NewStringObj("", -1));
	return TCL_OK;
    }

    retval = odbc_fetchrow(StatementHandle, interp, numColumns);
    if (retval != TCL_OK) {
	return retval;
    }

    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * odbc_fetchrow_cmd --
 *
 * 	Implements the 'fetchrow' command, which fetches a row of data
 * 	*after* the SQLFetch function has been called.
 *
 * Results:
 *	Standard Tcl result.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

static int
odbc_fetchrow_cmd(ClientData clientData, Tcl_Interp *interp,
	       int objc, Tcl_Obj *CONST objv[]) {

    SQLHSTMT StatementHandle;
    int numColumns = 0;
    int i = 0;
    int retval = TCL_OK;

    if (objc < 3) {
	Tcl_WrongNumArgs(interp, 1, objv, "handle numColumns");
	return TCL_ERROR;
    }

    if (Tcl_GetLongFromObj(interp, objv[1], &StatementHandle) != TCL_OK) {
	return TCL_ERROR;
    }

    if (Tcl_GetIntFromObj(interp, objv[2], &numColumns) != TCL_OK) {
	return TCL_ERROR;
    }

    retval = odbc_fetchrow(StatementHandle, interp, numColumns);
    if (retval != TCL_OK) {
	return retval;
    }

    return TCL_OK;
}

/*
 *-----------------------------------------------------------------------------
 *
 * odbc_getdata_cmd --
 *
 * 	Implements the 'getdata' command, which fetches the data
 * 	associated with a single row and column in a result.
 *
 * Results:
 *	Standard Tcl result.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

static int
odbc_getdata_cmd(ClientData clientData, Tcl_Interp *interp,
		 int objc, Tcl_Obj *CONST objv[]) {

    SQLHSTMT StatementHandle;
    int ColumnNumber = 0;
    Tcl_Obj *encodingObj;
    Tcl_Encoding encoding;

    if (objc < 3) {
	Tcl_WrongNumArgs(interp, 1, objv, "handle colnum");
	return TCL_ERROR;
    }

    if (Tcl_GetLongFromObj(interp, objv[1], &StatementHandle) != TCL_OK) {
	return TCL_ERROR;
    }

    if (Tcl_GetIntFromObj(interp, objv[2], &ColumnNumber) != TCL_OK) {
	return TCL_ERROR;
    }
    if (odbc_tcl_getencoding(interp, &encoding) != TCL_OK) {
	return TCL_ERROR;
    }

    return odbc_getdata(interp, StatementHandle, ColumnNumber, encoding);
}

/*
 *-----------------------------------------------------------------------------
 *
 * Getdata_Init --
 *
 * 	Initialize the loadable library, create some Tcl objects that
 * 	are only used internally, and create the cfetch, fetchrow and
 * 	fetchdata commands.
 *
 * Results:
 *	Standard Tcl result.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------------
 */

EXTERN int
Getdata_Init(Tcl_Interp *interp) {

    options = Tcl_NewStringObj("options", -1);
    nulloption = Tcl_NewStringObj("-null", -1);
    encodingoption = Tcl_NewStringObj("-encoding", -1);

    /* Create the command. */
    Tcl_CreateObjCommand(interp, "getdata", odbc_getdata_cmd,
			 (ClientData *) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateObjCommand(interp, "fetchrow", odbc_fetchrow_cmd,
			 (ClientData *) NULL, (Tcl_CmdDeleteProc *) NULL);

    Tcl_CreateObjCommand(interp, "cfetch", odbc_fetch_cmd,
			 (ClientData *) NULL, (Tcl_CmdDeleteProc *) NULL);

    return Tcl_PkgProvide(interp, "getdata", "1.0");
}
