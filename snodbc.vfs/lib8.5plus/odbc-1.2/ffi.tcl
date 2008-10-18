package provide odbc::ffi 1.1
package require Ffidl 0.6
package require Ffidlx 0.7
namespace eval odbc::ff {}
if {![info exists odbc::ff::dlls]} {
    if {$tcl_platform(platform) eq "windows"} {
	variable odbc::ff::dlls "odbc32 odbccp32"
    } else {
	variable odbc::ff::dlls "libodbc[info sharedlibextension].1 libodbcinst[info sharedlibextension].1"
	# variable odbc::ff::dlls "libiodbc[info sharedlibextension].2 libiodbcinst[info sharedlibextension].2"
	# variable odbc::ff::dlls "/usr/lib/odbc/libtdsodbc.so /usr/lib/odbc/libtdsS.so"
    }
}


proc ::odbc::dllBind {{ns ::odbc::ff} {dllset {}}} {
    namespace eval $ns {namespace export *}
    if {$dllset eq ""} {
	set dllset $::odbc::ff::dlls 
    }
    ::ffidlx::callfactory ::odbc::api -dlls $dllset 
    catch {
	::ffidl::typedef SQLSMALLINT sint16
	::ffidl::typedef SQLUSMALLINT uint16
	::ffidl::typedef SQLRETURN SQLSMALLINT
	::ffidl::typedef SQLHANDLE pointer
	::ffidl::typedef SQLHWND pointer
	::ffidl::typedef SQLINTEGER sint32
	::ffidl::typedef SQLUINTEGER uint32
    }
    api SQLRETURN ${ns}::SQLGetDiagRec(W)  SQLSMALLINT SQLHANDLE SQLSMALLINT \
	pointer-var  pointer-var pointer-var SQLUSMALLINT pointer-var
    api SQLRETURN ${ns}::SQLAllocHandle SQLSMALLINT SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLFreeHandle SQLSMALLINT SQLHANDLE
    api ? SQLRETURN ${ns}::SQLConnect(W) SQLHANDLE \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT 
    api SQLRETURN ${ns}::SQLDriverConnect(W) SQLHANDLE SQLHWND \
	pointer-byte SQLSMALLINT \
	pointer-var SQLSMALLINT \
	pointer-var SQLUSMALLINT 
    api SQLRETURN ${ns}::SQLPrepare(W) SQLHANDLE pointer-byte SQLINTEGER
    api SQLRETURN ${ns}::SQLExecute SQLHANDLE 
    api SQLRETURN ${ns}::SQLExecDirect(W) SQLHANDLE pointer-byte SQLINTEGER
    api SQLRETURN ${ns}::SQLNumResultCols SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLNumParams SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLDescribeCol SQLHANDLE SQLUSMALLINT \
	pointer-var SQLUSMALLINT pointer-var \
	pointer-var pointer-var \
	pointer-var pointer-var
    api ? SQLRETURN ${ns}::SQLDescribeParam SQLHANDLE SQLUSMALLINT \
	pointer-var pointer-var \
	pointer-var pointer-var
    api SQLRETURN ${ns}::SQLFetch SQLHANDLE 
    api SQLRETURN ${ns}::SQLFetchScroll SQLHANDLE SQLSMALLINT SQLINTEGER
    api SQLRETURN ${ns}::SQLGetData SQLHANDLE SQLUSMALLINT SQLSMALLINT \
	pointer-var SQLUINTEGER pointer-var
    api SQLRETURN ${ns}::SQLCloseCursor SQLHANDLE
    api SQLRETURN ${ns}::SQLRowCount SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLFreeStmt SQLHANDLE SQLUSMALLINT
    api SQLRETURN ${ns}::SQLDisconnect SQLHANDLE 
    api ? SQLRETURN ${ns}::SQLDrivers(W) SQLHANDLE SQLUSMALLINT \
	pointer-var SQLSMALLINT pointer-var \
	pointer-var SQLSMALLINT pointer-var 
    api ? SQLRETURN ${ns}::SQLDataSources(W) SQLHANDLE SQLUSMALLINT \
	pointer-var SQLSMALLINT pointer-var \
	pointer-var SQLSMALLINT pointer-var 
    api SQLRETURN ${ns}::SQLColumns SQLHANDLE \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT 
    api SQLRETURN ${ns}::SQLColumns(W)/raw SQLHANDLE \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT 
    api SQLRETURN ${ns}::SQLProcedureColumns(W)/raw SQLHANDLE \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT 
    api SQLRETURN ${ns}::SQLSpecialColumns(W)/raw SQLHANDLE SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	SQLSMALLINT SQLSMALLINT

    api SQLRETURN ${ns}::SQLPrimaryKeys(W)/raw SQLHANDLE \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \

    api SQLRETURN ${ns}::SQLTables(W)/raw SQLHANDLE \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT 
    api SQLRETURN ${ns}::SQLProcedures(W)/raw SQLHANDLE \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT 

    api SQLRETURN ${ns}::SQLTables SQLHANDLE \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT 

    api SQLRETURN ${ns}::SQLStatistics SQLHANDLE \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT \
	pointer-byte SQLSMALLINT \
	SQLUSMALLINT SQLSMALLINT 

    api SQLRETURN ${ns}::SQLStatistics/raw SQLHANDLE \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	pointer SQLSMALLINT \
	SQLUSMALLINT SQLSMALLINT 

    api SQLRETURN ${ns}::SQLBindParameter SQLHANDLE SQLUSMALLINT \
	SQLSMALLINT SQLSMALLINT SQLSMALLINT SQLUINTEGER \
	SQLSMALLINT pointer SQLINTEGER pointer

    api SQLRETURN ${ns}::SQLSetConnectAttr/int SQLHANDLE SQLINTEGER pointer \
	SQLINTEGER

    api SQLRETURN ${ns}::SQLSetStmtAttr/int SQLHANDLE SQLINTEGER pointer \
	SQLINTEGER
    api SQLRETURN ${ns}::SQLSetEnvAttr/int SQLHANDLE SQLINTEGER pointer \
	SQLSMALLINT

    api SQLRETURN ${ns}::SQLGetConnectAttr/int SQLHANDLE SQLINTEGER \
	pointer-var SQLINTEGER pointer-var
    api SQLRETURN ${ns}::SQLGetStmtAttr/int SQLHANDLE SQLINTEGER \
	pointer-var SQLINTEGER pointer-var
    api SQLRETURN ${ns}::SQLEndTran SQLSMALLINT SQLHANDLE SQLINTEGER
    api SQLRETURN ${ns}::SQLGetTypeInfo SQLHANDLE SQLSMALLINT
    api SQLRETURN ${ns}::SQLColAttribute SQLHANDLE SQLUSMALLINT SQLUSMALLINT \
	pointer-var SQLSMALLINT pointer-var pointer-var
    api SQLRETURN ${ns}::SQLMoreResults SQLHANDLE 
    api ? int ${ns}::SQLConfigDataSource SQLHWND uint16 \
	pointer-byte pointer-byte
    api ? SQLRETURN ${ns}::SQLInstallerError uint16 pointer-var pointer-var \
	uint16 pointer-var
    rename ::odbc::api {}
}

