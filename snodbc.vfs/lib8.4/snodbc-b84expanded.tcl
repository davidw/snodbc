;# expanded file: bundle84.tcl
# 2005, Anton Kovalenko
# Do what you want with this code, but don't blame me.
#
# This package implements a simplistic approach to 
# references. 
# 
# Each reference is a variable within ::reftrace::v, tied to some
# other variable containing some data structure. MkRefLowLevel
# takes (among other arguments) a command to extract an element
# from the data structure and a command to put it back.
#
# There are two types of references: 'temporary' 
# and 'persistent'.
#
# Typical usage for a temporary reference:
# incr [lindex* myListVar 3 12]
#
# Typical usage for a persistent reference:
# .entry configure -textvariable [lindex& myListVar 3 12]
#
# To prevent leaking references, it's preferable to use temporary
# references when it's possible. And it's always possible if the
# following is true:
# - the reference is only required to be valid for an interval 
# during which the main data structure will not be accessed in any way.
#
# It sounds very restrictively, however, there is a lot of cases
# where this restriction is satisfied. Almost every single call 
# to a command that accepts a variable name as an argument 
# will work well with a temporary reference. The only way for 
# the called command to mess things up is to access the main data 
# structure, that's almost never needed when you have to pass
# a reference.
#
# Note that the reference NEVER survives the main data structure,
# so there will be many cases where it's not necessary to worry
# about 'leaking' or 'hanging' refs.
#
# Exact specification of the references behavior:
#
# (for both t&p) - the data structure element gets extracted 
# initially on call to MkRefLowLevel (lindex*, lindex&, etc)
#
# (for both t&p) - when the data structure is read, the new
# element's value is put back on its place
#
# After putting the value back, any TEMPORARY reference 
# is destroyed.
#
# (for both t&p) - when the data structure is unset, the 
# reference is unset too.
#
# For persistent references:
# if the data structure is written, the reference gets new value
# automatically.
#
# For temporary references:
# if the data structure is written, the reference is destroyed.

package provide reftrace 1.0

namespace eval ::reftrace { 
    variable seq 0
    namespace export {[a-z]*}
}
namespace eval ::reftrace::v {}



# Make a persistent reference to a [sub]list element
proc ::reftrace::lindex& {listVarName args} {
    upvar 1 $listVarName lv
    MkRefLowLevel [list ::lindex $args]          [list ::lset $args] 0 lv
}

# Make a persistent reference to a [dict]'s element
proc ::reftrace::dict& {dictVarName args} {
    upvar 1 $dictVarName dv
    MkRefLowLevel [list {::dict get} $args]          [list {::dict set} $args] 0 dv
}

# Make a temporary reference to a [sub]list element
proc ::reftrace::lindex* {listVarName args} {
    upvar 1 $listVarName lv
    MkRefLowLevel [list ::lindex $args]          [list ::lset $args] 1 lv
}

# Make a temporary reference to a [dict]'s element
proc ::reftrace::dict* {dictVarName args} {
    upvar 1 $dictVarName dv
    MkRefLowLevel [list {::dict get} $args]          [list {::dict set} $args] 1 dv
}

# Please put more examples here.


# The low-level procedure creating references
# - inConv is a 'command template' for extracting value 
#   from the data structure;
# - outConv is a 'command template' for putting a value 
#   back to the data structure.
# * Command template is a 2-elements list of lists:
#   {lset myList} {2 4}
#   Extra arguments are inserted between these two parts 
#   of a command. It is a non-standard but (i think) a convenient
#   solution for passing callbacks in Tcl.

proc ::reftrace::MkRefLowLevel {inConv outConv isTemporal structureVarName} {
    # We will assign traces to the data structure
    upvar 1 $structureVarName structureVar
    # Initialize it (may be useful sometimes) [FIXME: explain]
    if {![info exists structureVar]} {
        set structureVar {}
    }
    set linkedName [MkName]
    upvar \#0 $linkedName linkedVar
    foreach {cvpre cvpost} $inConv {break}
    # Used to be {*} :-(
    set linkedVar [eval [lrange $cvpre 0 end] [list $structureVar] [lrange $cvpost 0 end]]
    trace add variable structureVar {write read unset}          [list ::reftrace::Tracker $linkedName $inConv $outConv $isTemporal]
    return $linkedName
}

proc ::reftrace::Tracker      {linkedName inConv outConv isTemporal name1 name2 op} {
    # The linked var is always unset when the data structure var is.
    # For temporary ('unaliased') references, the linked var is
    # also unset when the data structure is written.
    if {($op eq "unset")||($op eq "write" && $isTemporal)} {
        unset $linkedName
    } elseif {$op eq "write"} {
        # For non-temporal references, we update linked var when
        # the data structure is written.
        if {$name2 eq ""} {
            upvar 1 $name1 myvar
        } else {
            upvar 1 ${name1}($name2) myvar
        }
        foreach {cvpre cvpost} $inConv {break}
        set $linkedName [eval [lrange $cvpre 0 end] [list $myvar] [lrange $cvpost 0 end]]
    } elseif {$op eq "read"} {
        # Someone tries to read a data structure...
        if {$name2 eq ""} {
            upvar 1 $name1 myvar
        } else {
            upvar 1 ${name1}($name2) myvar
        }
        # Time to put the value back
        foreach {cvpre cvpost} $outConv {break}
        # If someone didn't unset linked var yet
        if {[info exists $linkedName]} {
	    eval [lrange $cvpre 0 end] [list myvar] [lrange $cvpost 0 end] [list [set $linkedName]]
        } else {
            # The link is destroyed, trace is not necessary
            trace remove variable myvar {write read unset}                  [list ::reftrace::Tracker                      $linkedName $inConv $outConv $isTemporal]
        }
        # Temporal link is removed when the structure is first read.
        if {$isTemporal} {
            unset $linkedName
            trace remove variable myvar {write read unset}                  [list ::reftrace::Tracker                      $linkedName $inConv $outConv $isTemporal]
        }
    }
}

# Reinventing a wheel: generate unique name variable name
proc ::reftrace::MkName {} {
    return ::reftrace::v::[incr ::reftrace::seq]
}
package provide Ffidlx 0.7
package require Ffidl 0.6
package require reftrace

;# Creating callouts for interesting Tcl stubs
    namespace eval ::ffidlx {
	namespace export {[A-Za-z]*}
    }
    namespace eval ::ffidlx::rma {}
    ;# Tcl_Alloc, stub #3 was replaced with Tcl_AttemptAlloc
    ::ffidl::callout ::ffidlx::rma::malloc {uint32} pointer  	[::ffidl::stubsymbol tcl stubs 428]

    # Tcl_Free, stub #4
    ::ffidl::callout ::ffidlx::rma::free {pointer} void  	[::ffidl::stubsymbol tcl stubs 4]

    # Tcl_GetIntFromObj, stub #38. We use it to write a word 
    # at the arbitrary address
    ::ffidl::callout ::ffidlx::rma::getIntFromObj  	{pointer pointer-obj pointer} int  	[::ffidl::stubsymbol tcl stubs 38]
    # Tcl_GetLongFromObj, stub #39. We use it to write a long
    # at the arbitrary address
    
    # it's important to be able to write pointers 
    ::ffidl::callout ::ffidlx::rma::getLongFromObj  	{pointer pointer-obj pointer} int  	[::ffidl::stubsymbol tcl stubs 38]

    # Tcl_GetWideInt, to write 64-bit values.
    ::ffidl::callout ::ffidlx::rma::getWideFromObj  	{pointer pointer-obj pointer} int  	[::ffidl::stubsymbol tcl stubs 487]

    # Tcl_LinkVar 

    # Tcl_NewByteArrayObj, stub #50. Get binary byte array from 
    # memory
    ::ffidl::callout ::ffidlx::rma::newByteArray {pointer uint32}  	pointer-obj [::ffidl::stubsymbol tcl stubs 50]

    # These three are useful for building ::ffidlx::buffer -
    # byte array TclObj of which the interpreter doesn't know.
    ::ffidl::callout ::ffidlx::rma::newByteArray/ptr {pointer-byte uint32}  	pointer [::ffidl::stubsymbol tcl stubs 50]
    ::ffidl::callout ::ffidlx::rma::getByteArray/ptr  {pointer pointer}  	pointer [::ffidl::stubsymbol tcl stubs 33]
    ::ffidl::callout ::ffidlx::rma::freeObj/ptr  {pointer}  	void [::ffidl::stubsymbol tcl stubs 19]

;# Safe versions of malloc and free
    proc ::ffidlx::malloc {size} {
	variable MALLOCS
	variable REGIONS
	set addr [rma::malloc $size]
	if {!$addr} {
	    return -code error "Memory allocation error"
	}
	set MALLOCS($addr) $size
	set REGIONS($addr) $size
	return $addr
    }
    proc ::ffidlx::free {addr} {
	variable MALLOCS
	variable REGIONS
	if {![info exists MALLOCS($addr)]} {
	    return -code error "Freeing non-malloc()ed memory"
	}
	rma::free $addr
	unset MALLOCS($addr)
	unset REGIONS($addr)
    }

;# Working with Tcl_Obj bytearrays
    proc ::ffidlx::buffer {cmd arg} {
	variable BUFFERS
	variable REGIONS
	if {$cmd eq "create"} {
	    set bh [rma::newByteArray/ptr $arg [string length $arg]]
	    set BUFFERS($bh) [rma::getByteArray/ptr $bh 0]
	    set REGIONS($BUFFERS($bh)) [string length $arg]
	    return $bh
	} elseif {$cmd eq "addr"} {
	    return $BUFFERS($arg)
	} elseif {$cmd eq "free"} {
	    unset REGIONS($BUFFERS($arg)) BUFFERS($arg)
	    rma::freeObj/ptr $arg
	} elseif {$cmd eq "data"} {
	    return [rma::newByteArray $BUFFERS($arg) $REGIONS($BUFFERS($arg))]
	}
    }

;# Integer poke/peek, the safe way
    proc ::ffidlx::poke {base offset value} {
	variable REGIONS
	if {$REGIONS($base)<$offset+[::ffidl::info sizeof int]} {
	    return -code error "Writing to random place ???"
	}
	rma::getIntFromObj 0 $value [expr {$base+$offset}]
    }
    proc ::ffidlx::peek {base offset} {
	variable REGIONS
	if {$REGIONS($base)<$offset+[::ffidl::info sizeof int]} {
	    return -code error "Reading random place ???"
	}
	binary scan  [rma::newByteArray [expr {$base+$offset}] [::ffidl::info sizeof int]] [::ffidl::info format int] value
	return $value
    }

;# Integer references
    proc ::ffidlx::LP {type intVarName} {	
	upvar 1 $intVarName iv
	if {![info exists iv]} { set iv 0}
	set bfmt [::ffidl::info format $type]
	::reftrace::MkRefLowLevel [list [list binary format $bfmt] {}]  	    [list [namespace code _decode_LP] $bfmt] 1 iv
    }
    proc ::ffidlx::_decode_LP {targetVarName bfmt value} {
	upvar 2 $targetVarName targetVar
	binary scan $value $bfmt targetVar
    }

;# Data reformatting
    proc ::ffidlx::LPSTR {str {encoding {}}} {
	if {$encoding eq ""} { set encoding [encoding system]}
	binary format a*x [encoding convertto $encoding $str]
    }
    proc ::ffidlx::LPWSTR {str } {
	binary format a*x2 [encoding convertto unicode $str]
    }

;# Call factory

proc ::ffidlx::callfactory {name args} {
    set callerNs [uplevel 1 [list namespace current]]
    if {![string match ::* $name]} {
	set name ${callerNs}::$name
    }
    interp alias {} $name {} ::ffidlx::_actualfactory $args
}

proc ::ffidlx::_actualfactory {options rettype name args} {
    if {$rettype eq "?"} {
	lappend options -optional 1
	set rettype $name
	set name [lindex $args 0]
	set args [lrange $args 1 end]
    }
    array set opts {-dlls {{}}}
    array set opts {-optional 0}
    set opts(-protocol) [
	expr {$::tcl_platform(platform) eq "windows"? "stdcall": "cdecl"}]
    set callerNs [uplevel 1 [list namespace current]]
    if {![string match ::* $name]} {
	set name ${callerNs}::$name
    }
    array set opts $options
    if {[string match */* $name]} {
	foreach {realname suffix} [split $name /] {break}
	set suffix /$suffix
    } else {
	set suffix {}
	set realname $name
    }
    if {[string match *(W) $realname]} {
	# looking for optional unicode and mandatory ansi is typical
	eval [linsert $args 0  		_actualfactory $options  		$rettype [string range $realname 0 end-3]$suffix]
	eval [linsert $args 0  		_actualfactory [linsert $options end -optional 1]  		$rettype [string range $realname 0 end-3]W$suffix]
	return
    }
    # We allow multiple callouts for a single function,
    # e.g. SearchPathW/nopath to pass NULL as a path.
    set funcName [lindex [split [namespace tail $name] /] 0]
    # find a function's address
    foreach dll $opts(-dlls) {
	if {[catch {::ffidl::symbol $dll $funcName} addr]} {
	    set addr 0
	} else {
	    break
	}
    }
    if {!$addr} {
	if {!$opts(-optional)} {
	    return -code error "Symbol lookup failed for function: $funcName"
	} else {
	    return "Symbol lookup failed for function: $funcName"
	}
    }
    # now we are ready to construct an actual call.
    ::ffidl::callout $name $args $rettype $addr $opts(-protocol)
}
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
    api SQLRETURN ${ns}::SQLGetDiagRec(W)  SQLSMALLINT SQLHANDLE SQLSMALLINT  	pointer-var  pointer-var pointer-var SQLUSMALLINT pointer-var
    api SQLRETURN ${ns}::SQLAllocHandle SQLSMALLINT SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLFreeHandle SQLSMALLINT SQLHANDLE
    api ? SQLRETURN ${ns}::SQLConnect(W) SQLHANDLE  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT
    api SQLRETURN ${ns}::SQLDriverConnect(W) SQLHANDLE SQLHWND  	pointer-byte SQLSMALLINT  	pointer-var SQLSMALLINT  	pointer-var SQLUSMALLINT
    api SQLRETURN ${ns}::SQLPrepare(W) SQLHANDLE pointer-byte SQLINTEGER
    api SQLRETURN ${ns}::SQLExecute SQLHANDLE
    api SQLRETURN ${ns}::SQLExecDirect(W) SQLHANDLE pointer-byte SQLINTEGER
    api SQLRETURN ${ns}::SQLNumResultCols SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLNumParams SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLDescribeCol SQLHANDLE SQLUSMALLINT  	pointer-var SQLUSMALLINT pointer-var  	pointer-var pointer-var  	pointer-var pointer-var
    api ? SQLRETURN ${ns}::SQLDescribeParam SQLHANDLE SQLUSMALLINT  	pointer-var pointer-var  	pointer-var pointer-var
    api SQLRETURN ${ns}::SQLFetch SQLHANDLE
    api SQLRETURN ${ns}::SQLFetchScroll SQLHANDLE SQLSMALLINT SQLINTEGER
    api SQLRETURN ${ns}::SQLGetData SQLHANDLE SQLUSMALLINT SQLSMALLINT  	pointer-var SQLUINTEGER pointer-var
    api SQLRETURN ${ns}::SQLCloseCursor SQLHANDLE
    api SQLRETURN ${ns}::SQLRowCount SQLHANDLE pointer-var
    api SQLRETURN ${ns}::SQLFreeStmt SQLHANDLE SQLUSMALLINT
    api SQLRETURN ${ns}::SQLDisconnect SQLHANDLE
    api ? SQLRETURN ${ns}::SQLDrivers(W) SQLHANDLE SQLUSMALLINT  	pointer-var SQLSMALLINT pointer-var  	pointer-var SQLSMALLINT pointer-var
    api ? SQLRETURN ${ns}::SQLDataSources(W) SQLHANDLE SQLUSMALLINT  	pointer-var SQLSMALLINT pointer-var  	pointer-var SQLSMALLINT pointer-var
    api SQLRETURN ${ns}::SQLColumns SQLHANDLE  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT
    api SQLRETURN ${ns}::SQLColumns(W)/raw SQLHANDLE  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT
    api SQLRETURN ${ns}::SQLProcedureColumns(W)/raw SQLHANDLE  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT
    api SQLRETURN ${ns}::SQLSpecialColumns(W)/raw SQLHANDLE SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	SQLSMALLINT SQLSMALLINT

    api SQLRETURN ${ns}::SQLPrimaryKeys(W)/raw SQLHANDLE  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT
    api SQLRETURN ${ns}::SQLTables(W)/raw SQLHANDLE  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT
    api SQLRETURN ${ns}::SQLProcedures(W)/raw SQLHANDLE  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT

    api SQLRETURN ${ns}::SQLTables SQLHANDLE  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT

    api SQLRETURN ${ns}::SQLStatistics SQLHANDLE  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT  	pointer-byte SQLSMALLINT  	SQLUSMALLINT SQLSMALLINT

    api SQLRETURN ${ns}::SQLStatistics/raw SQLHANDLE  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	pointer SQLSMALLINT  	SQLUSMALLINT SQLSMALLINT

    api SQLRETURN ${ns}::SQLBindParameter SQLHANDLE SQLUSMALLINT  	SQLSMALLINT SQLSMALLINT SQLSMALLINT SQLUINTEGER  	SQLSMALLINT pointer SQLINTEGER pointer

    api SQLRETURN ${ns}::SQLSetConnectAttr/int SQLHANDLE SQLINTEGER pointer  	SQLINTEGER

    api SQLRETURN ${ns}::SQLSetStmtAttr/int SQLHANDLE SQLINTEGER pointer  	SQLINTEGER
    api SQLRETURN ${ns}::SQLSetEnvAttr/int SQLHANDLE SQLINTEGER pointer  	SQLSMALLINT

    api SQLRETURN ${ns}::SQLGetConnectAttr/int SQLHANDLE SQLINTEGER  	pointer-var SQLINTEGER pointer-var
    api SQLRETURN ${ns}::SQLGetStmtAttr/int SQLHANDLE SQLINTEGER  	pointer-var SQLINTEGER pointer-var
    api SQLRETURN ${ns}::SQLEndTran SQLSMALLINT SQLHANDLE SQLINTEGER
    api SQLRETURN ${ns}::SQLGetTypeInfo SQLHANDLE SQLSMALLINT
    api SQLRETURN ${ns}::SQLColAttribute SQLHANDLE SQLUSMALLINT SQLUSMALLINT  	pointer-var SQLSMALLINT pointer-var pointer-var
    api SQLRETURN ${ns}::SQLMoreResults SQLHANDLE
    api ? int ${ns}::SQLConfigDataSource SQLHWND uint16  	pointer-byte pointer-byte
    api ? SQLRETURN ${ns}::SQLInstallerError uint16 pointer-var pointer-var  	uint16 pointer-var
    rename ::odbc::api {}
}
# vim: ft=tcl
package provide odbc::obj 1.2

package require odbc::ffi
package require Ffidlx
package require snit

namespace eval ::odbc {}

# \uFFFF is a convenient value to represent NULLs,
# as it's (almost) never legitimate in other contexts.
#
# Snodbc doesn't use it by default - NULLs are represented as empty strings
# to preserve tclodbc compatibility

proc ::odbc::NULL {} { return "\uFFFF"}
proc ::odbc::DEFAULT {} { return "\uFFFFFFFF"}

variable ::odbc::ATTRS {
    readonly 101
    concurrency {7 readonly 1 lock 2 rowver 3 values 4}
    cursortype {6 forwardonly 0 keyset 1 dynamic 2 static 3}
    cursors {110 auto 0 odbc 1 driver 2}
    autocommit 102 logintimeout 103 login_timeout 103 
    maxrows 1 timeout 0 maxlength 3
    multisets 36 rowsetsize 3 noscan 2 scrollable -1
    transactionisolationlevel {108 readuncommitted 1 readcommitted 2 
	repeatableread 4 serializable 8}
    transaction_isolation_level {108 read_uncommitted 1 read_committed 2 
	repeatable_read 4 serializable 8}
}

namespace eval ::odbc {}; proc ::odbc::environment {args} {};  ::::snit::type ::odbc::environment {
    variable henv 0
    variable apins
    typevariable ODBCconfigureOps -array {
	add_dsn 1 config_dsn 2 remove_dsn 3
	add_sys_dsn 4 config_sys_dsn 5 remove_sys_dsn 6
    }
    option -libraries {}
    typevariable defaultlibraries {}

    constructor {args} {
	$self configurelist $args
	set apins [$self apins]
	if {$options(-libraries) eq ""} {
	    if {$defaultlibraries eq ""} {
		switch -exact -- $::tcl_platform(platform) {windows {
			set defaultlibraries {{odbc32 odbccp32}}
		    } unix {
			set defaultlibraries [list "libodbc[info sharedlibextension].1 libodbcinst[info sharedlibextension].1"]
		    } default {
			set defaultlibraries {{}}
		    }}
	    }
	    foreach liblist $defaultlibraries {
		::odbc::dllBind $apins $liblist
		if {[llength [info commands ${apins}::SQLAllocHandle]]} {
		    break
		}
		namespace delete ${apins}
	    }
	} else {
		::odbc::dllBind $apins $options(-libraries)
	}
	if {![llength [info commands ${apins}::SQLAllocHandle]]} {
	    return -code error "Can't load ODBC libraries. Specifying -libraries explicitly may help"
	}
	${apins}::SQLAllocHandle 1 0 [LP SQLHANDLE henv]
	${apins}::SQLSetEnvAttr/int $henv 200 3 0
    }
    destructor {
	catch { ${apins}::SQLFreeHandle 1 $henv}
    }
    method apins {} {return ${selfns}::ffi}
    method HANDLE {} {
	return $henv
    }
    method diagnose {{htype 1} {handle 0} {xdiagvarname {}}} {
	if {$xdiagvarname ne {}} {
	    upvar 1 $xdiagvarname xdiag
	}
	if {$htype == 1} {set handle $henv}
	if {!$handle} {return}
	set recno 1
	set ds [list]
	while {1} {
	    set sqlState [binary format @6]
	    set msgBuf [binary format @1028]
	    set r [${apins}::SQLGetDiagRec $htype $handle $recno  		    sqlState [::ffidlx::LP SQLINTEGER nativeError]  		    msgBuf 1024 [::ffidlx::LP SQLSMALLINT msgBufRealLen]]
	    if {$r==100 || $r==-1} {break}
	    set sqlMessage [encoding convertfrom  		[string range $msgBuf 0 [expr {$msgBufRealLen-1}]]]
	    set sqlState [encoding convertfrom [string range $sqlState 0 4]]
	    lappend ds "$sqlState/$nativeError: $sqlMessage"
	    lappend xdiag [list $sqlState $nativeError $sqlMessage]
	    incr recno
	}
	return [join $ds "\n"]
    }
    method drivers {} {
	set bufDescr 256
	set bufAttr 2048
	set more 1
	while {$more} {
	    set drvlist [list]
	    # first=2, next=1
	    for {set dir 2} {$more} {set dir 1} {
		set dvDescr [binary format @$bufDescr]
		set dvAttr [binary format @$bufAttr]
		set r [${apins}::SQLDriversW $henv $dir  			dvDescr $bufDescr [LP SQLSMALLINT nbufDescr]  			dvAttr $bufAttr [LP SQLSMALLINT nbufAttr]]
		if {$r==100} {
		    set more 0
		    break
		} elseif {$bufDescr <= $nbufDescr} {
		    set bufDescr [expr {$nbufDescr*2+2}]
		    break
		} elseif {$bufAttr <= $nbufAttr} {
		    set bufAttr [expr {$nbufAttr*2+2}]
		    break
		} elseif {$r == 1} {
		    set bufAttr [expr {$bufAttr*3/2}]
		    set bufDescr [expr {$bufDescr*3/2}]
		    break
		} elseif {$r == -1} {
		    return -code error [::odbc::Diagnose $henv 1]
		}
		set dvDescr [encoding convertfrom unicode  			[string trimright $dvDescr "\x00"]\x00]
		set dvAttr [encoding convertfrom unicode  			[string trimright $dvAttr "\x00"]\x00]
		lappend drvlist [list $dvDescr [split $dvAttr "\x00"]]
	    }
	}
	return $drvlist
    }
    method datasources {{which all}} {
	set bufName 256
	set bufDescr 2048
	set more 1
	while {$more} {
	    set dsnlist [list]
	    # first=2, next=1
	    for {set dir [expr {$which eq "all"? 2 : ($which eq "user"? 31 : 32)}]}  		{$more} {set dir 1} {
		set dvName [binary format @$bufName]
		set dvDescr [binary format @$bufDescr]
		set r [${apins}::SQLDataSources $henv $dir  			dvName $bufName [LP SQLSMALLINT nbufName]  			dvDescr $bufDescr [LP SQLSMALLINT nbufDescr]]
		if {$r==100} {
		    set more 0
		    break
		} elseif {$bufName <= $nbufName} {
		    set bufName [expr {$nbufName*2+2}]
		    break
		} elseif {$bufDescr <= $nbufDescr} {
		    set bufDescr [expr {$nbufDescr*2+2}]
		    break
		} elseif {$r == 1} {
		    set bufDescr [expr {$bufDescr*3/2}]
		    set bufName [expr {$bufName*3/2}]
		    break
		} elseif {$r == -1} {
		    return -code error [::odbc::Diagnose $henv 1]
		}
		set dvName [string trimright [encoding convertfrom [ 			string range $dvName 0 [expr {$nbufName*2}]]] "\x00"]
		set dvDescr [string trimright [encoding convertfrom [ 			string range $dvDescr 0 [expr {$nbufDescr*2}]]] "\x00"]
		lappend dsnlist [list $dvName $dvDescr]
	    }
	}
	return $dsnlist
    }
    method configdatasource {op driver {attrs {}}} {
	set myop $ODBCconfigureOps($op)
	set driver [binary format a*x [encoding convertto $driver]]
	set zattrs ""
	foreach attr $attrs {
	    append zattrs [binary format a*x [encoding convertto $attr]]
	}
	set zattrs [binary format a*x $zattrs]
	set r [${apins}::SQLConfigDataSource 0 $myop $driver $zattrs]
	if {!$r} {
	    set msg {}
	    set delim {}
	    foreach i {1 2 3 4 5 6 7 8} {
		set msgbuf [binary format x1028]
		set rq [
		    ${apins}::SQLInstallerError $i [LP SQLINTEGER ec]  			msgbuf 1024 [LP uint16 msgl]]
		if {$rq == 100} {break}
		binary scan $msgbuf A* themsg
		append msg $delim "$ec [encoding convertfrom $themsg]"
		set delim "\n"
	    }
	    return -code error $msg
	}
    }

    typeconstructor {
	if {[catch {namespace path}]} {
	    namespace import ::odbc::* ::ffidlx::*
	} else {
	    namespace path [linsert [namespace path] end ::odbc ::ffidlx]
	}
    }

    typemethod {extension preferpackage} {pkg} {
	set pkg [string tolower $pkg]
	if {$::tcl_platform(platform) ne "windows"} { 
	    switch -exact -- $pkg {iodbc {
		    set defaultlibraries [
		    list  			"libiodbc[info sharedlibextension].2 libiodbcinst[info sharedlibextension].2"  			"libodbc[info sharedlibextension].1 libodbcinst[info sharedlibextension].1"]

		} unixodbc {
		    set defaultlibraries [
		    list  			"libodbc[info sharedlibextension].1 libodbcinst[info sharedlibextension].1"  			"libiodbc[info sharedlibextension].2 libiodbcinst[info sharedlibextension].2"]
		}}
	}
    }
}

namespace eval ::odbc {}; proc ::odbc::database {args} {};  ::::snit::type ::odbc::database {
    typevariable henv
    typevariable defaultEnvironment {::odbc::DEFAULTENVIRONMENT}
    variable hdbc 0
    variable autoStmtCache;#{}
    variable autoStmtSeq {}
    variable connected 0
    variable apins
    variable diagnostics {}
    option -encoding {}
    option -null {}
    option -default {}
    option -connectionstring {}
    option -preparedsqlcachesize 0
    option -environment {}
    option -uid {}
    option -pwd {}
    delegate method disconnect using "%s destroy"
    delegate typemethod connect using "%t create"
    delegate typemethod {extension preferpackage} using "::odbc::environment extension preferpackage"
    delegate typemethod {extension datatype} using "::odbc::statement extension datatype"
    method HDBC {} {return $hdbc}
    method Diagnose {{htype 2} {handle 0} {xdiagvarname {}}} {
	if {$xdiagvarname ne ""} {
	    upvar 1 $xdiagvarname xdiag
	}
	if {$htype == 2} {set handle $hdbc}
	$options(-environment) diagnose $htype $handle xdiag
    }


    constructor {args} {
	$self configure -encoding [encoding system]
	# we receive either connstring -options,
	# or dsn uid pwd
	set optargs {}
	if {[set optpos [lsearch -glob $args -*]]!=-1} {
	    set optargs [lrange $args $optpos end]
	    set args [lrange $args 0 [expr {$optpos-1}]]
	}
	if {![llength [$self info methods [list Connect [lindex $args 0]]]]} {
	    if {[llength $args]==1} {
		set args [linsert $args 0 driver]
	    } else {
		foreach {dsn uid pwd} {{}} break; foreach {dsn uid pwd} $args break; lrange $args 3 end
		set args [list datasource $dsn -uid $uid -pwd $pwd]
	    }
	}
	set result [eval [list $self] [list Connect] [lrange $args 0 end] [lrange $optargs 0 end]]
	if {$result==-1 || $result ==-2} {
	    set msg [$self Diagnose]
	    ${apins}::SQLFreeHandle 2 $hdbc
	    return -code error $msg
	} else {
	    set connected 1
	}
    }
    method Preconfigure {args} {
	if {[set e [from args -environment {}]] eq ""} {
	    set e $defaultEnvironment
	}
	if {[namespace which -command $e] eq ""} {
	    ::odbc::environment $e -libraries [from args -libraries {}]
	}
	set options(-environment) $e
	set apins [$e apins]
	set newargs [list]
	foreach {option value} $args {
	    set oa [string range $option 1 end]
	    if {[dict exists $::odbc::ATTRS $oa]} {
		$self set $oa $value
	    } else {
		lappend newargs $option $value
	    }
	}
	${apins}::SQLAllocHandle 2 [$e HANDLE] [LP SQLHANDLE hdbc]
	$self configurelist $newargs
    }

    method {Connect datasource} {dsn args} {
	eval [list $self] [list Preconfigure] [lrange $args 0 end]
	${apins}::SQLConnect $hdbc [LPSTR $dsn] -3  	    [LPSTR [from args -uid]] -3  	    [LPSTR [from args -pwd]] -3
    }
    typevariable apart_thread_id {}
    variable promptvals -array {none 0 complete 1 all 2 required 3}
    method {Connect driver} {connstring args} {
	eval [list $self] [list Preconfigure] [lrange $args 0 end]
	if {$connstring eq "?"} {
	    set connstring ""
	    set window [from args -window x]
	    if {$window eq "x"} {
		catch {set window [winfo toplevel [focus]]}
		if {$window eq "x"} {
		    set window .
		}
	    }
	    lappend args  		-window $window  		-prompt [from args -prompt all]
	} elseif {![string match *=* $connstring]} {
	    set connstring DSN=$connstring
	}
	set apart [from args -apart 0]
	if {$apart eq "auto"} {
	    set apart [expr {[info exists ::tcl_platform(threaded)]&& 
		             $::tcl_platform(threaded)&&
			     ![catch {package require Thread}]}]
	}
	if {$apart} {
	    variable cstr
	    set window [from args -window {}]
	    if {$window ne ""} { lappend args -hwnd [wm frame $window]}
	    # let's autoload dict for tcl 8.4
	    if {$apart_thread_id eq ""} {
		package require Thread
		package require odbc::apart
		set tid [thread::create]
		catch { thread::send $tid [list load {} Ffidl]}
		catch { thread::send $tid [list load {} dict]}
		catch { thread::send $tid [::odbc::apart_hook]}
		thread::send $tid {package require odbc::obj}
		set apart_thread_id $tid
	    }  else {
		set tid $apart_thread_id
	    }
	    if 1 {
		thread::send -async $tid  		    "catch {[linsert $args 0 ::odbc::database fconn driver $connstring]}
		     set cstr {}
		     catch {set cstr \[fconn get connectionstring\]}
		     catch {fconn disconnect}
		     set cstr" [myvar cstr]
		vwait [myvar cstr]
	    } else {
		set cstr [
		thread::send $tid  		    "[linsert $args 0 ::odbc::database fconn driver $connstring];
		     set cstr \[fconn get connectionstring\]
		     fconn disconnect
		     set cstr"
		]
	    }
	    set args {}
	    set connstring $cstr
	    if {$connstring eq ""} { return -code error "CANCELLED"}
	}
	set bcs [encoding convertto $connstring]
	set bcsZ [binary format a*x $bcs]
	set obf [binary format @2060]
	set hwnd [from args -hwnd 0]
	set tkwin [from args -window {}]
	if {$tkwin ne "" && !$hwnd} {
	    set hwnd [wm frame $tkwin]
	}
	set prompt [from args -prompt none]
	set iPrompt $promptvals($prompt)
	set r [${apins}::SQLDriverConnect $hdbc $hwnd $bcsZ [string length $bcs] obf 2048  	    [LP SQLINTEGER _] $iPrompt]
	$self configure -connectionstring  	    [encoding convertfrom [string trimright $obf "\x00"]]
	;# if {$options(-connectionstring) eq ""} {
	;#   return -code error "CANCELLED"
	;# }
	return $r
    }
    method AutoStmt {sql {argTypes {}}} {
	if {$options(-preparedsqlcachesize)} {
	    set pcsFull [expr {$options(-preparedsqlcachesize)}]
	    set pcsPrev [expr {$options(-preparedsqlcachesize)-1}]
	    set key $sql\x00[join $argTypes \x00]
	    if {[info exists autoStmtCache($key)]} {
		set stmt $autoStmtCache($key)
	    } else {
		set stmt [::odbc::statement within $self %AUTO% $sql $argTypes]
		set autoStmtCache($key) $stmt
		lappend autoStmtSeq $key
	    }
	    foreach too_old [lrange $autoStmtSeq 0 end-$pcsFull] {
		$autoStmtCache($too_old) destroy
		unset autoStmtCache($too_old)
	    }
	    set autoStmtSeq [lrange $autoStmtSeq end-$pcsPrev end]
	    return $stmt
	} else {
	    variable directstatement
	    if {![info exists directstatement]} {
		set directstatement [::odbc::statement within $self %AUTO% $sql $argTypes -direct 1]
	    } else {
		$directstatement configure -sql $sql -argtypes $argTypes  		    -encoding $options(-encoding) -null $options(-null)  		    -default $options(-default)
	    }
	    return $directstatement
	}
    }
    method run {sql {argTypes {}} {argList {}}} {
	set rv [uplevel 1 [list [set st [$self AutoStmt $sql $argTypes]] run $argList]]
	set diagnostics [$st lastmessages]
	return $rv
    }
    method lastmessages {} {lindex [list $diagnostics [set diagnostics {}]] 0}
    method typeinfo {typeName} {
	$self run typeinfo {} [list $typeName]
    }
    method tables {args} { $self run tables {} $args}
    method columns {args} { $self run columns {} $args}
    method rowid {args} { $self run rowid {} $args}
    method primarykeys {args} { $self run primarykeys {} $args}
    method procedures {args} { $self run procedures {} $args}
    method procedurecolumns {args} { $self run procedurecolumns {} $args}
    method indexes {args} { $self run indexes {} $args}

    method eval {cmd sql {argTypes {}} {argList {}}} {
	if {![llength $argList]} {
	    set argList $argTypes
	    set argTypes {}
	}
	uplevel 1 [list [$self AutoStmt $sql $argTypes] eval $cmd $argList]
    }
    method read {arrays sql {argTypes {}} {argList {}}} {
	if {![llength $argList]} {
	    set argList $argTypes
	    set argTypes {}
	}
	uplevel 1 [list [$self AutoStmt $sql $argTypes] read $arrays $argList]
    }
    method set {opt value} {
	if {[info exists options(-$opt)]} {
	    $self configure -$opt $value
	    foreach stmt [::odbc::statement info instances] {
		if {[$stmt HDBC] == $hdbc} { $stmt configure -$opt $value}
	    }
	    return $value
	} else {
	    if {[string is boolean $value]} { set value [expr {$value? 1 : 0}]}
	    foreach {optCode} {{}} break; foreach {optCode} [dict get $::odbc::ATTRS $opt] break; lrange [dict get $::odbc::ATTRS $opt] 1 end
	    set avdict [lrange [dict get $::odbc::ATTRS $opt] 1 end]
	    if {[llength $avdict]} {
		set value [dict get $avdict $value]
	    }
	    set rc [${apins}::SQLSetConnectAttr/int $hdbc $optCode $value 0]
	    if {$rc == -1} {
		return -code error [$self Diagnose]
	    } elseif {$rc == 1} {
		return [$self get $opt]
	    } else {
		return $value
	    }
	}
    }
    method get {opt} {
	if {[info exists options(-$opt)]} {
	    return [$self cget -$opt]
	}
	set avdict [foreach {optCode} {{}} break; foreach {optCode} [dict get $::odbc::ATTRS $opt] break; lrange [dict get $::odbc::ATTRS $opt] 1 end]
	set rc [${apins}::SQLGetConnectAttr/int $hdbc $optCode [LP SQLINTEGER value]  	    0 [LP SQLINTEGER _]]
	if {[llength $avdict]} {
	    dict for {k v} $avdict { if {$value==$k} {set value $v; break} }
	}
	return $value
    }
    method commit {} { $self EndTran 0}
    method rollback {} { $self EndTran 1}
    method EndTran {how} {
	set rc [${apins}::SQLEndTran 2 $hdbc $how]
	if {$rc==-1} { return -code error [$self Diagnose]}
	return OK
    }

    delegate method statement using "::odbc::statement within %s"
    delegate method * using "%s _Run %M"

    method _Run {sqlL {argTypes {}} {argList {}}} {
	$self run [lindex $sqlL 0] $argTypes $argList
    }

    typemethod datasources {{which all}} {
	if {[namespace which -command $defaultEnvironment] eq ""} {
	    ::odbc::environment $defaultEnvironment
	}
	$defaultEnvironment datasources $which
    }
    typemethod drivers {} {
	if {[namespace which -command $defaultEnvironment] eq ""} {
	    ::odbc::environment $defaultEnvironment
	}
	$defaultEnvironment drivers
    }
    typemethod configure {op driver {attrs {}}} {
	if {[namespace which -command $defaultEnvironment] eq ""} {
	    ::odbc::environment $defaultEnvironment
	}
	$defaultEnvironment configdatasource $op $driver $attrs
    }
    typeconstructor {
	if {[catch {namespace path}]} {
	    namespace import ::odbc::* ::ffidlx::*
	} else {
	    namespace path [linsert [namespace path] end ::odbc ::ffidlx]
	}
    }
    destructor {
	foreach stmt [::odbc::statement info instances] {
	    if {[$stmt HDBC] == $hdbc} { $stmt destroy}
	}
	if {$connected} {
	    ${apins}::SQLDisconnect $hdbc
	    ${apins}::SQLFreeHandle 2 $hdbc
	}
    }
}

namespace eval ::odbc {}; proc ::odbc::statement {args} {};  ::::snit::type ::odbc::statement {
    variable hdbc
    variable hstmt
    variable apins
    ;# Buffer pointer for parameter indication
    variable pib 0
    variable numParams 0
    variable numColumns 0
    variable gdBuffer
    variable dbConn
    variable isSpecial 0
    variable specialFunction ""
    variable diagnostics {}
    delegate method drop using "%s destroy"

    option -sql -configuremethod PrepareSQL
    option -argtypes
    option -null {}
    option -default {}
    option -encoding {}
    option -direct 0


    typevariable sqltypes -array {
	CHAR {1} NUMERIC {2} DECIMAL {3} 
	INTEGER {4} SMALLINT {5}
	FLOAT {6} REAL {7} DOUBLE {8}
	DATE {91} TIME {92} TIMESTAMP {93}
	VARCHAR {12} LONGVARCHAR {-1} BINARY {-2}
	VARBINARY {-3} LONGVARBINARY {-4} 
	BIGINT {-5} TINYINT {-6} BIT {-7} 
	WCHAR {-8} WVARCHAR {-9} WLONGVARCHAR {-10} 
	DB2V9_XML {-370}
    }
    typevariable ctypeoverrides -array {}
    proc ctype_from_sqltype {sqltypeno} {
	if {[info exists ctypeoverrides($sqltypeno)]} {
	    return $ctypeoverrides($sqltypeno)
	}
	if {$sqltypeno>=-10 && $sqltypeno <= -8} {
	    ;# SQL_C_WCHAR for W[LONG][VAR]CHAR
	    return -8
	} elseif {$sqltypeno>=-4 && $sqltypeno <=-2 || $sqltypeno == -370} {
	    ;# SQL_C_BINARY for [LONG][VAR]BINARY and DB2 v9 XML
	    return -2
	} else {
	    ;# SQL_C_CHAR for any other
	    return 1
	}
    }
    typemethod {extension datatype} {typename code {ctype wchar}} {
	switch -exact -- [string toupper $ctype] {WCHAR { set ctype -8} CHAR { set ctype 1} BINARY { set ctype -2} default {
		return -code error "C type must be one of WCHAR, CHAR and BINARY"
	    }}
	set sqltypes([string toupper $typename]) $code
	set ctypeoverrides($code) $ctype
    }
    typevariable ctype_terminations -array {1 1 -8 2 -2 0}
    proc ctype_termination {ctypeno} {
	set ctype_terminations($ctypeno)
    }

    typevariable columnattrs {
	name 22 
	label 18 
	displaysize 6
	scale 1006
	type 1002 
	precision 1005 
	typename 14 
	nullable 7 updatable 10 
	tablename 23
	qualifiername 17 
	owner 16
	count 1001
	columnname 1011
    }

    typevariable columnattrs_str {
	label name tablename typename qualifiername owner columnname
    }

    constructor {args} {
	set hdbc [[set dbConn [from args -dbc]] HDBC]
	set apins [[$dbConn cget -environment] apins]
	${apins}::SQLAllocHandle 3 $hdbc [LP SQLHANDLE hstmt]
	$self configurelist $args
    }
    typemethod within {conn stmtname sql args} {
	foreach {argTypes} {{}} break; foreach {argTypes} $args break; lrange $args 1 end
	set rest [lrange $args 1 end]
	set stmt [uplevel 1  	    [mytypemethod create $stmtname -dbc $conn -direct [from rest -direct 0] -sql $sql  		-argtypes $argTypes -encoding [$conn cget -encoding]  		-null [$conn cget -null]] -default [$conn cget -default]]
	$stmt configurelist $rest
	return $stmt
    }

    method Diagnose {} { set diagnostics {}; $dbConn Diagnose 3 $hstmt diagnostics}
    method DiagnoseAppend {} { $dbConn Diagnose 3 $hstmt diagnostics}
    method lastmessages {} {lindex [list $diagnostics [set diagnostics {}]] 0}

    method MakePib {nParams} {
	if {$pib} {::ffidlx::free $pib; set pib 0}
	if {$nParams!=0} {
	    ;# there are parameters. 
	    ;# We allocate PIB for them, but not to bind them so early.
	    set pibSize [expr {[::ffidl::info sizeof SQLINTEGER]*$nParams}]
	    set pib [::ffidlx::malloc $pibSize]
	}
    }

    method PrepareSQL {args} {
	set sql [from args -sql]
	variable desc_param_cache
	unset -nocomplain desc_param_cache
	# special function instead of sql clause
	if {[llength [$self info methods [list SpecialExecute $sql]]]} {
	    set isSpecial 1
	    set specialFunction $sql
	    return
	} else {
	    set isSpecial 0
	}
	if {$pib} {::ffidlx::free $pib; set pib 0}
	;# how much parameters are there?
	;# we won't bind param until needed!
	if {!$options(-direct)} {
	    set r [${apins}::SQLPrepare $hstmt [LPSTR $sql] -3]
	    if {$r == -1} { return -code error [$self Diagnose]}
	    ${apins}::SQLNumParams $hstmt [LP SQLSMALLINT numParams]
	} else {
	    set numParams 0
	}
	set options(-sql) $sql
    }
    method AskForParamType {nth} {
	if {!$options(-direct)} {
	    variable desc_param_cache
	    if {![info exists desc_param_cache($nth)]} {
		foreach {ptype pscale pprec pnull} {{}} break; foreach {ptype pscale pprec pnull} {0 0 0 0} break; lrange {0 0 0 0} 4 end
		catch {
		    ${apins}::SQLDescribeParam $hstmt $nth  			[LP SQLSMALLINT ptype]  [LP SQLINTEGER pscale]  			[LP SQLSMALLINT pprec]  [LP SQLSMALLINT pnull]
		}
		set desc_param_cache($nth) [list $ptype $pscale $pprec]
	    }
	    return $desc_param_cache($nth)
	} else {
	    return {0 0 0}
	}
    }
    method DescribeColumn {nth} {
	variable desc_column_cache
	if {![info exists desc_column_cache($nth)]} {
	    set cname [binary format @256]
	    ${apins}::SQLDescribeCol $hstmt $nth  		cname 256  		    [LP SQLSMALLINT cnameLen]  		    [LP SQLSMALLINT ctype]  		    [LP SQLINTEGER cscale]  		    [LP SQLSMALLINT cprec]  		    [LP SQLSMALLINT nullable]
	    set cname [encoding convertfrom [string trimright $cname "\x00"]]
	    set desc_column_cache($nth)  		[list $cname $ctype $cscale $cprec $nullable]
	}
	return $desc_column_cache($nth)
    }

    method GetColumnAttr {nth attr} {
	set attrcode [dict get $columnattrs $attr]
	set buf [binary format @256]
	set r [${apins}::SQLColAttribute $hstmt $nth $attrcode  		buf 256 [LP SQLSMALLINT realLength] [LP SQLINTEGER iResult]]
	if {[lsearch $columnattrs_str $attr]!=-1}  {
	    set av [encoding convertfrom $options(-encoding)  		    [string trimright $buf "\x00"]]
	} else {
	    set av $iResult
	}
	if {$r == -1} {
	    return -code error [$self Diagnose]
	} else {
	    return $av
	}
    }
    ;# this method actually binds parameters.
    ;# it also selects the associated c types and allocates 
    ;# buffers for parm values.
    method BindParams {args} {
	variable pvBufs
	${apins}::SQLFreeStmt $hstmt 3
	catch {foreach buf $pvBufs { ::ffidlx::buffer free $buf}}
	set pvBufs {}
	variable pvOutput
	set pvOutput {}

	if {!$options(-direct) && ([llength $args]!=$numParams)} {
	    return -code error  		"Wrong number of parameters: $numParams needed, [llength $args] given."
	}
	$self MakePib [llength $args]
	set theIndicator 0
	for {set i 0; set ip 1} {$i<[llength $args]}  	    {   incr i; incr ip; 
		incr theIndicator [::ffidl::info sizeof SQLINTEGER]
	    } {
	    set proposedType [lindex $options(-argtypes) $i]
	    set givenType [$self AskForParamType $ip]
	    set argvar [set arg [lindex $args $i]]
	    set io 1
	    switch -exact -- [string tolower [lindex $proposedType 0]] {input {
		    set proposedType [lrange $proposedType 1 end]
		} inputoutput {
		    set io 2
		    set proposedType [lrange $proposedType 1 end]
		} output {
		    set io 4
		    set proposedType [lrange $proposedType 1 end]
		}}
	    if {$io != 1} {
		upvar 1 $arg pvar
		if {[info exists pvar]} {
		    set arg $pvar
		} else {
		    set arg $options(-null)
		}
	    }
	   
	    set argLength [string length $arg]
	    if {[llength $proposedType]} {
		if {![string is integer [lindex $proposedType 0]]} {
		    lset proposedType 0 $sqltypes([string toupper [lindex $proposedType 0]])
		}
	    } else {
		set proposedType $givenType
	    }
	    foreach {ptnum} {{}} break; foreach {ptnum} $proposedType break; lrange $proposedType 1 end
	    if {$ptnum!=0} {
		set triedTypes [list [lrange [linsert $proposedType end 0 0 0] 0 2]]
		if {$ptnum == -9} {
		    lset proposedType 0 12
		    lappend triedTypes  			[lrange [linsert $proposedType end 0 0 0] 0 2]
		} elseif {$ptnum == -10} {
		    lset proposedType 0 -1
		    lappend triedTypes  			[lrange [linsert $proposedType end 0 0 0] 0 2]
		}
	    } else {
		set wlongArg [expr {$argLength>255}]
		set longArg [expr {[
		    string length [
			encoding convertto $options(-encoding) $arg]]>255}]
		set wLONGorNO [expr {$wlongArg?"LONG":""}]
		set LONGorNO [expr {$longArg?"LONG":""}]
		set triedTypes  		    [list  			[list $sqltypes(W${wLONGorNO}VARCHAR) 0 0]  			[list $sqltypes(${LONGorNO}VARCHAR) 0 0]]
	    }
	    foreach tryType $triedTypes {
		foreach {ptype pscale pprec} {{}} break; foreach {ptype pscale pprec} $tryType break; lrange $tryType 3 end
		set c_type [ctype_from_sqltype $ptype]
		if {$c_type == 1} {
		    set bytes [encoding convertto $options(-encoding) $arg]
		    set charlen [string length $bytes]
		    set required_bytes [expr {$pprec + 1}]
		} elseif {$c_type==-8} {
		    set bytes [encoding convertto unicode $arg]
		    set charlen [string length $arg]
		    set required_bytes [expr {$pprec*2 + 2}]
		} else {
		    set bytes [binary format a* $arg]
		    set charlen [string length $bytes]
		    set required_bytes $pprec
		}
		set strlen [string length $bytes]
		# SQL_NTS for character types. 
		if {$c_type != -2} {
		    set bytes [binary format a*x2 $bytes[set bytes {}]]
		    set strlen -3
		}
		if {$io != 1 && [string length $bytes]<$required_bytes} {
		    set bytes [binary format a*@$required_bytes $bytes]
		}
		set buf [::ffidlx::buffer create $bytes]
		if {!$pprec} { set pprec $charlen}
		if {!$pprec} { set pprec 1}
		set bound [${apins}::SQLBindParameter $hstmt $ip $io $c_type  			$ptype $pprec $pscale [::ffidlx::buffer addr $buf]  			[string length $bytes]  			[expr {$theIndicator+$pib}]]
		if {$options(-null) ne {} && $arg eq $options(-null)} {
		    set strlen -1;# SQL_NULL_DATA
		}
		if {$options(-default) ne {} && $arg eq $options(-default)} {
		    set strlen -5;# SQL_DEFAULT_PARAM
		}
		::ffidlx::poke $pib $theIndicator $strlen
		if {$bound == -1} {
		    ::ffidlx::buffer free $buf
		} else {
		    lappend pvBufs $buf
		    if {$io != 1} {
			lappend pvOutput [list $argvar [expr {[info level]-1}] $c_type [string length $bytes]]
		    } else {
			lappend pvOutput {}
		    }
		    break
		}
	    }
	    if {$bound == -1} {
		return -code error "Parameter $ip couldn't be bound!"
	    }
	}
    }
    method columns {args} {
	if {[llength $args]} {
	    set attrlist $args
	} else {
	    set attrlist columnname
	}
	set result [list]
	set ncol [$self GetColumnAttr 1 count]
	for {set i 1} {$i<=$ncol} {incr i} {
	    set cdl [list]
	    foreach attr $attrlist {
		lappend cdl [$self GetColumnAttr $i $attr]
	    }
	    lappend result $cdl
	}
	return $result
    }
    method execute {{argList {}}} {
	${apins}::SQLCloseCursor $hstmt
	variable desc_column_cache
	unset -nocomplain desc_column_cache
	if {!$isSpecial} {
	    uplevel 1 [eval [list list] [list $self] [list BindParams] [lrange $argList 0 end]]
	    if {$options(-direct)} {
		set execrslt [${apins}::SQLExecDirect $hstmt [LPSTR $options(-sql)] -3]
	    } else {
		set execrslt [${apins}::SQLExecute $hstmt]
	    }
	} else {
	    set execrslt [eval [list $self] [list SpecialExecute] [list $specialFunction] [lrange $argList 0 end]]
	}
	if {$pib} {
	    variable pvBufs
	    variable pvOutput
	    set theIndicator 0
	    foreach pvbuf $pvBufs pvoutput $pvOutput {
		if {[llength $pvoutput]} {
		    foreach {varname level c_type bytesize} {{}} break; foreach {varname level c_type bytesize} $pvoutput break; lrange $pvoutput 4 end
		    upvar #$level $varname writevar
		    set strlen [::ffidlx::peek $pib $theIndicator]
		    set data [::ffidlx::buffer data $pvbuf]
		    if {$c_type == -8} {;# WCHAR
			set enc unicode
		    } elseif {$c_type == 1} {
			set enc $options(-encoding)
		    } else {
			set enc ""
		    }
		    if {$enc ne ""} {
			set data [encoding convertfrom $enc $data]
		    }
		    if {$strlen == 0} {
			set writevar {}
		    } elseif {$strlen == -1} { ;# SQL_NULL_DATA
			set writevar $options(-null)
		    } elseif {$strlen == -3} {
			set writevar [string range $data 0 [expr {[string first "\x00" $data]-1}]]
		    } else {
			set writevar [string range $data 0 [expr {$strlen-1}]]
		    }
		}
		incr theIndicator [::ffidl::info sizeof SQLINTEGER]
	    }
	    catch {foreach buf $pvBufs { ::ffidlx::buffer free $buf}}
	    set pvBufs {}
	}

	if {$execrslt == -1} { return -code error [$self Diagnose]}
	if {$execrslt == 1} { $self Diagnose}
	${apins}::SQLNumResultCols $hstmt [LP SQLSMALLINT numColumns]
    }
    method run {{argList {}}} {
	uplevel 1 [list $self execute $argList]
	set result 0
	if {$numColumns} {
	    set result [$self ResultAsListOrLOL]
	} else {
	    ${apins}::SQLRowCount $hstmt [LP SQLINTEGER result]
	}
	while {![catch {$self moreresults} mr] && $mr} {}
	catch { ${apins}::SQLCloseCursor $hstmt}
	return $result
    }
    method fetch {{arrayName {}} {colNames {}}} {
	set rc [${apins}::SQLFetch $hstmt]
	if {$rc==1} {$self Diagnose}
	if {$rc==-1} {return -code error [$self Diagnose]}
	if {$arrayName eq ""} {
	    if {$rc==100} {return ""}
	    set row [list]
	    for {set i 1} {$i<=$numColumns} {incr i} {
		lappend row [$self GetData $i]
	    }
	    return $row
	} else {
	    upvar 1 $arrayName rowArray
	    if {$rc==100} {return 0}
	    for {set i 1} {$i<=$numColumns} {incr i} {
		set cname [lindex $colNames [expr {$i-1}]]
		if {$cname eq ""} { foreach {cname} {{}} break; foreach {cname} [$self DescribeColumn $i] break; lrange [$self DescribeColumn $i] 1 end}
		set rowArray($cname) [$self GetData $i]
	    }
	    return 1
	}
    }
    method scroll {dir {n 0} {arrayName {}} {colNames {}}} {
	set iDir [dict get {absolute 5 relative 6 
	    first 2 last 3 next 1 prior 4} $dir]
	set rc [${apins}::SQLFetchScroll $hstmt $iDir $n]
	if {$rc==-1} {return -code error [$self Diagnose]}
	if {$arrayName eq ""} {
	    if {$rc==100} {return ""}
	    set row [list]
	    for {set i 1} {$i<=$numColumns} {incr i} {
		lappend row [$self GetData $i]
	    }
	    return $row
	} else {
	    upvar 1 $arrayName rowArray
	    if {$rc==100} {return 0}
	    for {set i 1} {$i<=$numColumns} {incr i} {
		set cname [lindex $colNames [expr {$i-1}]]
		if {$cname eq ""} { foreach {cname} {{}} break; foreach {cname} [$self DescribeColumn $i] break; lrange [$self DescribeColumn $i] 1 end}
		set rowArray($cname) [$self GetData $i]
	    }
	    return 1
	}
    }
    method eval {cmd {argList {}}} {
	uplevel 1 [list $self execute $argList]
	while {[llength [set row [$self fetch]]]} {
	    uplevel 1 $cmd $row
	}
	${apins}::SQLCloseCursor $hstmt
	return {}
    }
    method read {arrays {argList {}}} {
	uplevel 1 [list $self execute $argList]
	set i 0
	foreach uv $arrays { upvar 1 $uv ar_[incr i]}
	set rowcount 0
	while {[llength [set row [$self fetch]]]} {
	    set key [lindex $row 0]
	    set col 2
	    foreach other [lrange $row 1 end] {
		if {[llength $arrays==1]&&$numColumns>2} {
		    foreach {cname} {{}} break; foreach {cname} [$self DescribeColumn $col] break; lrange [$self DescribeColumn $col] 1 end
		    set ar_1($key,$cname) $other
		} else {
		    set ar_[expr {$col-1}]($key) $other
		}
		incr col
	    }
	    incr rowcount
	}
	return $rowcount
    }
    method rowcount {} {
	if {[${apins}::SQLRowCount $hstmt [LP SQLINTEGER result]]==-1} {
	    return -code error [$self Diagnose]
	} else {
	    return $result
	}
    }
    method moreresults {} {
	set r [${apins}::SQLMoreResults $hstmt]
	if {$r==100} {return 0}
	if {$r==1} {$self DiagnoseAppend}
	if {$r==-1} {return -code error [$self DiagnoseAppend]}
	return 1
    }
    method set {opt value} {
	if {[info exists options(-$opt)]} {
	    $self configure -$opt $value
	    return $value
	} else {
	    if {[string is boolean $value]} { set value [expr {$value? 1 : 0}]}
	    foreach {optCode} {{}} break; foreach {optCode} [dict get $::odbc::ATTRS $opt] break; lrange [dict get $::odbc::ATTRS $opt] 1 end
	    set avdict [lrange [dict get $::odbc::ATTRS $opt] 1 end]
	    if {[llength $avdict]} {
		set value [dict get $avdict $value]
	    }
	    set rc [${apins}::SQLSetStmtAttr/int $hstmt $optCode $value 0]
	    if {$rc == -1} {
		return -code error [$self Diagnose]
	    } elseif {$rc == 1} {
		return [$self get $opt]
	    } else {
		return $value
	    }
	}
    }
    method get {opt} {
	if {[info exists options(-$opt)]} {
	    return [$self cget -$opt]
	}
	set avdict [foreach {optCode} {{}} break; foreach {optCode} [dict get $::odbc::ATTRS $opt] break; lrange [dict get $::odbc::ATTRS $opt] 1 end]
	set rc [${apins}::SQLGetStmtAttr/int $hstmt $optCode [LP SQLINTEGER value]  	    0 [LP SQLINTEGER _]]
	if {[llength $avdict]} {
	    dict for {k v} $avdict { if {$value==$k} {set value $v; break} }
	}
	return $value
    }
    method GetData {nth} {
	if {![info exists gdBuffer]} {
	    set gdBuffer [binary format @1024]
	}
	foreach {nm tp sc pr nl} {{}} break; foreach {nm tp sc pr nl} [$self DescribeColumn $nth] break; lrange [$self DescribeColumn $nth] 5 end
	set c_type [ctype_from_sqltype $tp]
	set real_max_length [expr {1024-[ctype_termination $c_type]}]
	set bin ""
	while 1 {
	    set rc [${apins}::SQLGetData  $hstmt $nth $c_type gdBuffer 1024  		    [LP SQLINTEGER indicator]]
	    if {$rc==-1} { return -code error "Couldn't SQLGetData"}
	    if {$rc==100} { break}
	    if {$indicator==-1} { return $options(-null)}
	    set dl [expr {
		    ($indicator>$real_max_length||$indicator==-4)?
		    $real_max_length: $indicator}]
	    append bin [string range $gdBuffer 0 [expr {$dl-1}]]
	    # real
	    if {$indicator>=0 && $indicator<=$real_max_length} {break}
	}
	switch -exact -- $c_type {-8 {encoding convertfrom unicode $bin} -2 {set bin} 1 {encoding convertfrom $options(-encoding) $bin}}
    }
    method ResultAsListOrLOL {} {
	set rows [list]
	while {1} {
	    set rc [${apins}::SQLFetch $hstmt]
	    if {$rc !=0 && $rc !=1 } {break}
	    if {$rc == 1} {
		$self DiagnoseAppend
	    }
	    set row [list]
	    for {set i 1} {$i<=$numColumns} {incr i} {
		set item [$self GetData $i]
		if {$numColumns==1} {
		    set row $item
		} else {
		    lappend row $item
		}
	    }
	    lappend rows $row
	}
	return $rows
    }
    method {SpecialExecute tables} {{pattern {%}} args} {
	dict set args -table $pattern
	set stargs [list]
	set pbufs [list]
	foreach spxo {catalog schema table type} {
	    if {[dict exists $args -$spxo]} {
		set arg [dict get $args -$spxo]
		set binarg [encoding convertto $options(-encoding) $arg]
		set buf [::ffidlx::buffer create $binarg]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [eval [list ${apins}::SQLTables/raw] [list $hstmt] [lrange $stargs 0 end]]
	foreach buf $pbufs { ::ffidlx::buffer free $buf}
	return $rslt
    }
    method {SpecialExecute columns} {{table {%}} args} {
	dict set args -table $table
	set stargs [list]
	set pbufs [list]
	foreach spxo {catalog schema table column} {
	    if {[dict exists $args -$spxo]} {
		set arg [dict get $args -$spxo]
		set binarg [encoding convertto $options(-encoding) $arg]
		set buf [::ffidlx::buffer create $binarg]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [eval [list ${apins}::SQLColumns/raw] [list $hstmt] [lrange $stargs 0 end]]
	foreach buf $pbufs { ::ffidlx::buffer free $buf}
	return $rslt
    }
    method {SpecialExecute rowid} {table args} {
	dict set args -table $table
	set stargs [list]
	set pbufs [list]
	foreach spxo {catalog schema table} {
	    if {[dict exists $args -$spxo]} {
		set arg [dict get $args -$spxo]
		set binarg [encoding convertto $options(-encoding) $arg]
		set buf [::ffidlx::buffer create $binarg]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set scope [dict get {row 0 currow 0 transaction 1 session 2}  	    [from args -scope row]]
	if {[from args -nullable 1]} {set nullable 1} {set nullable 0}
	set rslt [eval [list ${apins}::SQLSpecialColumns/raw] [list $hstmt] [list 1] [lrange $stargs 0 end] [list $scope] [list $nullable]]
	foreach buf $pbufs { ::ffidlx::buffer free $buf}
	return $rslt
    }
    method {SpecialExecute primarykeys} {table args} {
	dict set args -table $table
	set stargs [list]
	set pbufs [list]
	foreach spxo {catalog schema table} {
	    if {[dict exists $args -$spxo]} {
		set arg [dict get $args -$spxo]
		set binarg [encoding convertto $options(-encoding) $arg]
		set buf [::ffidlx::buffer create $binarg]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [eval [list ${apins}::SQLPrimaryKeys/raw] [list $hstmt] [lrange $stargs 0 end]]
	foreach buf $pbufs { ::ffidlx::buffer free $buf}
	return $rslt
    }
    method {SpecialExecute typeinfo} {typename} {
	if {![string is integer $typename]} {
	    set typename $sqltypes([string toupper $typename])
	}
	${apins}::SQLGetTypeInfo $hstmt $typename
    }
    method {SpecialExecute indexes} {table args} {
	dict set args -table $table
	set stargs [list]
	set pbufs [list]
	foreach spxo {catalog schema table} {
	    if {[dict exists $args -$spxo]} {
		set arg [dict get $args -$spxo]
		set binarg [encoding convertto $options(-encoding) $arg]
		set buf [::ffidlx::buffer create $binarg]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set uidx [expr {[from args -unique 0]?0:1}]
	set rslt [eval [list ${apins}::SQLStatistics/raw] [list $hstmt] [lrange $stargs 0 end] [list $uidx] [list 0]]
	foreach buf $pbufs { ::ffidlx::buffer free $buf}
	return $rslt
    }
    method {SpecialExecute procedures} {{pattern {%}} args} {
	dict set args -proc $pattern
	set stargs [list]
	set pbufs [list]
	foreach spxo {catalog schema proc} {
	    if {[dict exists $args -$spxo]} {
		set arg [dict get $args -$spxo]
		set binarg [encoding convertto $options(-encoding) $arg]
		set buf [::ffidlx::buffer create $binarg]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [eval [list ${apins}::SQLProcedures/raw] [list $hstmt] [lrange $stargs 0 end]]
	foreach buf $pbufs { ::ffidlx::buffer free $buf}
	return $rslt
    }
    method {SpecialExecute procedurecolumns} {{proc {%}} args} {
	dict set args -proc $proc
	set stargs [list]
	set pbufs [list]
	foreach spxo {catalog schema proc column} {
	    if {[dict exists $args -$spxo]} {
		set arg [dict get $args -$spxo]
		set binarg [encoding convertto $options(-encoding) $arg]
		set buf [::ffidlx::buffer create $binarg]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [eval [list ${apins}::SQLProcedureColumns/raw] [list $hstmt] [lrange $stargs 0 end]]
	foreach buf $pbufs { ::ffidlx::buffer free $buf}
	return $rslt
    }
    delegate method HDBC to dbConn
    destructor { catch { ${apins}::SQLFreeHandle 3 $hstmt}}
    typeconstructor {
	if {[catch {namespace path}]} {
	    namespace import ::odbc::* ::ffidlx::*
	} else {
	    namespace path [linsert [namespace path] end ::odbc ::ffidlx]
	}
    }
}

namespace eval ::odbc { namespace export database}
package provide odbc::wrap 1.1
package require odbc::obj
namespace eval ::odbc::wrapped {}
variable ::odbc::wrap_seq 0

proc ::odbc::wrap_tclodbc_database {args} {
    # Creating my private object 
    set arg0 [lindex $args 0]
    if {$arg0 eq "drivers" || $arg0 eq "configure" || $arg0 eq "datasources" || $arg0 eq "extension"} {
	eval [list ::odbc::database] [lrange $args 0 end]
    } else {
	set myobj [uplevel 1 [linsert $args 0 ::odbc::database]]
	rename $myobj [set newname ::odbc::wrapped::[incr ::odbc::wrap_seq]]
	interp alias {} $myobj {} ::odbc::wrap_tclodbc_instance $newname
	trace add command $newname delete "[list rename $myobj {}];#"
	return $myobj
    }
}

proc ::odbc::wrap_tclodbc_instance {oobj args} {
    set mtd [lindex $args 0]
    if {![llength [$oobj info methods $mtd]]} {
	uplevel 1 [linsert $args 0 $oobj run]
    } else {
	set myobj [uplevel 1 [linsert $args 0 $oobj]]
	if {$mtd eq "statement"} {
	    rename $myobj [set newname ::odbc::wrapped::[incr ::odbc::wrap_seq]]
	    interp alias {} $myobj {} ::odbc::wrap_tclodbc_statement $newname
	    trace add command $newname delete "[list rename $myobj {}];#"
	    return $myobj
	} else {
	    return $myobj
	}
    }
}

proc ::odbc::wrap_tclodbc_statement {oobj args} {
    set mtd [lindex $args 0]
    if {![llength [$oobj info methods $mtd]]} {
	uplevel 1 [linsert $args 0 $oobj run]
    } else {
	uplevel 1 [linsert $args 0 $oobj]
    }
}
namespace eval ::odbc { namespace export wrap_tclodbc_database}
package require odbc::wrap 1.1
namespace eval ::odbc {}
proc ::odbc::setup_tclodbc_compatibility {} {
	namespace eval :: {
		namespace import ::odbc::wrap_tclodbc_database
	}
	if {[info commands ::wrap_tclodbc_database]ne {}} {
		rename ::wrap_tclodbc_database ::database
	}
}
package provide snodbc 1.2.20080522
::odbc::setup_tclodbc_compatibility
# Poor man's dict -- a pure tcl [dict] emulation
# Very slow, but complete.
#
# Not all error checks are implemented!
# e.g. [dict create odd arguments here] will work
#
# Implementation is based on lists, [array set/get]
# and recursion

if {![llength [info commands ::dict]] && [catch {package require dict 8.5.1}]} {
    namespace eval ::dict {}
    interp alias {} ::dict {} namespace inscope ::dict
    proc ::dict::update {dvar args} {
	::set name [string map {: {} ( {} ) {}} $dvar]
	upvar 1 $dvar dv
	upvar 1 _my_dict_array$name local

	array set local $dv
	foreach {k v} [lrange $args 0 end-1] {
	    if {[info exists local($k)]} {
	    if {![uplevel 1 [list info exists $v]]} {
		uplevel 1 [list upvar 0 _my_dict_array${name}($k) $v]
	    } else {
		uplevel 1 [list ::set $v $local($k)]
	    }
	    }
	}
	::set code [catch {uplevel 1 [lindex $args end]} res]

	foreach {k v} [lrange $args 0 end-1] {
	    if {[uplevel 1 [list info exists $v]]} {
	    ::set local($k) [uplevel 1 [list ::set $v]]
	    } else {
		::unset -nocomplain local($k)
	    }
	}
	::set dv [array get local]
	::unset local

	return -code $code $res
    }
    proc ::dict::get {dv args} {
        if {![llength $args]} {return $dv} else {
            array set dvx $dv
            ::set key [lindex $args 0]
            ::set dv $dvx($key)
            ::set args [lrange $args 1 end]
            return [eval [linsert $args 0 ::dict::get $dv]]
        }
    }
    proc ::dict::exists {dv key args} {
        array set dvx $dv
        ::set r [info exists dvx($key)]
        if {!$r} {return 0}
        if {[llength $args]} {
            return [eval [linsert $args 0 ::dict::exists $dvx($key)]]
        } else {return 1}
    }
    proc ::dict::set {dvar key value args } {
        upvar 1 $dvar dv
        if {![info exists dv]} {::set dv [list]}
        array set dvx $dv
        if {![llength $args]} {
            ::set dvx($key) $value
        } else {
            eval [linsert $args 0 ::dict::set dvx($key) $value]
        }
        ::set dv [array get dvx]
    }
    proc ::dict::unset {dvar key args} {
        upvar 1 $dvar mydvar
        if {![info exists mydvar]} {return}
        array set dv $mydvar
        if {![llength $args]} {
            if {[info exists dv($key)]} {
                ::unset dv($key)
            }
        } else {
            eval [linsert $args 0 ::dict::::unset dv($key)]
        }
        ::set mydvar [array get dv]
        return {}
    }
    proc ::dict::keys {dv {pat *}} {
        array set dvx $dv
        return [array names dvx $pat]
    }
    proc ::dict::append {dvar key {args}} {
        upvar 1 $dvar dv
        if {![info exists dv]} {::set dv [list]}
        array set dvx $dv
        eval [linsert $args 0 append dvx($key)]
        ::set dv [array get dvx]
    }
    proc ::dict::create {args} {
        return $args
    }
    proc ::dict::filter {dv ftype args} {
        ::set r [list]
        foreach {globpattern} $args {break}
        foreach {varlist script} $args {break}

        switch $ftype {key {
                foreach {key value} $dv {
                    if {[string match $globpattern $key]} {
                        lappend r $key $value
                    }
                }
            } value {
                foreach {key value} $dv {
                    if {[string match $globpattern $value]} {
                        lappend r $key $value
                    }
                }
            } script {
                foreach {Pkey Pval} $varlist {break}
                upvar 1 $Pkey key $Pval value
                foreach {key value} $dv {
                    if {[uplevel 1 $script]} {
                        lappend r $key $value
                    }
                }
            } default {
                error "Wrong filter type"
            }}
        return $r
    }
    proc ::dict::for {kv dict body} {
        uplevel 1 [list foreach $kv $dict $body]
    }
    proc ::dict::incr {dvar key {incr 1}} {
        upvar 1 $dvar dv
        if {![info exists dv]} {::set dv [list]}
        array set dvx $dv
        if {![info exists dvx($key)]} {::set dvx($key) 0}
        incr dvx($key) $incr
        ::set dv [array get dvx]
    }
    proc ::dict::info {dv} {
        return "Dictionary is represented as plain list"
    }
    proc ::dict::lappend {dvar key args} {
        upvar 1 $dvar dv
        if {![info exists dv]} {::set dv [list]}
        array set dvx $dv
        eval [linsert $args 0 lappend dvx($key)]
        ::set dv [array get dvx]
    }
    proc ::dict::merge {args} {
        foreach dv $args {
            array set dvx $dv
        }
        array get dvx
    }
    proc ::dict::replace {dv args} {
        foreach {k v} $args {
            ::dict::set dv $k $v
        }
        return $dv
    }
    proc ::dict::remove {dv args} {
        foreach k $args {
            ::dict::unset dv $k
        }
        return $dv
    }
    proc ::dict::size {dv} {
        return [expr {[llength $dv]/2}]
    }
    proc ::dict::values {dv {gp *}} {
        ::set r [list]
        foreach {k v} $dv {
            if {[string match $gp $v]} {
                lappend r $v
            }
        }
        return $r
    }
}
proc lassign_is_broken_or_missing {} {
    catch {
	set rest [foreach {a} {{}} break; foreach {a} {two values} break; lrange {two values} 1 end]
	if {![llength $rest]} {
	    error "lassign doesn't return the rest of the list, thus it's broken"
	}
    }
}
if {[lassign_is_broken_or_missing]} {
    proc lassign {listValue args} {
	set i 0
	set argc [llength $args]
	foreach arg $args value $listValue {
	    if {[incr i]>$argc} break
	    upvar 1 $arg myArg
	    set myArg $value
	}
	lrange $listValue $argc end
    }
}
rename lassign_is_broken_or_missing {}
