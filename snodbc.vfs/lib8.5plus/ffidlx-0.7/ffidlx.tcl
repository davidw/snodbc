package provide Ffidlx 0.7
package require Ffidl 0.6
package require reftrace

;# Creating callouts for interesting Tcl stubs
    namespace eval ::ffidlx {
	namespace export {[A-Za-z]*}
    }
    namespace eval ::ffidlx::rma {}
    ;# Tcl_Alloc, stub #3 was replaced with Tcl_AttemptAlloc
    ::ffidl::callout ::ffidlx::rma::malloc {uint32} pointer \
	[::ffidl::stubsymbol tcl stubs 428]

    # Tcl_Free, stub #4
    ::ffidl::callout ::ffidlx::rma::free {pointer} void \
	[::ffidl::stubsymbol tcl stubs 4]

    # Tcl_GetIntFromObj, stub #38. We use it to write a word 
    # at the arbitrary address
    ::ffidl::callout ::ffidlx::rma::getIntFromObj \
	{pointer pointer-obj pointer} int \
	[::ffidl::stubsymbol tcl stubs 38]
    # Tcl_GetLongFromObj, stub #39. We use it to write a long
    # at the arbitrary address
    
    # it's important to be able to write pointers 
    ::ffidl::callout ::ffidlx::rma::getLongFromObj \
	{pointer pointer-obj pointer} int \
	[::ffidl::stubsymbol tcl stubs 38]

    # Tcl_GetWideInt, to write 64-bit values.
    ::ffidl::callout ::ffidlx::rma::getWideFromObj \
	{pointer pointer-obj pointer} int \
	[::ffidl::stubsymbol tcl stubs 487]

    # Tcl_LinkVar 

    # Tcl_NewByteArrayObj, stub #50. Get binary byte array from 
    # memory
    ::ffidl::callout ::ffidlx::rma::newByteArray {pointer uint32} \
	pointer-obj [::ffidl::stubsymbol tcl stubs 50]

    # These three are useful for building ::ffidlx::buffer -
    # byte array TclObj of which the interpreter doesn't know.
    ::ffidl::callout ::ffidlx::rma::newByteArray/ptr {pointer-byte uint32} \
	pointer [::ffidl::stubsymbol tcl stubs 50]
    ::ffidl::callout ::ffidlx::rma::getByteArray/ptr  {pointer pointer} \
	pointer [::ffidl::stubsymbol tcl stubs 33]
    ::ffidl::callout ::ffidlx::rma::freeObj/ptr  {pointer} \
	void [::ffidl::stubsymbol tcl stubs 19]

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
	if {![info exists iv]} { set iv 0 }
	set bfmt [::ffidl::info format $type]
	::reftrace::MkRefLowLevel [list [list binary format $bfmt] {}] \
	    [list [namespace code _decode_LP] $bfmt] 1 iv
    }
    proc ::ffidlx::_decode_LP {targetVarName bfmt value} {
	upvar 2 $targetVarName targetVar
	binary scan $value $bfmt targetVar
    }

;# Data reformatting
    proc ::ffidlx::LPSTR {str {encoding {}}} {
	if {$encoding eq ""} { set encoding [encoding system] }
	binary format a*x [encoding convertto $encoding $str]
    }
    proc ::ffidlx::LPWSTR {str } {
	binary format a*x2 [encoding convertto unicode $str]
    }

;# Call factory

proc ::ffidlx::callfactory {name args} {
    set callerNs [uplevel 1 [list namespace current]]
    if {![string match ::* $name ]} {
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
    if {![string match ::* $name ]} {
	set name ${callerNs}::$name
    }
    array set opts $options
    if {[string match */* $name ]} {
	foreach {realname suffix} [split $name /] {break}
	set suffix /$suffix
    } else {
	set suffix {}
	set realname $name
    }
    if {[string match *(W) $realname]} {
	# looking for optional unicode and mandatory ansi is typical
	eval [linsert $args 0 \
		_actualfactory $options \
		$rettype [string range $realname 0 end-3]$suffix]
	eval [linsert $args 0 \
		_actualfactory [linsert $options end -optional 1] \
		$rettype [string range $realname 0 end-3]W$suffix]
	return
    }
    # We allow multiple callouts for a single function,
    # e.g. SearchPathW/nopath to pass NULL as a path.
    set funcName [lindex [split [namespace tail $name] / ] 0]
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
