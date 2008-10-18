package provide odbc::wrap 1.1
package require odbc::obj
namespace eval ::odbc::wrapped {}
variable ::odbc::wrap_seq 0

proc ::odbc::wrap_tclodbc_database {args} {
    # Creating my private object 
    set arg0 [lindex $args 0]
    if {$arg0 eq "drivers" || $arg0 eq "configure" || $arg0 eq "datasources" || $arg0 eq "extension"} {
	::odbc::database {*}$args
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
namespace eval ::odbc { namespace export wrap_tclodbc_database }

