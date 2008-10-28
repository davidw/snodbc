# vim: ft=tcl
package provide odbc::obj 1.2

package require odbc::ffi
package require Ffidlx
package require getdata
package require snit

namespace eval ::odbc {}

# \uFFFF is a convenient value to represent NULLs,
# as it's (almost) never legitimate in other contexts.
#
# Snodbc doesn't use it by default - NULLs are represented as empty strings
# to preserve tclodbc compatibility

proc ::odbc::NULL {} { return "\uFFFF" }
proc ::odbc::DEFAULT {} { return "\uFFFFFFFF" }

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

::snit::type ::odbc::environment {
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
		switch -exact -- $::tcl_platform(platform) {
		    windows {
			set defaultlibraries {{odbc32 odbccp32}}
		    }
		    unix {
			set defaultlibraries [list "libodbc[info sharedlibextension].1 libodbcinst[info sharedlibextension].1"]
		    }
		    default {
			set defaultlibraries {{}}
		    }
		}
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
	catch { ${apins}::SQLFreeHandle 1 $henv }
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
	    set r [${apins}::SQLGetDiagRec $htype $handle $recno \
		    sqlState [::ffidlx::LP SQLINTEGER nativeError] \
		    msgBuf 1024 [::ffidlx::LP SQLSMALLINT msgBufRealLen]]
	    if {$r==100 || $r==-1} {break}
	    set sqlMessage [encoding convertfrom \
		[string range $msgBuf 0 [expr {$msgBufRealLen-1}]]]
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
		set r [${apins}::SQLDriversW $henv $dir \
			dvDescr $bufDescr [LP SQLSMALLINT nbufDescr] \
			dvAttr $bufAttr [LP SQLSMALLINT nbufAttr]]
		if {$r==100} {
		    set more 0
		    break
		} elseif {$bufDescr <= $nbufDescr } {
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
		set dvDescr [encoding convertfrom unicode \
			[string trimright $dvDescr "\x00"]\x00]
		set dvAttr [encoding convertfrom unicode \
			[string trimright $dvAttr "\x00"]\x00]
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
	    for {set dir [expr {$which eq "all"? 2 : ($which eq "user"? 31 : 32)}]} \
		{$more} {set dir 1} {
		set dvName [binary format @$bufName]
		set dvDescr [binary format @$bufDescr]
		set r [${apins}::SQLDataSources $henv $dir \
			dvName $bufName [LP SQLSMALLINT nbufName] \
			dvDescr $bufDescr [LP SQLSMALLINT nbufDescr]]
		if {$r==100} {
		    set more 0
		    break
		} elseif {$bufName <= $nbufName } {
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
		set dvName [string trimright [encoding convertfrom [\
			string range $dvName 0 [expr {$nbufName*2}]]] "\x00"]
		set dvDescr [string trimright [encoding convertfrom [\
			string range $dvDescr 0 [expr {$nbufDescr*2}]]] "\x00"]
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
		    ${apins}::SQLInstallerError $i [LP SQLINTEGER ec] \
			msgbuf 1024 [LP uint16 msgl]]
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
	    switch -exact -- $pkg {
		iodbc {
		    set defaultlibraries [
		    list \
			"libiodbc[info sharedlibextension].2 libiodbcinst[info sharedlibextension].2" \
			"libodbc[info sharedlibextension].1 libodbcinst[info sharedlibextension].1" \
			]

		}
		unixodbc {
		    set defaultlibraries [
		    list \
			"libodbc[info sharedlibextension].1 libodbcinst[info sharedlibextension].1" \
			"libiodbc[info sharedlibextension].2 libiodbcinst[info sharedlibextension].2" \
			]
		}
	    }
	}
    }
}

::snit::type ::odbc::database {
    typevariable henv
    typevariable defaultEnvironment {::odbc::DEFAULTENVIRONMENT}
    variable hdbc 0
    variable autoStmtCache ;#{}
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
		lassign $args dsn uid pwd
		set args [list datasource $dsn -uid $uid -pwd $pwd]
	    }
	}
	set result [$self Connect {*}$args {*}$optargs]
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
	$self Preconfigure {*}$args
	${apins}::SQLConnect $hdbc [LPSTR $dsn] -3 \
	    [LPSTR [from args -uid] ] -3 \
	    [LPSTR [from args -pwd] ] -3
    }
    typevariable apart_thread_id {}
    variable promptvals -array {none 0 complete 1 all 2 required 3}
    method {Connect driver} {connstring args} {
	$self Preconfigure {*}$args
	if {$connstring eq "?"} {
	    set connstring ""
	    set window [from args -window x]
	    if {$window eq "x"} {
		catch {set window [winfo toplevel [focus]]}
		if {$window eq "x"} {
		    set window .
		}
	    }
	    lappend args \
		-window $window \
		-prompt [from args -prompt all]
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
	    if {$window ne ""} { lappend args -hwnd [wm frame $window] }
	    # let's autoload dict for tcl 8.4
	    if {$apart_thread_id eq ""} {
		package require Thread
		package require odbc::apart
		set tid [thread::create]
		catch { thread::send $tid [list load {} Ffidl] } 
		catch { thread::send $tid [list load {} dict] }
		catch { thread::send $tid [::odbc::apart_hook] }
		thread::send $tid {package require odbc::obj}
		set apart_thread_id $tid
	    }  else {
		set tid $apart_thread_id
	    }
	    if 1 {
		thread::send -async $tid \
		    "catch {[linsert $args 0 ::odbc::database fconn driver $connstring]}
		     set cstr {}
		     catch {set cstr \[fconn get connectionstring\]}
		     catch {fconn disconnect}
		     set cstr" [myvar cstr]
		vwait [myvar cstr]
	    } else {
		set cstr [
		thread::send $tid \
		    "[linsert $args 0 ::odbc::database fconn driver $connstring];
		     set cstr \[fconn get connectionstring\]
		     fconn disconnect
		     set cstr"
		]
	    }
	    set args {}
	    set connstring $cstr
	    if {$connstring eq ""} { return -code error "CANCELLED" }
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
	set r [${apins}::SQLDriverConnect $hdbc $hwnd $bcsZ [string length $bcs] obf 2048 \
	    [LP SQLINTEGER _] $iPrompt]
	$self configure -connectionstring \
	    [encoding convertfrom [string trimright $obf "\x00"]]
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
		$directstatement configure -sql $sql -argtypes $argTypes \
		    -encoding $options(-encoding) -null $options(-null) \
		    -default $options(-default)
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
    method tables {args} { $self run tables {} $args }
    method columns {args} { $self run columns {} $args }
    method rowid {args} { $self run rowid {} $args }
    method primarykeys {args} { $self run primarykeys {} $args }
    method procedures {args} { $self run procedures {} $args }
    method procedurecolumns {args} { $self run procedurecolumns {} $args }
    method indexes {args} { $self run indexes {} $args }

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
		if {[$stmt HDBC] == $hdbc} { $stmt configure -$opt $value }
	    }
	    return $value
	} else {
	    if {[string is boolean $value]} { set value [expr {$value? 1 : 0}] }
	    lassign [dict get $::odbc::ATTRS $opt] optCode
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
	set avdict [lassign [dict get $::odbc::ATTRS $opt] optCode]
	set rc [${apins}::SQLGetConnectAttr/int $hdbc $optCode [LP SQLINTEGER value] \
	    0 [LP SQLINTEGER _]]
	if {[llength $avdict]} {
	    dict for {k v} $avdict { if {$value==$k} {set value $v; break} }
	}
	return $value
    }
    method commit {} { $self EndTran 0 }
    method rollback {} { $self EndTran 1 } 
    method EndTran {how} {
	set rc [${apins}::SQLEndTran 2 $hdbc $how]
	if {$rc==-1} { return -code error [$self Diagnose] }
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
	    if {[$stmt HDBC] == $hdbc} { $stmt destroy }
	}
	if {$connected} {
	    ${apins}::SQLDisconnect $hdbc
	    ${apins}::SQLFreeHandle 2 $hdbc
	}
    }
}

::snit::type ::odbc::statement {
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
	switch -exact -- [string toupper $ctype] {
	    WCHAR { set ctype -8 }
	    CHAR { set ctype 1}
	    BINARY { set ctype -2}
	    default {
		return -code error "C type must be one of WCHAR, CHAR and BINARY"
	    }
	}
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
	lassign $args argTypes
	set rest [lrange $args 1 end]
	set stmt [uplevel 1 \
	    [mytypemethod create $stmtname -dbc $conn -direct [from rest -direct 0] -sql $sql \
		-argtypes $argTypes -encoding [$conn cget -encoding] \
		-null [$conn cget -null]] -default [$conn cget -default]]
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
	    set r [${apins}::SQLPrepare $hstmt [LPSTR $sql ] -3]
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
		lassign {0 0 0 0} ptype pscale pprec pnull
		catch {
		    ${apins}::SQLDescribeParam $hstmt $nth \
			[LP SQLSMALLINT ptype]  [LP SQLINTEGER pscale] \
			[LP SQLSMALLINT pprec]  [LP SQLSMALLINT pnull]
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

	    ${apins}::SQLDescribeCol $hstmt $nth \
		cname 256 \
		    [LP SQLSMALLINT cnameLen] \
		    [LP SQLSMALLINT ctype] \
		    [LP SQLINTEGER cscale] \
		    [LP SQLSMALLINT cprec ] \
		    [LP SQLSMALLINT nullable ]
	    set cname [encoding convertfrom [string trimright $cname "\x00"]]
	    set desc_column_cache($nth) \
		[list $cname $ctype $cscale $cprec $nullable]
	} 
	return $desc_column_cache($nth)
    }

    method GetColumnAttr {nth attr} {
	set attrcode [dict get $columnattrs $attr]
	set buf [binary format @256]
	set r [${apins}::SQLColAttribute $hstmt $nth $attrcode \
		buf 256 [LP SQLSMALLINT realLength] [LP SQLINTEGER iResult]]
	if {[lsearch $columnattrs_str $attr]!=-1}  {
	    set av [encoding convertfrom $options(-encoding) \
		    [string trimright $buf "\x00"]]
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
	catch {foreach buf $pvBufs { ::ffidlx::buffer free $buf }}
	set pvBufs {}
	variable pvOutput
	set pvOutput {}

	if {!$options(-direct) && ([llength $args]!=$numParams)} {
	    return -code error \
		"Wrong number of parameters: $numParams needed, [llength $args] given."
	} 
	$self MakePib [llength $args]
	set theIndicator 0
	for {set i 0; set ip 1} {$i<[llength $args]} \
	    {   incr i; incr ip; 
		incr theIndicator [::ffidl::info sizeof SQLINTEGER]
	    } {
	    set proposedType [lindex $options(-argtypes) $i]
	    set givenType [$self AskForParamType $ip]
	    set argvar [set arg [lindex $args $i]]
	    set io 1
	    switch -exact -- [string tolower [lindex $proposedType 0] ] {
		input {
		    set proposedType [lrange $proposedType 1 end]
		}
		inputoutput {
		    set io 2
		    set proposedType [lrange $proposedType 1 end]
		}
		output {
		    set io 4
		    set proposedType [lrange $proposedType 1 end]
		}
	    }
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
	    lassign $proposedType ptnum
	    if {$ptnum!=0} {
		set triedTypes [list [lrange [linsert $proposedType end 0 0 0] 0 2]]
		if {$ptnum == -9} {
		    lset proposedType 0 12
		    lappend triedTypes \
			[lrange [linsert $proposedType end 0 0 0] 0 2]
		} elseif {$ptnum == -10} {
		    lset proposedType 0 -1
		    lappend triedTypes \
			[lrange [linsert $proposedType end 0 0 0] 0 2]
		}
	    } else {
		set wlongArg [expr {$argLength>255}]
		set longArg [expr {[
		    string length [
			encoding convertto $options(-encoding) $arg]]>255}]
		set wLONGorNO [expr {$wlongArg?"LONG":""}]
		set LONGorNO [expr {$longArg?"LONG":""}]
		set triedTypes \
		    [list \
			[list $sqltypes(W${wLONGorNO}VARCHAR) 0 0 ] \
			[list $sqltypes(${LONGorNO}VARCHAR) 0 0 ] \
			]
	    }
	    foreach tryType $triedTypes {
		lassign $tryType ptype pscale pprec
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
		if {!$pprec} { set pprec $charlen }
		if {!$pprec} { set pprec 1 }
		set bound [${apins}::SQLBindParameter $hstmt $ip $io $c_type \
			$ptype $pprec $pscale [::ffidlx::buffer addr $buf] \
			[string length $bytes] \
			[expr {$theIndicator+$pib}]]
		if {$options(-null) ne {} && $arg eq $options(-null)} {
		    set strlen -1 ;# SQL_NULL_DATA
		}
		if {$options(-default) ne {} && $arg eq $options(-default)} {
		    set strlen -5 ;# SQL_DEFAULT_PARAM
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
	    uplevel 1 [list $self BindParams {*}$argList]
	    if {$options(-direct)} {
		set execrslt [${apins}::SQLExecDirect $hstmt [LPSTR $options(-sql)] -3]
	    } else {
		set execrslt [${apins}::SQLExecute $hstmt]
	    }
	} else {
	    set execrslt [$self SpecialExecute $specialFunction {*}$argList]
	}
	if {$pib} {
	    variable pvBufs
	    variable pvOutput
	    set theIndicator 0
	    foreach pvbuf $pvBufs pvoutput $pvOutput {
		if {[llength $pvoutput]} {
		    lassign $pvoutput varname level c_type bytesize
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
	    catch {foreach buf $pvBufs { ::ffidlx::buffer free $buf }}
	    set pvBufs {}
	}

	if {$execrslt == -1} { return -code error [$self Diagnose]}
	if {$execrslt == 1} { $self Diagnose }
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
	catch { ${apins}::SQLCloseCursor $hstmt } 
	return $result
    }
    method fetch {{arrayName {}} {colNames {}}} {
	set row [cfetch $hstmt $numColumns]
	if {$arrayName eq ""} {
	    return $row
	} else {
	    upvar 1 $arrayName rowArray
	    set i 0
	    foreach x $row {
		set cname [lindex $colNames $i]
		set rowArray($cname) $x
		incr i
	    }
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
		if {$cname eq ""} { lassign [$self DescribeColumn $i] cname }
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
	foreach uv $arrays { upvar 1 $uv ar_[incr i] }
	set rowcount 0
	while {[llength [set row [$self fetch]]]} {
	    set key [lindex $row 0]
	    set col 2
	    foreach other [lrange $row 1 end] {
		if {[llength $arrays==1]&&$numColumns>2} {
		    lassign [$self DescribeColumn $col] cname
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
	    if {[string is boolean $value]} { set value [expr {$value? 1 : 0}] }
	    lassign [dict get $::odbc::ATTRS $opt] optCode
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
	set avdict [lassign [dict get $::odbc::ATTRS $opt] optCode]
	set rc [${apins}::SQLGetStmtAttr/int $hstmt $optCode [LP SQLINTEGER value] \
	    0 [LP SQLINTEGER _]]
	if {[llength $avdict]} {
	    dict for {k v} $avdict { if {$value==$k} {set value $v; break} }
	}
	return $value
    }
    method GetData {nth} {
	set retval [getdata $hstmt $nth]
	return $retval
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
		set buf [::ffidlx::buffer create $binarg ]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [${apins}::SQLTables/raw $hstmt {*}$stargs]
	foreach buf $pbufs { ::ffidlx::buffer free $buf }
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
		set buf [::ffidlx::buffer create $binarg ]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [${apins}::SQLColumns/raw $hstmt {*}$stargs]
	foreach buf $pbufs { ::ffidlx::buffer free $buf }
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
		set buf [::ffidlx::buffer create $binarg ]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set scope [dict get {row 0 currow 0 transaction 1 session 2} \
	    [from args -scope row]]
	if {[from args -nullable 1]} {set nullable 1} {set nullable 0}
	set rslt [${apins}::SQLSpecialColumns/raw $hstmt 1 {*}$stargs $scope $nullable]
	foreach buf $pbufs { ::ffidlx::buffer free $buf }
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
		set buf [::ffidlx::buffer create $binarg ]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [${apins}::SQLPrimaryKeys/raw $hstmt {*}$stargs ]
	foreach buf $pbufs { ::ffidlx::buffer free $buf }
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
		set buf [::ffidlx::buffer create $binarg ]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set uidx [expr {[from args -unique 0]?0:1}]
	set rslt [${apins}::SQLStatistics/raw $hstmt {*}$stargs $uidx 0]
	foreach buf $pbufs { ::ffidlx::buffer free $buf }
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
		set buf [::ffidlx::buffer create $binarg ]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [${apins}::SQLProcedures/raw $hstmt {*}$stargs]
	foreach buf $pbufs { ::ffidlx::buffer free $buf }
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
		set buf [::ffidlx::buffer create $binarg ]
		lappend stargs [::ffidlx::buffer addr $buf] [string length $binarg]
		lappend pbufs $buf
	    } else {
		lappend stargs 0 0
	    }
	}
	set rslt [${apins}::SQLProcedureColumns/raw $hstmt {*}$stargs]
	foreach buf $pbufs { ::ffidlx::buffer free $buf }
	return $rslt
    }
    delegate method HDBC to dbConn
    destructor { catch { ${apins}::SQLFreeHandle 3 $hstmt } }
    typeconstructor {
	if {[catch {namespace path}]} {
	    namespace import ::odbc::* ::ffidlx::*
	} else {
	    namespace path [linsert [namespace path] end ::odbc ::ffidlx]
	}
    }
}

namespace eval ::odbc { namespace export database }

