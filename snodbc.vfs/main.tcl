package require starkit
if {![package vsatisfies [package provide Tcl] 8.4]} {
   starkit::panic "TCL versions below 8.4 are unsupported."
}

if {[starkit::startup]eq"sourced"} {
    if {[package vsatisfies [package provide Tcl] 8.5 ]} {
	starkit::autoextend [file join $::starkit::topdir lib8.5plus]
    } else {
	starkit::autoextend [file join $::starkit::topdir lib8.4]
    }
    starkit::autoextend [file join $::starkit::topdir lib [starkit::platform] ]
    starkit::autoextend [file join $::starkit::topdir lib]
} else {
    source [file join $::starkit::topdir help help.tcl]
}
