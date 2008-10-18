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
