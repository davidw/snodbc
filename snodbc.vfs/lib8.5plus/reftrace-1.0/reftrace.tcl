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
    MkRefLowLevel [list ::lindex $args] \
        [list ::lset $args] 0 lv
}

# Make a persistent reference to a [dict]'s element
proc ::reftrace::dict& {dictVarName args} {
    upvar 1 $dictVarName dv
    MkRefLowLevel [list {::dict get} $args] \
        [list {::dict set} $args] 0 dv
}

# Make a temporary reference to a [sub]list element
proc ::reftrace::lindex* {listVarName args} {
    upvar 1 $listVarName lv
    MkRefLowLevel [list ::lindex $args] \
        [list ::lset $args] 1 lv
}

# Make a temporary reference to a [dict]'s element
proc ::reftrace::dict* {dictVarName args} {
    upvar 1 $dictVarName dv
    MkRefLowLevel [list {::dict get} $args] \
        [list {::dict set} $args] 1 dv
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
    set linkedVar [{*}$cvpre $structureVar {*}$cvpost]
    trace add variable structureVar {write read unset} \
        [list ::reftrace::Tracker $linkedName $inConv $outConv $isTemporal]
    return $linkedName
}

proc ::reftrace::Tracker \
    {linkedName inConv outConv isTemporal name1 name2 op} {
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
        set $linkedName [{*}$cvpre $myvar {*}$cvpost]
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
	    {*}$cvpre myvar {*}$cvpost [set $linkedName]
        } else {
            # The link is destroyed, trace is not necessary
            trace remove variable myvar {write read unset} \
                [list ::reftrace::Tracker \
                    $linkedName $inConv $outConv $isTemporal]
        }
        # Temporal link is removed when the structure is first read.
        if {$isTemporal} {
            unset $linkedName
            trace remove variable myvar {write read unset} \
                [list ::reftrace::Tracker \
                    $linkedName $inConv $outConv $isTemporal]
        }
    }
}

# Reinventing a wheel: generate unique name variable name
proc ::reftrace::MkName {} {
    return ::reftrace::v::[incr ::reftrace::seq] 
}

