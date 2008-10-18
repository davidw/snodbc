#!/bin/sh
#WISH you were here \
    exec wish "$0" "$@"
catch {
    package require starkit
    starkit::startup
}

package require Tk
variable hs
set hs(hover.fg) red
set hs(link.fg) darkgreen
set winsys [tk windowingsystem]
set scriptdir [file normalize [file dirname [info script]]]

option add *sectitle.anchor w
option add *linkbar*background white
option add *htxt.padX 8
option add *htxt.padY 8
option add *Menu.tearOff 0


proc launch {url} {
    if {$::tcl_platform(platform) eq "windows"} {
	eval exec [linsert [auto_execok start] end $url] &
    } else {
	set brw "firefox:mozilla -remote -openURL(\"%s\"):dillo:mozilla:xterm -e lynx:xterm -e links:sensible-browser"
	if {[info exists ::env(BROWSER)]} {
	    set brw $::env(BROWSER)
	}
	foreach try [split $brw :] {
	    if {[string first %s $try]==-1} {append try " %s"}
	    set cmd [string map [list %s $url %% %] $try]
	    if {[catch {set brfh [ open "|$cmd" r]}]} {continue}
	    fconfigure $brfh -blocking no
	    if {![catch {close $brfh}]} {
		return
	    }
	}
	goto file:sys/unixbrowser
    }
}

if {$::winsys eq "x11"} {
    option add *Scrollbar.elementBorderWidth 1
    option add *Scrollbar.borderWidth 1
    option add *Scrollbar.relief sunken
    option add *Scrollbar.highlightThickness 0
    option add *Scrollbar.width 14
    option add *Menu.activeBorderWidth 1
} else {
    option add *Button.width -13
}

proc makefonts {base {sizeincr 1}} {
    array set baseopts [font actual $base]
#    foreach namedfont [font names] {
#	if {[string match f.* $namedfont]} {
#	    font delete $namedfont
#	}
#    }
    set ofn [font names]
    incr baseopts(-size) [expr {$baseopts(-size)<0?-$sizeincr:$sizeincr}]
    set baseopts(-weight) normal
    foreach {fname foptoverride} {
	base {}
	BIG {size *= 2 weight = bold}
	big {size *= 4/3 weight = bold}
	small {size *= 3/4 }
	bold {weight = bold}
	italic {slant = italic}
	underline {underline = 1}
    } {
	array set newopts [array get baseopts]
	foreach {attr modfop exprv} $foptoverride {
	    set value $newopts(-$attr)
	    set newopts(-$attr) [
		switch -exact $modfop {
		    *= { expr "\$value*$exprv" }
		    =  { set exprv }
		}
	    ]
	}
	set cmd [expr {[lsearch $ofn f.$fname]!=-1?"configure":"create"}]

	eval [linsert [array get newopts] 0 font $cmd f.$fname ]
    }
}
proc linkbar {path vnp} {
    variable linkbar
    set linkbar(path) $path
    frame $path
    set col 0
    foreach {v name} $vnp {
	incr col
	grid [label $path.$v -text $name] -column $col -row 0
	bind $path.$v <Enter> "linkbar_hover %W 1"
	bind $path.$v <Leave> "linkbar_hover %W 0"
	bind $path.$v <Button-1> "linkbar_follow $v"
	incr col
	grid columnconfigure $path $col -minsize 10
	setlink $v ""
    }
    grid columnconfigure $path $col -weight 1
    return $path
}

proc setlink {codename url} {
    variable hs
    set path $::linkbar(path)
    set lab $path.$codename
    set ::linkbar($codename) $url
    if {$url ne ""} {
	$lab configure -cursor hand2 -font f.underline -fg $hs(link.fg)
    } else {
	$lab configure -cursor {} -font f.base -fg black
    }
}

proc linkbar_hover {lab on} {
    variable hs
    if {$on} {
	if {$::linkbar([winfo name $lab]) ne ""} {
	    $lab configure -fg $hs(hover.fg)
	}
    } else {
	if {$::linkbar([winfo name $lab]) ne ""} {
	    $lab configure -fg $hs(link.fg)
	} else {
	    $lab configure -fg black
	}
    }
}

proc linkbar_follow {cname} {
    set url $::linkbar($cname)
    if {$url eq ""} {return}
    goto $url
}

proc seticon {tlw} {
    if {$::winsys eq "win32"} {
	wm iconbitmap $tlw $::scriptdir/help.ico
    } else {
	wm iconphoto $tlw i.book-open
    }
}

proc searcher {w} {
    variable search
    set search(cs) 0
    set search(re) 0
    toplevel $w 
    wm title $w "Search Help for"
    wm withdraw $w
    wm transient $w . 
    seticon $w
    label $w.l_what  -text "Find what: " 
    entry $w.what -textvariable search(s) -width 30
    frame $w.bf

    grid [ button $w.ok -text Find -default active  \
	-command [list searcher.ok $w]] \
	-in $w.bf -sticky news -pady 1
    grid [ button $w.cancel -text Cancel -default normal \
	-command [list wm withdraw $w]] \
	-in $w.bf -sticky news -pady 1

    trace add variable search(s) write  "searcher.strchange $w ;#"
    checkbutton $w.cs -text "Match case" -variable search(cs) -anchor w
    checkbutton $w.re -text "Regular expression" -variable search(re) -anchor w
    grid rowconfigure $w.bf 2 -weight 1
    grid rowconfigure $w 0 -weight 1
    grid $w.l_what $w.what -padx 3 -pady 5
    grid $w.cs - -sticky news -padx 3
    grid $w.re - -sticky news -padx 3 
    grid $w.bf -row 0 -column 2 -rowspan 3 -sticky news -padx 10 -pady 3
    wm resizable $w 0 0
    bind $w <Key-Escape> [list $w.cancel invoke]
    bind $w <Key-Return> [list $w.ok invoke]
}

proc searcher.strchange {w} {
    variable search
    if {!$search(re)} {
	set bs [expr {$search(s) eq ""? "disabled":"normal"}]
    } else {
	set bs [expr {$search(s) eq ""||
		[catch {regexp -about -- $search(s)}]? "disabled":"normal"}]
    }
    $w.ok configure -state $bs
}

proc searcher.ok {w} {
    variable search
    wm withdraw $w
    goto script:[list search.generate $search(s) $search(cs) $search(re)]
}

proc search {} {
    if {![winfo exists .searcher]} {
	searcher .searcher
	searcher.strchange .searcher
	wm geometry .searcher +[expr {[winfo rootx .]+50}]+[expr {[winfo rooty .]+50}]
	update
    }
    wm deiconify .searcher
    focus .searcher.what
    .searcher.what selection range 0 end
    .searcher.what icursor end
}

proc regexpcs {cs args} {
    if {$cs} {
	uplevel [linsert $args 0 regexp ]
    } else {
	uplevel [linsert $args 0 regexp -nocase ]
    }
}
proc regsubcs {cs args} {
    if {$cs} {
	uplevel [linsert $args 0 regsub ]
    } else {
	uplevel [linsert $args 0 regsub -nocase ]
    }
}

proc search.generate {str cs re} {
    variable hs
    set a ""
    set reg [expr {$re? "$str" : "***=$str"}]
    set rescount 0
    variable sr
    set sr(links) [list]
    set sr(reg) $reg
    set sr(cs) $cs
    foreach file [glob -nocomplain -directory $hs(helpdir) *.wtx] {
	set fh [open $file r ]
	set title ""
	while {[gets $fh dline]!=-1} {
	    if {$title eq ""} {set title $dline}
	    if {[regexpcs $cs -- $str $dline]} {
		regsub {^\s*===*(.*)==\s*$} $dline {\1} dline
		regsub {^   *\* } $dline {} dline

		regsub -all {\(\([^\) ]+ ([^\)]*)\)\)} $dline {\1} dline
		regsub -all {\(\(([^) ]+)\)\)} $dline {\1} dline
		regsub -all {\*\*([^*]*)\*\*} $dline {\1} dline
		regsub -all {\!\!([^!]*)\!\!} $dline {\1} dline

		if {[string length $dline]>200} {
		    regexpcs $cs -indices -- $str $dline fm
		    set pos [lindex $fm 0]
		    if {$pos==-1} {continue}
		    set dline [string range $dline \
			    [expr {$pos-80}] [expr {$pos+80}]]
		    if {[set fs [string first " " $dline]] <60} {
			set dline ...[string range $dline $fs end]...
		    }
		}
		incr rescount

		regsubcs $cs -all $str $dline {**\0**} dline
		set dline [string trim $dline]

		set refname [file rootname [file tail $file]]
		set url code:search.go%20[llength $sr(links)]
		lappend sr(links) file:$refname
		append a "    * __**(($url $title))**__\n    $dline\n"
		break
	    }
	}
	close $fh
    }
    list "Search results ($rescount found)" \
	"=== Found '$str' in $rescount sections ==\n\n$a"
}

proc search.go {rno} {
    variable sr
    set url [lindex $sr(links) $rno]
    goto $url
    set nocase [expr {$sr(cs)?"-forwards":"-nocase"}]
    set idx [.htxt search $nocase -regexp -- $sr(reg) 0.0]
    if {$idx ne ""} {
	.htxt see $idx
    }
}

proc setup {} {
    variable hs
    set hs(helpdir) [file join [file normalize [file dirname [info script]]] doc]
    set hs(here) ""
    set hs(history) [list]
    set hs(histindex) end
    if {[tk windowingsystem] eq "win32"} {
	label .gui -font {Arial 8}
    } else {
	label .gui 
    }
    makefonts [.gui cget -font] $::fontsize
    . configure -bd 2 -relief sunken
    wm geometry . 750x550
    wm protocol . WM_DELETE_WINDOW {after 350 exit}
    bind . <Key-Escape> exit
    wm title . "Sw4me WikiHelp Viewer"
    image create photo i.book-open -file $::scriptdir/doc/bo.gif
    seticon .
    label .sectitle -textvariable hs(title) -bg white -fg black -font f.BIG
    set hs(title) Contents
    linkbar .linkbar  {back Back forward Forward contents Contents index Index search Search}
    frame .hsep -height 1 -bg black -bd 0 
    frame .htframe  -bd 0 -relief sunken
    text .htxt -bd 0 -bg white -fg black -yscrollcommand {.hvsb set} -state disabled -highlightthickness 0 -cursor {} -font f.base
    wackotags .htxt
    scrollbar .hvsb  -command {.htxt yview}
    grid .htxt .hvsb -in .htframe -sticky news
    grid rowconfigure .htframe 0 -weight 1
    grid columnconfigure .htframe 0 -weight 1
    grid .sectitle  -sticky news
    grid .linkbar  -sticky news
    grid .hsep  -sticky news
    grid .htframe -sticky news
    grid rowconfigure . 3 -weight 1
    grid columnconfigure . 0 -weight 1
}

image create bitmap i.circle -data {
#define circle_width 5
#define circle_height 5
static unsigned char circle_bits[] = {
   0x0e, 0x1f, 0x1f, 0x1f, 0x0e};
}

proc wacko_aline {twidget line {stylemod {}} {pretags {}}} {
    #First we should find next special markup sequence
    while {$line ne ""} {
	set match [list [string length $line] [string length $line]]
	set sc [regexp -indices {//|\*\*|\(\(|\[\[|\+\+|\-\-|\!\!|\{\{|__} $line match]
	foreach {rb re} $match {break}
	set closer "."
	set markup ""
	if {$sc} {
	    set markup [string range $line $rb $re]
	    switch -exact -- $markup {
		"((" {set closer "))"}
		"\{\{" {set closer "\}\}"}
		default {set closer $markup}
	    }
	}
	# Normal text
	set ntx_end [expr {$sc?$rb-1:"end"}]
	set ntx [string range $line 0 $ntx_end]
	# Specially-formatted part
	set spec_end [string first $closer $line [expr {$re+1}]]
	if {$spec_end==-1} {
	    # fake markup, we should append it and call self
	    append ntx $markup
	    set line [string range $line [expr {$re+1}] end]
	    $twidget insert end  $ntx $pretags
	}  else {
	    # real markup, we should handle it
	    set spec [string range $line [expr {$re+1}] [expr {$spec_end-1}]]
	    set line [string range $line \
		    [expr {$spec_end+[string length $closer]}] end]
	    $twidget insert end  $ntx $pretags
	    wacko_special $twidget $markup $spec $pretags
	}
	# Nonethically
    }
}
proc wacko_init {} {
    variable wacko
    if {![info exists wacko]} {
	set wacko(linktag) 0
    }
}

proc wacko_getimage {url} {
    variable wacko
    variable hs
    if {![info exists wacko(img,$url)]} {
	set wacko(img,$url) [
		image create photo -file [
		    file join $hs(helpdir) $url]]
    }  else {
	set wacko(img,$url)
    }
}

proc wacko_special {twidget markup line pretags} {
    variable wacko
    variable hs
    array set mtagmap {
	// italic
	** bold
	++ small
	-- overstrike
	__ underline
	!! note
    }
    if {[info exists mtagmap($markup)]} {
	lappend pretags $mtagmap($markup)
	wacko_aline $twidget $line {} $pretags
	return
    } else {
	# some very special markup, such as link or whatever
	if {$markup eq "(("||$markup eq "\[\["} {
	    # link. has a space?
	    set spi [string first " " $line]
	    if {$spi ==-1} {
		set text ""; 
		set url $line
	    } else {
		set url [string range $line 0 [expr {$spi-1}]]
		set text [string range $line [expr {$spi+1}] end]
	    }
	    set suffix [string tolower [file extension $url]]
	    foreach {lineno chno} [split [$twidget index end] .] break
	    set linktag link$wacko(linktag)_[expr {$lineno % 2}]
	    set wacko(linktag) [expr {!$wacko(linktag)}]
	    if {$suffix eq ".gif"} {
		# paste image
		if {[string match file:* $url]} {
		    set url [string range $url 5 end]
		}
		set img [wacko_getimage $url]
		if {$text ne ""} {
		    lappend pretags link $linktag go:$text
		}
		set eiid [$twidget image create end \
			-image $img -padx 1 -pady 1]
		foreach tag $pretags {
		    $twidget tag add $tag $eiid
		}
	    } else {
		# paste link
		if {$text eq ""} {set text $url}
		lappend pretags link $linktag go:$url
		$twidget insert end $text $pretags
	    }
	}
    }
}

proc wacko_action {twidget wtxt action} {
    variable wacko
    switch -exact [lindex $action 0] {
	listbullet {
	    if {[lindex $action 1] eq "reset"} {
		array unset wacko $twidget,bullet*
	    }
	    foreach {level image} [lrange $action 1 end] {
		set wacko($twidget,bullet$level) [wacko_getimage $image]
	    }
	}
	script {
	    showwacko $twidget [eval [lrange $action 1 end]]
	}
	include {
	    variable hs
	    set file [lindex $action 1]
	    regsub {^file:} $file {} file
	    set fc {}
	    catch {
		set fh [open [file join $hs(helpdir) $file.wtx] r]
		fconfigure $fh -encoding utf-8
		gets $fh
		set fc [read -nonewline $fh]
		close $fh
	    } msg
	    showwacko $twidget $fc
	}
	ifrs {
	    if {[info exists ::.....]} {
		wacko_action $twidget $wtxt [lindex $action 1]
	    } else {
		wacko_action $twidget $wtxt [lindex $action 2]
	    }
	}
    }
}
proc showwacko {twidget wtxt {stylemod {}}} {
    variable wacko
    array unset wacko $twidget,* 
    set istep 20
    set lineno 0
    foreach line [split $wtxt "\n"] {
	incr lineno
	set line [string map [list "(c)" "\u00A9"] $line]
	# headers
	if {[string match ==*== $line]} {
	    regexp {^==(=*)(.*)=*==} $line / _ line
	    set hlevel [string length $_]
	    $twidget insert end "[string trim $line]\n" \
		[list h$hlevel $stylemod.h$hlevel all $stylemod.all]
	} elseif {[string match "\{\{*\}\}" [set st [string trim $line]]]} { 
	    # only a special
	    wacko_action $twidget $wtxt [string range $st 2 end-2]
	} else { 
	    # normal string. 
	    # let's find out what indentation we need
	    regexp {^ *} $line _
	    set iindent [expr {([string length $_] / 2)}]
	    set indent $iindent
	    set startpos [$twidget index end-1c]
	    #puts "at $startpos: $line"
	    set line [string trim $line]
	    # 
	    # if there's any bullet (mark), we should decrease initial indent by
	    # one unit and insert a bullet image (and tab) before the line
	    if {[regexp {^\* (\S.*)$} $line -> rest ]} {
		incr iindent -1
		if {[info exists wacko($twidget,bullet$iindent)]} {
		    set bullet $wacko($twidget,bullet$iindent)
		} else {
		    set bullet i.circle
		}
		set bimg [$twidget image create end -image $bullet ]
		$twidget insert end "\t"
		set line [string trim $rest]
		if {[info exists wacko($twidget,listlevel)]} {
		    set oldll $wacko($twidget,listlevel)
		    if {$oldll<$iindent} {
			if {[info exists wacko($twidget,bullet$oldll.open)]} {
			    $twidget image configure \
				$wacko($twidget,listbullet) \
				-image $wacko($twidget,bullet$oldll.open)
			}
		    }
		}
		set wacko($twidget,listlevel) $iindent
		set wacko($twidget,listbullet) $bimg
	    } else {
		unset -nocomplain wacko($twidget,listlevel) \
		    wacko($twidget,listbullet)
	    }
	    wacko_aline $twidget $line $stylemod
	    $twidget insert end "\n"
	    $twidget tag add indent$indent $startpos end-1c
	    $twidget tag add iindent$iindent $startpos end-1c
	}
    }
}

proc wackotags {twidget} {
    variable hs
    set istep 20
    $twidget tag configure all -spacing3 4p -rmargin 10 
    $twidget configure -wrap word -tabs [set ts {20 40 60 80 100 120 140 160 180}]
    foreach ind {0 1 2 3 4 5 6} {
	set off [expr {$ind*$istep}]
	$twidget tag configure indent$ind -lmargin2 $off
	$twidget tag configure iindent$ind -lmargin1 $off -tabs [lrange $ts $ind end]
    }
    $twidget tag configure h0 -font f.BIG -underline 1
    $twidget tag configure h1 -font f.big -underline 1
    $twidget tag configure h2 -font f.bold -underline 1
    $twidget tag configure italic -font f.italic
    $twidget tag configure bold -font f.bold
    $twidget tag configure link -underline 1 -foreground $hs(link.fg)
    $twidget tag configure hover -underline 1 -foreground $hs(hover.fg)
    $twidget tag configure underline -underline 1 
    $twidget tag configure note -foreground red

    foreach linktag {link0_0 link1_0 link0_1 link1_1} {
	$twidget tag bind $linktag <Any-Enter> [list wacko_hover $linktag $twidget 1]
	$twidget tag bind $linktag <Any-Leave> [list wacko_hover $linktag $twidget 0]
	$twidget tag bind $linktag <Button-1> [list wacko_follow $linktag $twidget]
    }
    $twidget tag raise sel
    $twidget tag configure sel -borderwidth 0 -relief flat
}

proc wacko_keyboardlinknav {twidget delta} {
    # Key semantics:
    # <nextlink> selects next linking range,
    #   starting from the window's top when there's no current selection
    # <prevlink> selects prev linking range,
    #   starting from the window's bottom when there's no current selection
}

proc wacko_hover {linktag twidget on} {
    variable wacko
    $twidget configure -cursor [expr {$on?"hand2":""}]
    if {$on} {
	foreach {hs he} [$twidget tag prevrange $linktag current+1c] {break}
	$twidget tag add hover $hs $he
	set wacko($twidget,hoverstart) $hs
	set wacko($twidget,hoverend) $he
	foreach cand [$twidget tag names current] {
	    if {[string match go:* $cand]} {
		set wacko($twidget,hoverurl) [string range $cand 3 end]
		break
	    }
	}
    } else {
	catch {
	    $twidget tag remove hover \
		$wacko($twidget,hoverstart) $wacko($twidget,hoverend)
	    unset -nocomplain wacko($twidget,hoverstart) \
		wacko($twidget,hoverend)
	}
    }
}

proc wacko_follow {linktag twidget} {
    variable wacko
    # wacko_hover $linktag $twidget 0
    goto $wacko($twidget,hoverurl)
}

proc goto {url {history 1}} {
    variable hs
    set url [string map [list %20  " "] $url]
    if {[string match code:* $url]} {
	eval [string range $url 5 end]
	return
    }
    if {[string match http:* $url]||[string match mailto:* $url]} {
	launch $url
	return
    }
    if {[string is alnum $url]} {
	set url [string tolower file:$url]
    }
    set fc ""
    if {$url eq $hs(here)} {return}
    if {[string match file:* $url]} {
	set filename [file join $hs(helpdir) [string range $url 5 end]]
	set fh [open $filename.wtx r ]
	fconfigure $fh -encoding utf-8
	gets $fh title
	set hs(title) $title
	set fc [read -nonewline $fh]
	close $fh
    } elseif {[string match script:* $url]} {
	foreach {hs(title) fc} [eval [string range $url 7 end]] {break}
    }
    if {$fc ne ""} {
	.htxt configure -state normal
	.htxt delete 0.0 end+1c
	showwacko .htxt $fc
	.htxt configure -state disabled
	set hs(here) $url
	if {$history} {
	    set hs(history) [lrange $hs(history) 0 $hs(histindex)]
	    set hs(histindex) [llength $hs(history)]
	    lappend hs(history) $hs(here)
	}
	sethistlinks
    }
}

proc sethistlinks {} {
    variable hs
    set findex [expr {$hs(histindex)+1}]
    set bindex [expr {$hs(histindex)-1}]
    if {[set fwd [lindex $hs(history) $findex]] ne ""} {
	set fwd code:[list gohistory 1] 
    }
    if {[set bwd [lindex $hs(history) $bindex]] ne ""} {
	set bwd code:[list gohistory -1] 
    }
    setlink back $bwd 
    setlink forward $fwd 
}

proc gohistory {diff} {
    variable hs
    goto [lindex $hs(history) [incr hs(histindex) $diff]] 0
}
proc hide {} {
    catch {grab set .}
    after 50 {catch {grab release .};wm withdraw .}
}
proc show {} {
    wm deiconify .
    raise .
    update
    focus -force .htxt
}

proc textcmenu {w {x {}} {y {}}} {
    variable cmenu
    variable linkbar
    set m $w._cm
    if {![winfo exists $m]} {
	menu $m
	$m add command -label Back -underline 0 \
	    -command {linkbar_follow back} -accel "<--"
	$m add command -label Forward -underline 0 \
	    -command {linkbar_follow forward} 
	$m add separator
	$m add command -label Copy -underline 3 -accel "Ctrl+C" \
	    -command [list event generate $w <<Copy>>]
	$m add command -label "Select all" -underline 7 -accel "Ctrl+/" \
	    -command [list $w tag add sel 0.0 end]
	$m add separator
	$m add command -label "Contents" -underline 0 \
	    -command {linkbar_follow contents} -accel C
	$m add command -label "Page index" -underline 5 \
	    -command {linkbar_follow index} -accel X
	$m add command -label "Search..." -accel F3 \
	    -command {linkbar_follow search}
	$m add separator
	$m add command -label "Increase font size" -accel "Ctrl+\]" \
	    -command {fontresize 1}
	$m add command -label "Decrease font size" -accel "Ctrl+\[" \
	    -command {fontresize -1}

    }
    set may_selectall normal
    set nosel [catch {
	if {[$w index sel.first] eq [$w index 0.0] &&
	    [$w index sel.last] eq [$w index end]} {
		set may_selectall disabled
	    }
	}]
    
    $m entryconfigure "Select all" -state $may_selectall
    $m entryconfigure Back \
	-state [expr {$linkbar(back) eq ""?"disabled":"normal"}]
    $m entryconfigure Forward \
	-state [expr {$linkbar(forward) eq ""?"disabled":"normal"}]
    $m entryconfigure Copy \
	-state [expr {$nosel?"disabled":"normal"}]

    if {$x eq ""} {
        set x [expr {[winfo rootx $w]+[winfo width $w]/2}]
        set y [expr {[winfo rooty $w]+[winfo height $w]/2}]
    }
    if {$::winsys eq "win32"} {
	$m post $x $y
    } else {
	# $m post $x $y
	tk_popup $m $x $y
    }
}

variable fontsize [expr {$::winsys eq "win32"? 2: 1}]
proc fontresize {delta} {
    variable fontsize
    incr fontsize $delta
    set fontsize [expr {$fontsize<-4?-4:($fontsize>10?10:$fontsize)}]
    makefonts [.gui cget -font] $fontsize
}


proc genindex {} {
    variable hs
    if {![info exists hs(index)]} {
	set a "=== Alphabetical help index ==\n"
	foreach file [glob -nocomplain -directory $hs(helpdir) *.wtx] {
	    set fh [open $file r ]
	    gets $fh title 
	    close $fh
	    set refname [file rootname [file tail $file]]
	    set ufromh($title) $refname
	}
	set lt ""
	foreach title [lsort -dictionary [array names ufromh]] {
	    if {![string equal -nocase -length 1 $title $lt]} {
		append a "\n"
		set lt $title
	    }
	    append a "  * (($ufromh($title) $title))\n"
	}
	set hs(index) [list "Index" $a]
    }
    return $hs(index)
}

proc buildinfo {} {
    set bi {}
    catch {
	package require starkit
	set fh [open [file join $::starkit::topdir tclkit.inf] r]
	fconfigure $fh -encoding utf-8
	array set pi [read $fh]
	close $fh
	set bi "  $pi(FileDescription) **!!$pi(FileVersion)!!**"
    } msg
    return $bi
}

wacko_init
setup
setlink contents contents
setlink search code:search
setlink index script:genindex
if {![llength $::argv]} {
    goto contents
} else {
    goto [lindex $::argv 0]
}
bind .htxt <ButtonRelease-3> "textcmenu %W %X %Y"
catch {bind .htxt <Key-Menu> "textcmenu %W"}
catch {bind .htxt <Key-App> "textcmenu %W"}
catch {bind .htxt <Shift-Key-F10> "textcmenu %W"}

bind . <Control-Key-bracketright> {fontresize 1}
bind . <Control-Key-bracketleft> {fontresize -1}
bind . <Key-F3> {linkbar_follow search}
bind . <Key-BackSpace> {linkbar_follow back}
bind . <Alt-Key-Left> {linkbar_follow back}
bind . <Alt-Key-Right> {linkbar_follow forward}
bind . <Key-h> {linkbar_follow back}
bind . <Key-l> {linkbar_follow forward}
bind . <Key-c> {linkbar_follow contents}
bind . <Key-x> {linkbar_follow index}
bind . <Key-Tab> {focus .htxt}
bind .htxt <Key-Tab> {break}
bind .htxt <Key-Up> {.htxt yview scroll -4 units}
bind .htxt <Key-Down> {.htxt yview scroll 4 units}
bind .htxt <Key-space> {.htxt yview scroll 4 units}
bind .htxt <Shift-Key-space> {.htxt yview scroll -4 units}

focus .htxt
proc bgerror {msg} {
    puts $::errorInfo
}

if {$::tcl_platform(platform) eq "windows"} {
    package require dde
    dde servername Sw4meHelp.[string tolower [file tail [file dirname [info script]]]]
}
