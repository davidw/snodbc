# load package
source snodbc.kit
package require snodbc

# open connection to database
# DSN: name of DSN entry for postgres database connection
# MAKE SURE YOU CONNECT FIRST:

proc drop_test_db {conn} {
    # drop table if it exists
    catch {c drop}
    $conn statement c "DROP TABLE public.testdata"
    c execute
    c drop

    puts "Test database removed"
}

proc make_test_db {conn} {
    catch {c drop}
    $conn statement c "CREATE TABLE public.testdata (
       customer_id bigint,
       first_name character varying (30),
       last_name character varying (30),
       credit_limit bigint,
       email_address character varying (50),
       gender character varying (1),
       income_level character varying (50),
       marital_status character varying (20),
       score1 bigint,
       score2 bigint,
       score3 bigint,
       score4 bigint,
       score5 bigint,
       score6 bigint,
       score7 bigint,
       score8 bigint,
       score9 bigint,
       score10 bigint
       ) WITHOUT OIDS"
    c execute
    c drop

    puts "Test database created"
}


# get table size
proc get_table_size {conn} {
    catch {c drop}
    $conn statement c {select count(*) from public.testdata}
    c execute
    set size [c fetch]
    c drop

    return $size
}


# populate the table with test data
proc populate {conn m n} {

    for {set i $m} {$i <= $n} {incr i} {
	set id $i
	set fname "john_$i"
	set lname "wayne_$i"
	set limit [expr $i * 100]
	set email "john_$i@yahoo.com"
	if {$i % 2 == 0} {set gender "m"} else {set gender "f"}
	set income "[expr $i * 1000] - [expr 5 * $i * 1000]"
	if {$i % 2 == 0} {set marital "single"} else {set marital "married"}
	set s1 [expr $i + 0]
	set s2 [expr $i + 1]
	set s3 [expr $i + 2]
	set s4 [expr $i + 3]
	set s5 [expr $i + 4]
	set s6 [expr $i + 5]
	set s7 [expr $i + 6]
	set s8 [expr $i + 7]
	set s9 [expr $i + 8]
	set s10 [expr $i + 9]

	set sql "INSERT INTO public.testdata (customer_id, first_name,
               last_name, credit_limit, email_address, gender,
               income_level, marital_status, score1, score2, score3, score4,
               score5,score6,score7,score8,score9,score10)
               VALUES ($id, '$fname', '$lname', $limit, '$email', '$gender', '$income', '$marital', $s1, $s2,$s3,$s4,$s5,$s6,$s7,$s8,$s9,$s10) "

	$conn statement c $sql
	c execute
	c drop
	# puts "RECORD $i: $sql"
    }
    puts "Test Data Created: [expr $n - $m + 1] records inserted"
}


# fetch n records
# one way to see performance is to increase the n
# another way is to add extra columns to the query or the table definition
# for example: score1 as s1,score2 as s2,score3 as s3,score4 as s4 and so on
proc fetch_n {conn n} {

    set start [clock seconds]
    catch {c drop}
#    set where "where customer_id = 37677"
    set where ""
    $conn statement c "select customer_id, first_name,
               last_name, credit_limit, email_address, gender,
               income_level, marital_status, score1, score2, score3, score4,
               score5,score6,score7,score8,score9,score10
               from public.testdata $where limit $n"
    
    puts [time {c execute}]
    for {set i 1} {$i <= $n} {incr i} {
#	puts $i
#	puts stderr [time {
	set record [c fetch]
#	c fetch
#	puts "$record"
#	}]
    }
    c drop

    set end [clock seconds]
    puts "Time: [expr $end - $start] seconds"
}

# tk_messageBox -message [odbc::database drivers]

if { 0 } {
    load ./libgetdata[info sharedlibextension]
}

console show

set conn [odbc::database pgsqlconn {PostgreSQL35W} "davidw" "hola"]

#make_test_db $conn
#populate $conn 1 1000000

fetch_n $conn 50000

#while { 1 } {
#    after 1000
#    puts "end loop"
#}