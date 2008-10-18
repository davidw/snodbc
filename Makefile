snodbc.kit: libgetdata.so snodbc.vfs/lib8.5plus/odbc-1.2/classes.tcl
	./tclkit-linux-x86 sdx.kit wrap snodbc.kit

libgetdata.so: getdata.c
	gcc -g -O3 -W -I/usr/include/tcl8.5 -shared -o libgetdata.so -ltclstub8.5 -lodbc getdata.c

clean:
	rm libgetdata.so