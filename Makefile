all: sqlora.so sqlora-longfield.so

clean:
	rm *.c *.so *.o

carray.o: carray.scm
	csc -c $< -o $@
enums.o: enums.scm
	csc -c $< -o $@

sqlora.so: sqlora.scm carray.o enums.o
	csc -s $^ -X lazy-ffi -X easyffi
	cp sqlora.so s.so 

sqlora-longfield.so: sqlora-longfield.scm
	csc -X easyffi -X lazy-ffi -k -s sqlora-longfield.scm -lsqlora8

test: test.scm 
	csc test.scm 

try: test
	./test

