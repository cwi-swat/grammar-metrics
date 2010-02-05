
test: tables test-antlr test-ometa test-oslo test-rats

test-antlr: dummy
	./ptest.sh antlr

test-rats: dummy
	./ptest.sh rats

test-ometa: dummy
	./ptest.sh ometa

test-oslo: dummy
	./ptest.sh oslo


tables:
	sdf2table -p . -c -m ANTLR -o antlr.trm.tbl
	sdf2table -p . -c -m OMeta -o ometa.trm.tbl
	sdf2table -p . -c -m Oslo -o oslo.trm.tbl
	sdf2table -p . -c -m Rats -o rats.trm.tbl

clean:
	rm -f *.trm.tbl

dummy: