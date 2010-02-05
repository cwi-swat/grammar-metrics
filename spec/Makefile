
test: tables test-antlr test-ometa test-oslo test-rats test-javacup test-javacc

test-antlr: dummy
	./ptest.sh antlr

test-rats: dummy
	./ptest.sh rats

test-ometa: dummy
	./ptest.sh ometa

test-oslo: dummy
	./ptest.sh oslo

test-javacup: dummy
	./ptest.sh javacup

test-javacc: dummy
	./ptest.sh javacc


tables:
	sdf2table -p . -c -m ANTLR -o antlr.trm.tbl
	sdf2table -p . -c -m OMeta -o ometa.trm.tbl
	sdf2table -p . -c -m Oslo -o oslo.trm.tbl
	sdf2table -p . -c -m Rats -o rats.trm.tbl
	sdf2table -p . -c -m JavaCup -o javacup.trm.tbl
	sdf2table -p . -c -m JavaCC -o javacc.trm.tbl

clean:
	rm -f *.trm.tbl

dummy: