#! /bin/sh

LANG=$1

DUMMY=.bla.trm~

set -e

for t in `echo cases/${LANG}/*.${LANG}`; do
    echo "Parsing $t..."
    cat $t > ${DUMMY}
    echo >> ${DUMMY}
    sglr -p ${LANG}.trm.tbl -i ${DUMMY}  | ambtracker
    if [ $? -ne 0 ]; then
	exit 1;
    fi
done