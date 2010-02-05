#! /bin/sh

LANG=$1

DUMMY=.bla.trm~

set -e

for t in `find cases/${LANG} -name "[a-zA-Z]*" -type f -depth 1`; do
    echo "Parsing $t..."
    cat $t > ${DUMMY}
    echo >> ${DUMMY}
    sglr -p ${LANG}.trm.tbl -i ${DUMMY}  | ambtracker
    if [ $? -ne 0 ]; then
	exit 1;
    fi
done