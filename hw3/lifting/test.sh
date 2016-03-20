#!/bin/bash

# use GNU version of tools on Mac OS X if possible
if [[ -n $(which gsed) ]]; then
    SED=gsed
else
    SED=sed
fi

# colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# paths
FL='../fl'
LOG_FILE='test.log'

passed_tests=()
failed_tests=()

###############################
# test subroutine
###############################

test () {
    if [ $# -gt 0 ]; then
        status=""
        echo -ne "Testing case $1:" | tee -a $LOG_FILE
        # run program without lifting
        $FL > "test.orig.stdout" <<EOF
load $1
run
quit
EOF
        # run program after lifting
        $FL > "test.lift.stdout" <<EOF
load $1
lift
run
quit
EOF
        # process output
        $SED -i -e '/FL version/d' -e '/bye/d' \
                -e 's/>> //g' -e 's/? //g' "test.orig.stdout"
        # process output
        $SED -i -e '/FL version/d' -e '/bye/d' \
                -e 's/>> //g' -e 's/? //g' "test.lift.stdout"
        # compare outputs
        diff=$(diff -u -b "test.orig.stdout" "test.lift.stdout")
        if [[ -z "$diff" ]]; then
            status="PASSED"
            echo -e "$GREEN PASSED $NC"
            echo -e " PASSED " >> $LOG_FILE
        else
            status="FAILED"
            echo -e "$RED FAILED $NC"
            echo -e " FAILED " >> $LOG_FILE
            echo -e "\t\tReference output" >> $LOG_FILE
            echo -e "\t\t----------------" >> $LOG_FILE
            cat "test.orig.stdout" >> $LOG_FILE
            echo >> $LOG_FILE
            echo -e "\t\tActual output" >> $LOG_FILE
            echo -e "\t\t-------------" >> $LOG_FILE
            cat "test.lift.stdout" >> $LOG_FILE
        fi
        # remove generated files
        if [[ -f "test.orig.stdout" ]]; then
            rm -f "test.orig.stdout"
        fi
        if [[ -f "test.lift.stdout" ]]; then
            rm -f "test.lift.stdout"
        fi

        echo | tee -a $LOG_FILE
        if [[ "$status" = "PASSED" ]]; then
            passed_tests=("${passed_tests[@]}" "$1")
        fi
        if [[ "$status" = "FAILED" ]]; then
            failed_tests=("${failed_tests[@]}" "$1")
        fi
    fi
}

###############################
# begin testing
###############################

rm -f $LOG_FILE

# collect test cases
echo "Begin testing..."

number_of_tests=0
test_progs=$(ls -1v $1*.txt)
for t in $test_progs; do
    if [[ -e "$t" ]]; then
        test $t
        number_of_tests=$(expr $number_of_tests + 1)
    fi
done

echo "--------------------------------------------" | tee -a $LOG_FILE
echo -e "\t${#passed_tests[@]} of $number_of_tests tests passed" | tee -a $LOG_FILE
if [ ${#failed_tests[@]} -gt 0 ]; then
    echo "--------------------------------------------" | tee -a $LOG_FILE
    echo "Failed tests:" | tee -a $LOG_FILE
    for t in ${failed_tests[@]}; do
        echo -e "\t$t" | tee -a $LOG_FILE
    done
    echo "--------------------------------------------" | tee -a $LOG_FILE
fi
