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
IMP='../imp'
LOG_FILE='test.log'

passed_tests=()
failed_tests=()

###############################
# test subroutine
###############################

test () {
    if [ $# -gt 1 ]; then
        status=""
        echo -ne "Testing case $1:" | tee -a $LOG_FILE
        stdin=$(cat "$1.in")
        # run program
        $IMP > "test.stdout" <<EOF
load $2
run
$stdin
quit
EOF
        # process output
        $SED -i -e '/IMP interactive interpreter/d' -e '/bye/d' \
                -e 's/>> //g' -e 's/? //g' "test.stdout"
        # compare outputs
        diff=$(diff -u -b "$1.out" "test.stdout")
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
            cat "$1.out" >> $LOG_FILE
            echo >> $LOG_FILE
            echo -e "\t\tActual output" >> $LOG_FILE
            echo -e "\t\t-------------" >> $LOG_FILE
            cat "test.stdout" >> $LOG_FILE
        fi
        # remove generated files
        if [[ -f "test.stdout" ]]; then
            rm -f "test.stdout"
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
test_outputs=$(ls -1v $1*.out)
for o in $test_outputs; do
    t=$(echo $o | $SED 's/\.out//')
    s=$(echo $o | $SED 's/\(\.[0-9]\+\)\?\.out/\.imp/')
    if [[ -e "$s" ]] && [[ -e "$t.in" ]] && [[ -e "$t.out" ]]; then
        test $t $s
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
