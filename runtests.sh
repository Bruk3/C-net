#!/bin/sh

# Regression testing script for C-net.
#

# Set time limit for all operations
ulimit -t 30

globallog=runtests.log
rm -f $globallog
error=0
globalerror=0

intergrationtester="./ccnet -b"
integrationtests="tests/integration/test-*.cnet tests/integration/fail-*.cnet"

parsertester="./ccnet -a"
parsertests="tests/parser/test-*.cnet tests/parser/fail-*.cnet"

scannertester="./ccnet -t"
scannertests="tests/scanner/test-*.cnet tests/scanner/fail-*.cnet"

semantictester="./ccnet -s"
semantictests="tests/semant/test-*.cnet tests/semant/fail-*.cnet"

keep=0

Usage() {
    echo "Usage: runtests.sh [options] [.cnet files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

Check() {
    error=0
    basename=$(basename -s ".cnet" $1)
    reffile=`echo $1 | sed 's/.cnet$//'`
    basedir="$(dirname $1)/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.out" &&


    Run $2 $1 ">" "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Run "$MICROC" "$1" ">" "${basename}.ll" &&
    # Run "$CC" "-o" "${basename}.exe" "${basename}.s" "printbig.o" &&
    # Run "./${basename}.exe" > "${basename}.out" &&
    # Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.cnet//'`
    reffile=`echo $1 | sed 's/.cnet$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    # RunFail "$MICROC" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
    RunFail $2 $1 "2>" "${basename}.err" ">>" $globallog &&
    Compare ${basename}.err ${reffile}.err ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
        if [ $keep -eq 0 ] ; then
            rm -f $generatedfiles
        fi
        echo "OK"
        echo "###### SUCCESS" 1>&2
    else
        echo "###### FAILED" 1>&2
        globalerror=$error
    fi
}

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`


for file in $scannertests
do
    case $file in
	*test-*)

	    Check $file "$scannertester" 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file "$scannertester" 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

for file in $parsertests
do
    case $file in
	*test-*)
	    Check $file "$parsertester" 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file $parsertester 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

for file in $semantictests
do
    case $file in
    *test-*)
        Check $file "$semantictester" 2>> $globallog
        ;;
    *fail-*)
        CheckFail $file "$semantictester" 2>> $globallog
        ;;
    *)
        echo "unknown file type $file"
        globalerror=1
        ;;
    esac
done

# for file in $integrationtests
# do
#     case $file in
# 	*test-*)
# 	    './ccnet' $file
# 	    Check $file ${file%.cnet}.exe 2>> $globallog
# 	    rm ${file%.cnet}.exe
# 	    ;;
# 	*fail-*)
# 	    # './ccnet' $file
# 	    # CheckFail $file ${file%.cnet}.exe 2>> $globallog
# 	    ;;
# 	*)
# 	    echo "unknown file type $file"
# 	    globalerror=1
# 	    ;;
#     esac
# done

exit $globalerror
