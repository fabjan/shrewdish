#! /bin/sh

testbin=_build/example

if [ ! -x $testbin ]; then
    echo "Test binary not found, run build.sh first"
    exit 1
fi

failures=0

greptest()
{
    expected="$1"; shift
    cmdline=$*

    # shellcheck disable=SC2086
    output=$($testbin $cmdline)
    if echo "$output" | grep -q -E "$expected"; then
        printf "."
    else
        echo
        echo "FAIL: $cmdline"
        echo "Expected: $expected"
        echo "Got: $output"
        failures=$((failures + 1))
    fi
}

echo "Running tests"
greptest "Connected to Redis server version" "hello"
greptest "ping: [0-9]+ms" "ping"
newkey=$(uuidgen)
greptest "Counter $newkey is now 1" "incr $newkey"
greptest "Counter $newkey is now 11" "incr $newkey 10"

if [ $failures -eq 0 ]; then
    echo
    echo "All tests passed"
else
    echo
    echo "Some tests failed"
    echo "(is the Redis server running?)"
    exit 1
fi
