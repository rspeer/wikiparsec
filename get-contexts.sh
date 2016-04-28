#!/bin/sh
jq -cr 'select(.rel == "context") | .to.text' $1 | sort | uniq -c | sort -nrk 1
