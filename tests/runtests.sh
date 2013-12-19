#!/bin/sh

dir=$(dirname $0)
tsk=$dir/../dist/build/tsk/tsk

[ -d "$dir/res" ] || mkdir $dir/res

for f in $dir/test*.tsk ; do
    fname=$(basename $f)
    fdir=$(dirname $f)
    res=$fdir/res/$fname.txt
    echo Parsing: $f,  Res: $res
    "$tsk" "$f" 2>&1 > "$res"
done
