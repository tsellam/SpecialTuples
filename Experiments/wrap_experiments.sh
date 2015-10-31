#!/bin/bash

if [ "$1" = "download" ]
then
     echo DOWNLOADING FILES
    cd ../data/experiments
    ./get_data.sh

    mkdir KEEP THROW
    machine=`hostname -s`
    case $machine in
        rocks015)
            mv musk.arff adult.arff communities.arff KEEP
        ;;
        rocks017)
            mv pendigits.arff magic.arff diabetic_data.arff KEEP
        ;;
        rocks019)
            mv insurance.arff bank.arff insurance.arff KEEP
        ;;
        rocks020)
           mv breast.arff letrec.arff cover_type.arff  KEEP
        ;;
    esac
    mv *.arff THROW
    mv KEEP/* .
    cd ../../experiments
fi

R -f TestRealData.R

tarname=FindView-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.csv *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~
