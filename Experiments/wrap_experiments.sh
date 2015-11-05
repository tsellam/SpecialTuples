#!/bin/bash

if [ "$1" = "download" ]
then
     echo DOWNLOADING FILES
    cd ../Data
    ./get_data.sh

    mkdir KEEP THROW
    machine=`hostname -s`
    case $machine in
        rocks015)
            mv musk.arff KEEP
        ;;
        rocks017)
            mv diabetic_data.arff KEEP
        ;;
        rocks019)
            mv insurance.arff bank.arff KEEP
        ;;
        rocks020)
           mv breast.arff letrec.arff KEEP
        ;;
        rocks021)
           mv adult.arff KEEP
        ;;
        rocks023)
           mv communities.arff KEEP
        ;;
        rocks027)
           mv magic.arff KEEP
        ;;
        rocks028)
           mv pendigits.arff KEEP
        ;;
    esac
    mv *.arff THROW
    mv KEEP/* .
    cd ../Experiments
fi


R -f TestRealData.R



machine=`hostname -s`
case $machine in
    rocks015)
        R -f TestSyntheticData.R --args group1
    ;;
    rocks017)
        R -f TestSyntheticData.R --args group1
    ;;
    rocks019)
        R -f TestSyntheticData.R --args group2
    ;;
    rocks020)
        R -f TestSyntheticData.R --args group2
    ;;
    rocks021)
        R -f TestSyntheticData.R --args group3
    ;;
    rocks023)
        R -f TestSyntheticData.R --args group3
    ;;
    rocks027)
        R -f TestSyntheticData.R --args group4
    ;;
    rocks028)
        R -f TestSyntheticData.R --args group4
    ;;
esac

tarname=FindView-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.csv *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~






machine=`hostname -s`
case $machine in
    rocks015)
        R -f TestSyntheticData.R --args group1
    ;;
    rocks017)
        R -f TestSyntheticData.R --args group1
    ;;
    rocks019)
        R -f TestSyntheticData.R --args group2
    ;;
    rocks020)
        R -f TestSyntheticData.R --args group2
    ;;
    rocks021)
        R -f TestSyntheticData.R --args group3
    ;;
    rocks023)
        R -f TestSyntheticData.R --args group3
    ;;
    rocks027)
        R -f TestSyntheticData.R --args group4
    ;;
    rocks028)
        R -f TestSyntheticData.R --args group4
    ;;
esac

tarname=FindView2-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.csv *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~
