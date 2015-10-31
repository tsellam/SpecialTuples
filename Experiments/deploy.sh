# rocks 017 018  027 028

################
# SETUP SCRIPT #
################
ssh cwi
ssh rocks018
mkdir ~/R ~/R/library ~/R/sources

cd ~/R
wget --quiet http://cran.xl-mirror.nl/src/base/R-3/R-3.1.2.tar.gz
tar xzvf R-3.1.2.tar.gz
cd R-3.1.2
./configure  --with-x=no
make
echo 'export PATH=/local/sellam/R/R-3.1.2/bin:$PATH' >>  $HOME/.bashrc
source ~/.bashrc

echo 'R_LIBS_USER="~/R/library"' >  $HOME/.Renviron
echo 'install.packages("dplyr",lib="~/R/library", repos="'http://cran.us.r-project.org'")' | R --no-save
echo 'install.packages("tidyr",lib="~/R/library", repos="'http://cran.us.r-project.org'")' | R --no-save
echo 'install.packages("e1071",lib="~/R/library", repos="'http://cran.us.r-project.org'")' | R --no-save
echo 'install.packages("foreign",lib="~/R/library", repos="'http://cran.us.r-project.org'")' | R --no-save
echo 'install.packages("igraph",lib="~/R/library", repos="'http://cran.us.r-project.org'")' | R --no-save
echo 'install.packages("R.utils",lib="~/R/library", repos="'http://cran.us.r-project.org'")' | R --no-save
echo 'install.packages("class",lib="~/R/library", repos="'http://cran.us.r-project.org'")' | R --no-save

#################
# CHECKOUT CODE #
#################
rm -rf  /scratch/sellam
mkdir /scratch/sellam
cd /scratch/sellam
git clone https://tsellam@bitbucket.org/tsellam/turbogroups.git

cd turbogroups/code/Rlib
R CMD SHLIB info_theory.c
cd ../../experiments

########################
# RUN WITH DOWNLOADING #
########################
cd /scratch/sellam/turbogroups/experiments
 ./wrap_view_experiments.sh download



###################
# RUN EXPERIMENTS #
###################
# rocks 017 018  027 028

ssh cwi
ssh rocks028
killall R

cd /scratch/sellam/turbogroups
git checkout -- .
git pull -f

cd code/Rlib
R CMD SHLIB info_theory.c

cd /scratch/sellam/turbogroups/experiments
rm nohup.out
nohup ./wrap_view_experiments.sh &
tail -f nohup.out


################
# SEND RESULTS #
################
cd /scratch/sellam/turbogroups/experiments
tarname=FindView-`hostname -s`-` date +'%B%d'`.tar.gz
tar -czvf $tarname nohup.out *.out *.log
scp $tarname sellam@warsaw.ins.cwi.nl:~

########################
# INTERRUPT AND RELOAD #
########################
killall -usellam

ssh rocks028
killall R
cd /scratch/sellam/turbogroups
git checkout -- .
git pull -f

cd /scratch/sellam/turbogroups/experiments
rm nohup.out
nohup ./wrap_view_experiments.sh &
tail -f nohup.out




######################
# Rebuilds databases #
######################
ssh cwi
ssh rocks028
killall R

cd /scratch/sellam/turbogroups
git checkout -- .
git pull -f

cd data/experiments
./get_data.sh


