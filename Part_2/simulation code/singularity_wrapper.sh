#!/bin/bash

# /usr/local/bin/singularity shell /home/cejens/simulations/ubuntu_R.img <<EOT
/usr/local/bin/singularity shell /home/cejens/simulations/r-base_latest.sif <<EOT
# cd $(pwd)
cd /home/cejens/simulations-202311
export PATH=/home/cejens/bin:$PATH
export RUN_NUMBER=$1
export NUMBER_OF_RUNS=$2
export BATCH_NUMBER=$3
$4 ${@:5}
EOT

exit 0
