#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=21
#SBATCH --mem-per-cpu=8g
#SBATCH --time=24:00:00
#SBATCH --account=zhukai0
#SBATCH --partition=standard
#SBATCH --mail-type=BEGIN,END

module add python/3.10.4
module add spark/3.2.1

X=`cal ${MON} ${YEA} | grep -v '[A-Za-z]' | wc -w`
D=`seq -w ${STA} ${X}`
for d in ${D}
do
	spark-submit --num-executors 20 --executor-memory 8g \
	/nfs/turbo/twitter-decahose/tools/decahose-filter/decahose_filter.py \
	-k /home/songyl/GitHub/pheno_tweet/script/keywords/${CAT}.txt \
	-i file:///scratch/zhukai_root/zhukai0/songyl/${YEA}/*/gardenhose.${YEA}-${MON}-${d}*.bz2 \
	-o /nfs/turbo/seas-zhukai/phenology/Twitter/query/${CAT}/Spark/${YEA}/${MON}/${d}
done
