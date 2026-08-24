rm -r smoke/

elmfire smoke.data

mkdir smoke
cp ./outputs/EMITIMES.txt ./smoke

source ~/miniconda3/etc/profile.d/conda.sh
conda activate elmfire
python3 ../smokeHelpers/elmfire2hysplit.py


cd smoke
dos2unix CONTROL SETUP.CFG ASCDATA.CFG EMITIMES.txt

~/hysplit/exec/hycs_std # single threaded. 

python3 ../../smokeHelpers/bin2nc.py