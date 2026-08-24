source /home/nick/miniconda3/etc/profile.d/conda.sh
conda activate elmfire
./clean.sh
python setup.py
./elmfire_all.sh
python compare.py --mode 1
