#!/bin/bash

function get_cycle {

   TODAY=`date -u +%Y%m%d`
   YESTERDAY=`date -u -d "$TODAY -1 day" +%Y%m%d`
   HH=`date -u +%H`

   case $1 in

      1)
         if [ "$HH" = "00" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "01" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "02" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "03" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "04" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "05" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "06" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "07" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "08" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "09" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "10" ]; then CYCLE=${YESTERDAY}_06; fi
         if [ "$HH" = "11" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "12" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "13" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "14" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "15" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "16" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "17" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "18" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "19" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "20" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "21" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "22" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "23" ]; then CYCLE=${TODAY}_06; fi
         ;;

      2)
         if [ "$HH" = "00" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "01" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "02" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "03" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "04" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "05" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "06" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "07" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "08" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "09" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "10" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "11" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "12" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "13" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "14" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "15" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "16" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "17" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "18" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "19" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "20" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "21" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "22" ]; then CYCLE=${TODAY}_18; fi
         if [ "$HH" = "23" ]; then CYCLE=${TODAY}_18; fi
         ;;

      4)
         if [ "$HH" = "00" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "01" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "02" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "03" ]; then CYCLE=${YESTERDAY}_18; fi
         if [ "$HH" = "04" ]; then CYCLE=${TODAY}_00; fi
         if [ "$HH" = "05" ]; then CYCLE=${TODAY}_00; fi
         if [ "$HH" = "06" ]; then CYCLE=${TODAY}_00; fi
         if [ "$HH" = "07" ]; then CYCLE=${TODAY}_00; fi
         if [ "$HH" = "08" ]; then CYCLE=${TODAY}_00; fi
         if [ "$HH" = "09" ]; then CYCLE=${TODAY}_00; fi
         if [ "$HH" = "10" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "11" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "12" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "13" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "14" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "15" ]; then CYCLE=${TODAY}_06; fi
         if [ "$HH" = "16" ]; then CYCLE=${TODAY}_12; fi
         if [ "$HH" = "17" ]; then CYCLE=${TODAY}_12; fi
         if [ "$HH" = "18" ]; then CYCLE=${TODAY}_12; fi
         if [ "$HH" = "19" ]; then CYCLE=${TODAY}_12; fi
         if [ "$HH" = "20" ]; then CYCLE=${TODAY}_12; fi
         if [ "$HH" = "21" ]; then CYCLE=${TODAY}_12; fi
         if [ "$HH" = "22" ]; then CYCLE=${TODAY}_18; fi
         if [ "$HH" = "23" ]; then CYCLE=${TODAY}_18; fi
         ;;

      *)
         echo "Set NUM_CYCLES_PER_DAY to 1, 2, or 4"
         exit 1
         ;;
   esac

   echo $CYCLE

}
