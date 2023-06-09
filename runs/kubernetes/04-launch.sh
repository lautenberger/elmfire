#!/bin/bash

ARGS='{
"fireName": "nm-rfw-mar29",
"initializationType": "points_within_polygon",
"ignitionTime": "2023-03-29 21:00 UTC",
"ignitionLon": -106.3,
"ignitionLat": 34.2,
"centerLon": -106.3,
"centerLat": 34.2,
"westBuffer": 12,
"southBuffer": 12,
"eastBuffer": 24,
"northBuffer": 24,
"numEnsembleMembers": 100,
"addToActiveFires": "no",
"ignitionRadius": 300,
"runHours": 60,
"fuelSource": "landfire",
"fuelVersion": "2.2.0",
"scpInputDeck": "none",
"returnAfterQueue": "'yes'",
"runTemplate": "kubernetes"
}'

RAND=$RANDOM
POD=elmfire-$RAND
cp -f elmfire.yaml $POD.yaml
sed -i "s/name: elmfire/name: $POD/g" $POD.yaml
kubectl create -f $POD.yaml

FIRENAME=`echo $ARGS | jq .fireName | tr -d '"'`
TIMESTAMP=`echo $ARGS | jq .ignitionTime | tr -d '"'`
TIMESTAMP=`date -u -d "$TIMESTAMP" +%Y%m%d_%H%M00`

ISREADY=0
echo "Waiting for $POD to reach Running state"
while [ "$ISREADY" = "0" ]; do
   ISREADY=`kubectl get pods | grep $POD | grep Running | wc -l`
   sleep 0.1
done

FORECAST_DIR=/elmfire/elmfire/runs/forecasts

echo "Executing ELMFIRE"
kubectl exec -it $POD -- /bin/bash -c "cd $FORECAST_DIR && ./01-crs.sh '$ARGS'"

echo "Copying ELMFIRE outputs locally"
kubectl cp $POD:$FORECAST_DIR/runs/$FIRENAME/${FIRENAME}_$TIMESTAMP/${FIRENAME}-$TIMESTAMP.tar ./out/${FIRENAME}-$TIMESTAMP.tar

echo "Cleaning up"
kubectl delete pod $POD &

rm -f elmfire-$RAND.yaml

exit 0
