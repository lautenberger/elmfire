#!/bin/bash

docker login
docker tag elmfire clauten/elmfire
docker push clauten/elmfire

exit 0
