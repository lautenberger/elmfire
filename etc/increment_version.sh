#!/bin/bash

grep -rl 2023.1221 ../ | xargs sed -i 's/2023.1221/2023.1231/g'
