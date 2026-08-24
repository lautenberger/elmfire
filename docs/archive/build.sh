#!/bin/bash

cd $ELMFIRE_BASE_DIR/docs
sudo sphinx-build -a -b html ./ /var/www/html
