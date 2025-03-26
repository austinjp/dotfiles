#!/bin/bash

redshift -p 2>/dev/null | ag --silent Daytime >/dev/null && theme-light.sh || theme-dark.sh
