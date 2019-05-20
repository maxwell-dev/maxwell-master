#!/bin/bash

current_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )";

${current_dir}/_build/default/rel/maxwell_master_prod/bin/maxwell_master_prod $1