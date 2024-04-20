#!/bin/bash
set -e

ros run -e "(push #P\"/app/\" ql:*local-project-directories*)" -e "(ql:quickload :swank)" -e "(setf swank::*loopback-interface* \"0.0.0.0\")" -e "(swank:create-server :dont-close t :style :spawn)"
