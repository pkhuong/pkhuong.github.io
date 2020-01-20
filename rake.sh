#!/bin/bash

# Bundle derives its ruby version from its shebang line.
# You may have to edit `which bundle` to use /usr/bin/env
# or use the correct version.

exec bundle exec rake "$@"
