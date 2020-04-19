#!/usr/bin/env bash

# Requires DynamoDB local and a dynamodb start script on your path
# https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.DownloadingAndRunning.html

RED='\033[1;91m'
YELLOW='\033[1;93m'
RESET='\033[0m'
BLACK_BG='\033[40m'

command -v dynamodb >/dev/null 2>/dev/null
DYNAMODB_CHECK_STATUS=$?
if [ $DYNAMODB_CHECK_STATUS -ne 0 ]; then
    >&2 echo -e $RED$BLACK_BG"This script requires DynamoDB local and a dynamodb start script on your path"$RESET
    >&2 echo -e $RED$BLACK_BG"https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.DownloadingAndRunning.html"$RESET
    exit 1
fi

dynamodb -inMemory
