#!/bin/bash

cd /path/to/your/repo

git pull origin main

git add .

commit_message=${1:-"Update"}
git commit -m "$commit_message"

git push origin main
