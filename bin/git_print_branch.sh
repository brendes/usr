#!/bin/sh
# print git branch if it exists, as well as an indicator if the branch has been
# modified

if branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null); then
    if git status --porcelain=v1 2>/dev/null | tr -d ' ' | grep -q '^[UAMDR]'; then
        modified="+"
    else
        modified=""
    fi
    printf "[%s%s]" "${branch}""${modified}"
else
    exit
fi
