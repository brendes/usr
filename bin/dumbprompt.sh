#!/bin/sh

if [ -n "$SSH_CONNECTION" ]; then
  u="$(whoami)@"
  h="$(hostname | sed 's/\..*//'):"
else
  u=""
  h=""
fi

w="$(pwd -P | sed s,$HOME,~,g)"

printf %s "${u}${h}${w} $(git-branch-stat -b)"
unset u h w
