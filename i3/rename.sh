#!/bin/bash

new_name="$(zenity --entry --text='New workspace name:')"
if [[ "$new_name" == "" ]]; then
	exit 0
fi

i3-msg "rename workspace to \"$new_name\""
