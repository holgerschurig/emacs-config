#!/usr/bin/env bash
force_tty=false
force_wait=false
stdin_mode=""
UNAME="$(uname)"

args=()

if [ "$UID" = "0" ]; then
	if [ "$UNAME" = "Linux" ]; then
		args+=(--socket /run/user/1000/emacs/server)
	else
		echo "running under root not supported"
		exit 1
	fi
	args+=(--socket /run/user/1000/emacs/server)
fi

while :; do
	case "$1" in
		-t | -nw | --tty)
			force_tty=true
			shift ;;
		-w | --wait)
			force_wait=true
			shift ;;
		-m | --mode)
			stdin_mode=" ($2-mode)"
			shift 2 ;;
		-h | --help)
			echo -e "\033[1mUsage: e [-t] [-m MODE] [OPTIONS] FILE [-]\033[0m

Emacs client convenience wrapper.

\033[1mOptions:\033[0m
\033[0;34m-h, --help\033[0m            Show this message
\033[0;34m-t, -nw, --tty\033[0m        Force terminal mode
\033[0;34m-w, --wait\033[0m            Don't supply \033[0;34m--no-wait\033[0m to graphical emacsclient
\033[0;34m-\033[0m                     Take \033[0;33mstdin\033[0m (when last argument)
\033[0;34m-m MODE, --mode MODE\033[0m  Mode to open \033[0;33mstdin\033[0m with

Run \033[0;32memacsclient --help\033[0m to see help for the emacsclient."
			exit 0 ;;
		--*=*)
			set -- "$@" "${1%%=*}" "${1#*=}"
			shift ;;
		*)
			if [ "$#" = 0 ]; then
				break; fi
			args+=("$1")
			shift ;;
	esac
done

if [ ! "${#args[*]}" = 0 ] && [ "${args[-1]}" = "-" ]; then
	unset 'args[-1]'
	TMP="$(mktemp /tmp/emacsstdin-XXX)"
	cat > "$TMP"
	args+=(--eval "(let ((b (generate-new-buffer \"*stdin*\"))) (switch-to-buffer b) (insert-file-contents \"$TMP\") (delete-file \"$TMP\")${stdin_mode})")
fi

if [ "$UNAME" = "Linux" ]; then
	if [ -z "$DISPLAY" ] || $force_tty; then
		# detect terminals with sneaky 24-bit support
		if { [ "$COLORTERM" = truecolor ] || [ "$COLORTERM" = 24bit ]; } \
			&& [ "$(tput colors 2>/dev/null)" -lt 257 ]; then
			if echo "$TERM" | grep -q "^\w\+-[0-9]"; then
				termstub="${TERM%%-*}"; else
				termstub="${TERM#*-}"; fi
			if infocmp "$termstub-direct" >/dev/null 2>&1; then
				TERM="$termstub-direct"; else			TERM="xterm-direct"; fi # should be fairly safe
		fi
		exec emacsclient --tty --alternate-editor="" "${args[@]}"
	fi
fi


test $force_wait && args+=(--no-wait)
# The "env -" is to protect against Nix shenanigans
env - ~/bin/emacsclient --alternate-editor="" "${args[@]}"
