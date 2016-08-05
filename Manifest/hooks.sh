#!/bin/bash

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

old_dir=`pwd`; cd "$_base/.."; bdlroot=`pwd`; cd "$old_dir"; old_dir=

# ---------------------------------------------------------------------------
# Configuration

buddy_name="buddy-2.4"

function select_bin_dist() {
    case "$CIAO_OS" in
	*)
	    echo "ERROR: Binary unavailable for CIAO_OS=$CIAO_OS" 1>&2
	    exit 1
    esac
}

function select_src_dist() {
    buddy_baseurl="https://sourceforge.net/projects/buddy/files/buddy/BuDDy%202.4"
    buddy_file="buddy-2.4.tar.gz"
}

# --------------------------------------------------------------------------

if [ "$THIRDPARTY" = "" ]; then
    cat <<EOF
ERROR: THIRDPARTY directory missing (use 'ciao build')
EOF
    exit 1
fi

cachedir="$THIRDPARTY/cache"
storedir="$THIRDPARTY/store"
srcdir="$THIRDPARTY/src"

# --------------------------------------------------------------------------

function fetch_buddy() {
    # Ensure that cachedir is created
    mkdir -p "$cachedir"

    # Download buddy
    rm -f "$cachedir/$buddy_file"

    buddy_url="$buddy_baseurl/$buddy_file/download"
    curl -L "$buddy_url" -o "$cachedir/$buddy_file"
}

function uncompress_buddy_bin() {
    # Cleanup storedir for buddy and uncompress
    rm -rf "$storedir/$buddy_name"
    mkdir -p "$storedir/$buddy_name"
    tar -xz --strip-components 1 -f "$cachedir/$buddy_file" -C "$storedir/$buddy_name"
}

function uncompress_buddy_src() {
    # Cleanup srcdir for buddy and uncompress
    rm -rf "$srcdir/$buddy_name"
    mkdir -p "$srcdir/$buddy_name"
    tar -xz --strip-components 1 -f "$cachedir/$buddy_file" -C "$srcdir/$buddy_name"
}

function build_buddy() {
    pushd "$srcdir/$buddy_name" > /dev/null 2>&1

    LDFLAGS="-L$THIRDPARTY/lib" CPPFLAGS="-I$THIRDPARTY/include" LD_LIBRARY_PATH="$THIRDPARTY/lib" ./configure --prefix="$storedir/$buddy_name"
    make
    
    # Cleanup storedir for buddy and install
    rm -rf "$storedir/$buddy_name"
    mkdir -p "$storedir/$buddy_name"
    make install
    
    popd > /dev/null 2>&1
}

function fix_dylibs() {
    # TODO: We do not use install-buddy, just link ourselves the lib
    local buddylibVerN buddylibN
    case "$CIAO_OS" in
	LINUX)
	    buddylibVerN="libbdd.so.0.0"
	    buddylibN="libbdd.so"
	    ;;
	DARWIN)
	    buddylibVerN="libbdd.0.dylib"
	    buddylibN="libbdd.dylib"
	    ;;
    esac
    local buddylibVer="$storedir/$buddy_name/lib/$buddylibVerN"
    local buddylib="$storedir/$buddy_name/lib/$buddylibN"
    # Fix install dir (it was /usr/local)
    case "$CIAO_OS" in
	LINUX)
	    pushd "$storedir/$buddy_name" > /dev/null 2>&1
	    /sbin/ldconfig -n "lib"
            # Link name without version
	    ln -sf "$buddylibVerN" "lib/$buddylibN"
	    popd > /dev/null 2>&1
	    ;;
	DARWIN)
	    install_name_tool -id "$buddylibVer" "$buddylibVer"
            # Link name without version
	    ln -sf "$buddylibVer" "$buddylib"
	    ;;
    esac
}

# ---------------------------------------------------------------------------

function gen_config_auto() {
    local RPATH=
    case "$CIAO_OS" in
	LINUX)
	    RPATH="-Wl,-rpath,$storedir/$buddy_name/lib,-rpath,\\'\$ORIGIN\\'"
	    ;;
    esac
    cat > $bdlroot/src/ciao_buddy_config_auto.pl <<EOF
:- extra_compiler_opts([
	% For Buddy
	'-I$storedir/$buddy_name/include'
	]).
:- extra_linker_opts(' -L.').
:- extra_linker_opts([
	% For Buddy
	'$RPATH -L$storedir/$buddy_name/lib'
	]).

:- use_foreign_library(['bdd']).
EOF
}

# ===========================================================================

function install_dist() { # Mode=bin|src
    if [ -x "$storedir/$buddy_name" ]; then
	# echo "buddy already downloaded" 1>&2
	return 0
    fi

    if [ "$1" = bin ]; then
	select_bin_dist
    else # src
	select_src_dist
    fi
    fetch_buddy
    if [ "$1" = bin ]; then
	uncompress_buddy_bin
    else # src
	uncompress_buddy_src
	build_buddy
    fi
    fix_dylibs
}

# ===========================================================================

case $1 in
    install_bin_dist) install_dist bin ;;
    install_src_dist) install_dist src ;;
    gen_conf) gen_config_auto ;;
    *)
	echo "ERROR: Unknown action" 1>&2
	exit 1
esac
