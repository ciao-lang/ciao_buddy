# 3rd-party package definition

# Package name
pkg_name="buddy-2.4"

# Origin for binary distribution
function pkg_bin_origin() {
    case "$CIAO_OS" in
	*)
	    echo "ERROR: Binary unavailable for CIAO_OS=$CIAO_OS" 1>&2
	    exit 1
    esac
}

# Origin for source distribution
function pkg_src_origin() {
    local baseurl="https://sourceforge.net/projects/buddy/files/buddy/BuDDy%202.4"
    pkg_tarfile="buddy-2.4.tar.gz"
    pkg_url="$baseurl/$pkg_tarfile/download"
}

# Fixes for binary distribution
function pkg_fix_bin() {
    true
}

# Build from source
function pkg_build() {
    LDFLAGS="-L$THIRDPARTY/lib" CPPFLAGS="-I$THIRDPARTY/include" LD_LIBRARY_PATH="$THIRDPARTY/lib" ./configure --prefix="$storedir/$pkg_name"
    make
}

# Install from source
function pkg_install() {
    make install
}

# Dynamic library name and files
pkg_lib=bdd
case "$CIAO_OS" in
    LINUX)
	pkg_libfile="libbdd.so.0.0"
	pkg_libfileAct="libbdd.so"
	;;
    DARWIN)
	pkg_libfile="libbdd.0.dylib"
	pkg_libfileAct="libbdd.dylib"
	;;
esac
