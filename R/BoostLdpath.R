# These routines are adopted from RcppLdpath.R


# Copyright (C) 2010 - 2012 Dirk Eddelbuettel and Romain Francois
#
# This file is part of Rcpp.
#
# Rcpp is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rcpp is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

## make sure system.file returns an absolute path
Boost.system.file <- function(...){
    tools::file_path_as_absolute( base::system.file( ..., package = "BoostCompiledLibraries" ) )
}

## identifies if the default linking on the platform should be static
## or dynamic. Currently only linux uses dynamic linking by default
## although it works fine on mac osx as well
staticLinking <- function() {
    ! grepl( "^linux", R.version$os )
}

## Use R's internal knowledge of path settings to find the lib/ directory
## plus optinally an arch-specific directory on system building multi-arch
BoostLdPath <- function() {
    if (nzchar(.Platform$r_arch)) {	## eg amd64, ia64, mips
        path <- Boost.system.file("lib",.Platform$r_arch)
    } else {
        path <- Boost.system.file("lib")
    }
    path
}

## Provide linker flags -- i.e. -L/path/to/libBoost -- as well as an
## optional rpath call needed to tell the Linux dynamic linker about the
## location.  This is not needed on OS X where we encode this as library
## built time (see src/Makevars) or Windows where we use a static library
## Updated Jan 2010:  We now default to static linking but allow the use
##                    of rpath on Linux if static==FALSE has been chosen
##                    Note that this is probably being called from LdFlags()
BoostLdFlags <- function(static=staticLinking()) {
    rcppdir <- BoostLdPath()
    if (static) {                               # static is default on Windows and OS X
        flags <- paste(rcppdir, "/libBoost.a", sep="")
        if (.Platform$OS.type=="windows") {
            flags <- asBuildPath(flags)
        }
    } else {					# else for dynamic linking
        flags <- paste("-L", rcppdir, " -lBoost", sep="") # baseline setting
        if ((.Platform$OS.type == "unix") &&    # on Linux, we can use rpath to encode path
            (length(grep("^linux",R.version$os)))) {
            flags <- paste(flags, " -Wl,-rpath,", rcppdir, sep="")
        }
    }
    cat(invisible(flags))
}

## LdFlags defaults to static linking on the non-Linux platforms Windows and OS X
LdFlags <- function(static=staticLinking()) {
    cat(BoostLdFlags(static=static))
}


