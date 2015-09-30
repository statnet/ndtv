%  File man/install.ffmpeg.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{install.ffmpeg}
\alias{install.ffmpeg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Instructions for installing ffmpeg on various platforms
}
\description{
The animation package uses ffmpeg to export movies into video formats. This internal function doesn't actually install the ffmpeg library, it just gives instructions on how to do the installation -- which really just point to these docs. 
}
\usage{
install.ffmpeg()
}
%- maybe also 'usage' for other objects documented here.
\details{
Here are some all-too-brief instructions for the various platforms. After you have installed FFmpeg on your system, you can verify that R knows where to find it by typing \code{Sys.which('ffmpeg')} in the R terminal. You many need to first restart R after the install. 
\subsection{Installing in Windows}{
\itemize{
 \item Download the recent 'static' build from  \url{http://ffmpeg.zeranoe.com/builds/}
 \item Downloads are compressed with 7zip, so you may need to first install a 7zip decompression program before you can unpack the installer.
 \item Decompress the package and store contents on your computer (probably in Program Files)
 \item Edit your system path variable to include the path to the directory containing ffmpeg.exe
}
}

\subsection{Installing on a Mac}{
 \itemize{
  \item Download most recent build from http://www.evermeet.cx/ffmpeg/
  \item The binary files are compressed with 7zip so may need to install an unarchiving utility: \url{http://wakaba.c3.cx/s/apps/unarchiver.html}
  \item Copy ffmpeg to /usr/local/bin/ffmpeg
 }
}

\subsection{Installing in Linux/Unix (ffmpeg or avconv)}{
\itemize{
\item FFmpeg is a standard package on many linux systems.  You can check if it is installed with a command like \code{dpkg -s ffmpeg}. If it is not installed, you should be able to install with your system's package manager.  i.e. \code{sudo apt-get install ffmpeg}  or search 'ffmpeg' in the Software Center on Ubuntu.

\item Ubuntu and Debian systems may use an alternate program named "avconv" which can be installed with \code{sudo apt-get install libav-tools} or by searching 'libav-tools' in Ubuntu's Software Center. Verify that R knows where to find it by typing `Sys.which('avconv')` in the R terminal. You many need to first restart R after the install. The \code{animation} library should automatically use 'avconv' if it sees it instead of 'ffmpeg'. If it doesn't, you can tell it to by typing \code{ani.options(ffmpeg='avconv')} in your R session
}
}

}
\value{
On winddows: Will open a web browser window to the ffmpeg website and give instructions how to open this help file. 
}
\references{
\url{http://ffmpeg.org}
}
