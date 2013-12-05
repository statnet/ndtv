\name{install.graphviz}
\alias{install.graphviz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Instructions for installing the Graphviz libraries on various platforms
}
\description{
The \code{\link{network.layout.animate.Graphviz}} layout provides an interface for calling the various layouts provided by the Graphviz library (\url{http://www.graphviz.org}) if it is installed on your system.  Since Graphviz is \emph{not} an R package, you must manually install it on your system to get it to work. 
}
\usage{
install.graphviz()
}
%- maybe also 'usage' for other objects documented here.
\details{
This function doesn't actually install Graphviz, it just points to these docs which give a very brief overview of how to do it on each platform. 

\subsection{Installing on Windows}{
  \itemize{
    \item download the "current stable release" installer from \url{http://www.graphviz.org/Download_windows.php}
    \item run the installer
    \item Edit your system path variable to include the path to the directory containing the graphviz .exe files.
  }
}

\subsection{Installing on a Mac}{
\itemize{
\item download the most recent "current stable release" .pkg file for your system from \url{http://www.graphviz.org/Download_macos.php}
\item double click the package to run the installer or, if you are working from the OSX command line, run something like \code{sudo installer -pkg graphviz-2.34.0.pkg -target /}
}
}

\subsection{Installing in Linux/unix}{
\itemize{
 \item Graphviz is a standard package on many linux distributions. You can check if it is installed with a command like \code{dpkg -s graphivz}.  If it is not installed, you should be able to install it with your system's package manager. i.e. \code{sudo apt-get install graphviz} or search 'graphivz' in the Software Center on Ubuntu.
}
}
When Graphviz is installed correctly on any platform the R command \code{Sys.which('neato')} should print out the path to the installed libraries.
}
\value{
On some platforms this function will open a web browser pointing to the download page for Graphviz. 
}
\references{
John Ellson et.al (2001) "Graphviz -- open source graph drawing tools" Lecture Notes in Computer Science. Springer-Verlag. p483-484  \url{http://www.graphviz.org}
}
\author{
skyebend
}

\seealso{
 See \code{\link{network.layout.animate.Graphviz}} for more details about how ndtv usees Graphviz.
}

