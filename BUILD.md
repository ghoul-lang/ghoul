# Install LLVM (Windows)

On Windows there is currently no precompiled version of LLVM with all libraries, cmake files, etc. included as far as I know. As such we have to compile and install LLVM ourselves.
To do so, go to http://releases.llvm.org/download.html and download the LLVM source code. I recommend version 6.0.0 as that is what it has been tested to work with.
After downloading the source code extract the tar.xz to where ever you want (NOTE: there is a bug that makes it impossible to compile LLVM if the path has a space in it. If your user directory has a space in it like mine does I would recommend placing it outside of your user directory)
Once it is extracted create a directory inside the LLVM directory called 'build-vs' and run the following command:

For Visual Studio 2019:

    cmake -G "Visual Studio 16" -DCMAKE_GENERATOR_PLATFORM=x64 -Thost=x64 ../

For Visual Studio 2017

    cmake -G "Visual Studio 15" -DCMAKE_GENERATOR_PLATFORM=x64 -Thost=x64 ../

Then open the solution file in Visual Studio, change the Solution Configuration to 'Release', then right click the 'INSTALL' project and build it. This will build everything and should install to the directory 'C:\Program Files (x86)\LLVM'.
