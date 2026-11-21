## PREPRO 2026

The ENDF/B preprocessing codes (PREPRO) are a collection of 18 module codes,
which are designed to convert ENDF/B formatted evaluated data from
the originally distributed ENDF-6 format to formats in which the data can be used in
application codes.

Features of the codes:
- ENDF-6 formatted data include pointwise, groupwise cross-sections,
  emitted particles angular distribution and spectra, particle and activation
  yields, multi-band parameters.
- Data can be plotted on-screen or plots can be saved as PostScript files to disk.
- The codes run on Linux, MacOS and Windows.
- PREPRO 2026 is tested with ENDF/B-VIII.1 data and is compatible with Fortran, C and C++.

**Citation**

This computer code package should be cited as follows:

```
D.E. Cullen, "PREPRO 2026: ENDF/B Pre-processing Codes", report IAEA-NDS-0261, June, 2026
```


**Important note for users of earlier versions:**

Earlier versions of PREPRO (i.e., before version 2026) may not accurately
process current ENDF/B evaluations due to recent changes in the ENDF-6
format and procedures.
PREPRO 2026 can handle the ENDF/B-VIII.1 evaluations included with this release.

**Relation to the PREPRO 2026 code provided on the IAEA-NDS website:**

This repository is complementary to the [PREPRO website][PREPRO-website]
of the Nuclear Data Section at the IAEA
and contains the source files of the PREPRO 2026 codes.
The sources come from `SOURCES.OTHERS.tar.gz` in the 2026 release bundle;
the additional `UTILITY` platform build files come from `SOURCES.IBMPC.tar.gz`.
The GNU Makefiles are adapted from the previous repository layout.
If you do not want to compile the codes yourself, the IAEA distribution
provides download packages for [Linux][PREPRO-linux], [macOS][PREPRO-macos],
and [Windows][PREPRO-windows]. A separate [source package for other
systems][PREPRO-others] is also available. Additional platform-specific
build files are included in the distribution.

### Installation

These installation instructions have been only tested on Linux and MacOS.
Assuming that *git*, *GNU make*, *GNU Fortran*, *GNU gcc* 
and *Xlib* (libX11) or compatible XQuartz X.Org are installed on your system, 
run the following commands from your command line:
```
    cd PREPRO/source
    make
    make install
    make clean
```
If successful, the executables will be available in `PREPRO/bin`.
The included `bin/` executables were built on macOS for local use; rebuild
from source on other systems. Git ignores `bin/` and generated test output.

If you provide the argument `graphics=yes`, i.e.,
```
    make install graphics=yes
```
you will also install the graphics packages `evalplot`, `evalhard`, `evalhard1`, `complot`, `comhard`, `comhard1`.
These packages require X11 in /usr for Linux or /opt for MacOS.
The interactive plot codes require an X11 display.

Verify the installation by changing into the directory `test`.
There run the command
```
    ./verify.sh
```
The script uses the PREPRO 2026 input files and writes run logs and generated
data to `test/output`. The two hardcopy plot programs run without a display.
The full input produces several hundred megabytes of output. Remove
`test/output` when verification is complete.
To also run the interactive plot programs, use `./verify.sh graphics` with X11 available.

Finally, after successful verification, move the executables to
a place of your liking. Under Linux `/usr/local/bin` is usually
a good place.

:warning: **Warning:**  If PREPRO is being installed in a virtual environment (e.g. *conda, venv, pyenv*, etc.) or alongside *oneAPI*, then the `activate` code will clash with the `activate` command available in the environment. You can check which `activate` program you are running by inspecting the full path:
```
  which activate
```
If you run into this problem, the solution is to call the PREPRO code `activate` using it's full path, or to update the `$PATH` in your `.bashrc` script to ensure that the path containing the PREPRO executables is called before the path where the `activate` environment script is.


#### Installation with Apptainer

[Apptainer] is a containerisation application similar to Docker.
If Apptainer is installed, you can run the following instructions
for installation:
```
    cd PREPRO
    apptainer build --fakeroot prepro.sif apptainer_PREPRO.def
    # alternatively:
    # sudo apptainer build prepro.sif apptainer_PREPRO.def
```
The same verification script as described above in the convential
installation can be used to verify the proper working:
```
    apptainer run prepro.sif test
```

The PREPRO codes can be exetued by
```
    apptainer run prepro.sif <PREPRO-CODE>
```
For instance, `<PREPRO-CODE>` could be `endf2c` or `fixup`.

[Apptainer]: https://sylabs.io/

### Supplementary material

The official [IAEA PREPRO 2026 release page][PREPRO-website] hosts the
**PREPRO 2026-1** distribution, including platform-specific packages, source
code, recommended input parameters, and documentation. These links point to
the 2026 distribution rather than the archived PREPRO 2023 pages.

#### Code and executable packages

| Platform | Download |
| --- | --- |
| Windows | [WindowsCodesDownload.zip][PREPRO-windows] |
| Linux | [LINUXcodeDownload.zip][PREPRO-linux] |
| macOS | [MACcodeDownload.zip][PREPRO-macos] |
| Other systems / sources | [OTHERScodeDownload.zip][PREPRO-others] |

#### Recommended input parameters

- [BEST input parameters for Unix, Linux, and macOS][PREPRO-best-unix]
- [BEST input parameters for Windows][PREPRO-best-windows]

#### Documentation

- [PREPRO code documentation (ZIP archive)][PREPRO-documentation]
- [PREPRO 2026 technical report, IAEA-NDS-0261][PREPRO-report]

The IAEA release page notes that some individual-code history documentation
covers changes through 2023; consult the source-code comments for the latest
details.

[PREPRO-website]: https://nds.iaea.org/records/nzbnz-4f488
[PREPRO-windows]: https://nds.iaea.org/records/nzbnz-4f488/files/WindowsCodesDownload.zip
[PREPRO-linux]: https://nds.iaea.org/records/nzbnz-4f488/files/LINUXcodeDownload.zip
[PREPRO-macos]: https://nds.iaea.org/records/nzbnz-4f488/files/MACcodeDownload.zip
[PREPRO-others]: https://nds.iaea.org/records/nzbnz-4f488/files/OTHERScodeDownload.zip
[PREPRO-best-unix]: https://nds.iaea.org/records/nzbnz-4f488/files/BestParametersUNIX.zip
[PREPRO-best-windows]: https://nds.iaea.org/records/nzbnz-4f488/files/BestParametersWindows.zip
[PREPRO-documentation]: https://nds.iaea.org/records/nzbnz-4f488/files/Documentation.zip
[PREPRO-report]: https://doi.org/10.61092/iaea.2qcz-xqh5

### Legal note

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS OR IAEA BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
