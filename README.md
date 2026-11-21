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
If you do not want to compile the codes yourself or are unsuccessful using the
instructions provided below, you find executables
for Linux, MacOS and Windows [here][PREPRO-codes]. Additional
makefiles are also provided there which may be more pertinent for your
system.

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

The PREPRO 2026 codes can also be found [here][PREPRO-website]
on the IAEA-NDS website. In particular, these resources are provided:
- [Download of codes for various operating systems][PREPRO-codes]
- [Best input parameters][PREPRO-best-parameters]
- [Documentation][PREPRO-documentation]

[PREPRO-website]: https://www-nds.iaea.org/public/endf/prepro/
[PREPRO-codes]: https://www-nds.iaea.org/public/endf/prepro/ask4code.html
[PREPRO-best-parameters]: https://www-nds.iaea.org/public/endf/prepro/BEST/ask4best.html
[PREPRO-documentation]: https://www-nds.iaea.org/public/endf/prepro/DOCUMENT/ask4docs.html


### Legal note

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS OR IAEA BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
