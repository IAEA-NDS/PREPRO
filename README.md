## PREPRO 2019

The ENDF/B Pre-processing codes (PREPRO) are a collection of 18 computer codes,
which are designed to convert ENDF/B formatted neutron and/or photon data from
the originally distributed form to a form in which the data can be used in
applications.

Features of the codes:
- Data can be plotted on-screen or plots saved as PostScript files to disk.
- The codes run on Microsoft, Linux and MacOS.
- PREPRO 2019 is ENDF/B-VIII.0-tested and completely Fortran, C and C++ compatible

**Important note for users of earlier versions:**

Earlier versions of PREPRO (i.e., before version 2019-1) cannot accurately
process current ENDF/B-VIII evaluations due to recent changes in the ENDF-6
format and procedures.
PREPRO 2019 can handle all existing ENDF/B-VII.0, VII.1 and VIII evaluations.

**Relation to the PREPRO 2019 code provided on the IAEA-NDS website:**

This repository is complementary to the [PREPRO website][PREPRO2019-website]
of the Nuclear Data Section of the IAEA
and contains the source files of the PREPRO 2019 codes.
If you do not want to compile the codes yourself or are unsuccessful using the
instructions provided below, you find executables
for Windows, Linux and MacOS [here][PREPRO2019-codes]. Additional
makefiles are also provided there  which may be more pertinent for your
system.

### Installation

These installation instructions have been only tested on Linux.
Assuming that *git*, *GNU make* and *GNU Fortran* 
or compatible are installed on your system, 
run the following commands from your command line:
```
    git clone https://github.com/IAEA-NDS/PREPRO2019.git
    cd PREPRO2019/source
    make
    make install
```
If successful, the executables will be available in `PREPRO2019/bin`.

Verify the installation by changing into the directory `test`.
There run the command
```
    ./verify.sh
```
At the very end of the execution, you will see the plots by
*COMPLOT* comparing the result of PREPRO 2015 and PREPRO 2019.
The difference between these
versions should not exceed 2%, i.e., the plotted ratio should
be between 0.98 and 1.02.

Finally, after successful verification, move the executables to
a place of your liking. Under linux '/usr/local/bin' is a good
place.

### Supplementary material

The PREPRO 2019 codes can also be found [here][PREPRO2019-website]
on the IAEA-NDS website. In particular, these resources are provided:
- [Download of codes for various operating systems][PREPRO2019-codes]
- [Best input parameters][PREPRO2019-best-parameters]
- [Documentation][PREPRO2019-documentation]

[PREPRO2019-website]: https://www-nds.iaea.org/public/endf/prepro/
[PREPRO2019-codes]: https://www-nds.iaea.org/public/endf/prepro/ask4code.html
[PREPRO2019-best-parameters]: https://www-nds.iaea.org/public/endf/prepro/ask4best.html
[PREPRO2019-documentation]: https://www-nds.iaea.org/public/endf/prepro/DOCUMENT/ask4docs.html

### Citation

This computer code package should be cited as follows:

*"D.E. Cullen, "PREPRO 2019: 2019: ENDF/B Pre-processing Codes", report  IAEA-NDS-39, Rev. 19, August 20, 2019*

### Legal note

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS OR IAEA BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
