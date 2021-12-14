## PREPRO 2021

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
- PREPRO 2021 is ENDF/B-tested and completely Fortran, C and C++ compatible.


**Citation**

This computer code package should be cited as follows:

```
D.E. Cullen, "PREPRO 2021: ENDF/B Pre-processing Codes", report IAEA-NDS-0238, September, 2021
```


**Important note for users of earlier versions:**

Earlier versions of PREPRO (i.e., before version 2019) may not accurately
process current ENDF/B evaluations due to recent changes in the ENDF-6
format and procedures.
PREPRO 2021 can handle all past and present existing ENDF/B evaluations.

**Relation to the PREPRO 2021 code provided on the IAEA-NDS website:**

This repository is complementary to the [PREPRO website][PREPRO2021-website]
of the Nuclear Data Section at the IAEA
and contains the source files of the PREPRO 2021 codes.
If you do not want to compile the codes yourself or are unsuccessful using the
instructions provided below, you find executables
for Linux, MacOS and Windows [here][PREPRO2021-codes]. Additional
makefiles are also provided there which may be more pertinent for your
system.

### Installation

These installation instructions have been only tested on Linux and MacOS.
Assuming that *git*, *GNU make*, *GNU Fortran*, *GNU gcc*, *Intel OneApi* 
and *Xlib* (libX11) or compatible XQuartz X.Org are installed on your system, 
run the following commands from your command line:
```
    git clone https://github.com/IAEA-NDS/PREPRO.git
    cd PREPRO/source
    make
    make install
    make clean
```
If successful, the executables will be available in `PREPRO/bin`.

```
    make install graphics=yes
    make clean
```
Will also install the graphics packages that requires X11 in /usr for Linux or
/opt for MacOS

Verify the installation by changing into the directory `test`.
There run the command
```
    ./verify.sh
```
At the very end of the execution, you will see the plots by
*COMPLOT* comparing the result of PREPRO 2015 and PREPRO 2021.
The difference between these
versions should not exceed 2%, i.e., the plotted ratio should
be between 0.98 and 1.02.

Finally, after successful verification, move the executables to
a place of your liking. Under Linux `/usr/local/bin` is usually
a good place.


:warning: **Warning**: If PREPRO is being installed in a virtual environment (e.g. conda), then the `activate` code will clash with the `activate` command available in the environment.


#### Installation with Singularity

[Singularity] is a containerisation application similar to Docker.
If Singularity is installed, you can run the following instructions
for installation:
```
    git clone https://github.com/IAEA-NDS/PREPRO.git
    cd PREPRO
    singularity build --fakeroot prepro.sif singularity_PREPRO.def
    # alternatively:
    # sudo singularity build prepro.sif singularity_PREPRO.def
```
The same verification script as described above in the convential
installation can be used to verify the proper working:
```
    singularity run prepro.sif test
```

The PREPRO codes can be exetued by
```
    singularity run prepro.sif <PREPRO-CODE>
```
For instance, `<PREPRO-CODE>` could be `endf2c` or `fixup`.

[Singularity]: https://sylabs.io/

### Supplementary material

The PREPRO 2021 codes can also be found [here][PREPRO2021-website]
on the IAEA-NDS website. In particular, these resources are provided:
- [Download of codes for various operating systems][PREPRO2021-codes]
- [Best input parameters][PREPRO2021-best-parameters]
- [Documentation][PREPRO2021-documentation]

[PREPRO2021-website]: https://www-nds.iaea.org/public/endf/prepro/
[PREPRO2021-codes]: https://www-nds.iaea.org/public/endf/prepro/ask4code.html
[PREPRO2021-best-parameters]: https://www-nds.iaea.org/public/endf/prepro/ask4best.html
[PREPRO2021-documentation]: https://www-nds.iaea.org/public/endf/prepro/DOCUMENT/ask4docs.html


### Legal note

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS OR IAEA BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
