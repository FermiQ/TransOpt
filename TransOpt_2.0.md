# TransOpt 2.0 Documentation

## Overview

TransOpt 2.0 is an updated version of the TransOpt Fortran program, designed for calculating electronic transport properties based on Boltzmann transport theory. It builds upon version 1.1 by incorporating more advanced scattering mechanisms, particularly ionized impurity scattering, and streamlining input file management. It continues to use pre-calculated electronic band structures and group velocities, typically from DFT software like VASP, as primary inputs.

## Key Components

The program structure is similar to version 1.1, with a main program (`TransOpt`) and several modules and subroutines. Key changes and additions in version 2.0 include:

*   **Program `TransOpt`**: Main driver, similar to 1.1, but adapted for new input handling and scattering options.
*   **Module `params`**: Defines physical constants and precision. Adds `epsilon_0` (vacuum permittivity).
*   **Module `Fermifunction`**: Unchanged from 1.1.
    *   `DFERMI(E, EF, T, spi)`
    *   `FERMI(E, EF, T, spi)`
*   **Module `variable`**: Declares global variables. Key changes:
    *   `vecb(3,3)`: Reciprocal lattice vectors.
    *   `scattering`: Integer flag to select scattering mechanisms.
*   **Module `ef`**: Unchanged from 1.1.
    *   `getef(ferminorigin, fermaxorigin)`
*   **Module `variables`**: Declares global variables for electronic structure and transport. Key changes:
    *   **Input Handling**: `control.in` is deprecated. Its functionality (ifderivation, dimens, ifprint) and new scattering control are managed via `TransOpt.input`.
    *   **Scattering Parameters**: New variables for ionized impurity scattering: `epsilon_r` (relative dielectric constant), `nimpurity` (impurity concentration), `n0` (carrier concentration for screening), `zion` (charge state of impurity).
    *   **Deformation Potential**: `Edef(:)` from version 1.1 is now split into `EdefVB(:)` (for valence bands) and `EdefCB(:)` (for conduction bands) for more specific treatment.
    *   The `readTAU` flag and related variables (`sigma_origin`, `T_origin`, `elastic_origin`) are noted as discontinued/unnecessary in the comments, as `getTAU` now recalculates relaxation times based on the chosen `scattering` model.
*   **Subroutine `getTAU()`**: Significantly updated to handle different scattering mechanisms:
    *   If `scattering = 1` (acoustic phonon scattering): Calculates relaxation time based on deformation potential theory, similar to 1.1 but using `EdefVB` and `EdefCB`.
    *   If `scattering = 2` (acoustic phonon + ionized impurity scattering): Calculates relaxation rates for both mechanisms and combines them using Matthiessen's rule. Ionized impurity scattering calculation involves parameters like `zion`, `nimpurity`, `n0`, `epsilon_r`, and screening length `LD`.
    *   The calculation of relaxation times is now done on the full k-mesh (`alltau`) directly, rather than interpolating from irreducible k-mesh (`irtau`).
*   **Subroutine `getdos()`**: Similar to 1.1, calculates DOS. If `scattering > 0`, it also calculates `dostau` using the new `alltau`.
*   **Subroutine `gettrans()`**: Similar to 1.1, calculates transport coefficients. Uses `alltau` if `scattering > 0`.
*   **Subroutine `output()`**: Similar to 1.1, writes calculated transport properties. Output format remains largely the same.
*   **Subroutine `vkrotate(finalvk, originalvk, sym, vec)`**: Unchanged from 1.1.
*   **Subroutine `inverse(a, b)`**: Unchanged from 1.1.
*   **Subroutine `matmulonethree(a, b, c)`**: Unchanged from 1.1.
*   **Subroutine `matmulthreethree(a, b, c)`**: Unchanged from 1.1.

## Important Variables/Constants

Key input parameters for TransOpt 2.0 are read from `TransOpt.input` (which replaces `finale.input` and `control.in`):

*   **Line 1**: `ifderivation` (Logical: `.TRUE.` for derivation method, `.FALSE.` for momentum matrix).
*   **Line 2**: `dimens(:)` (Logical array (3 elements): x, y, z dimensions for transport).
*   **Line 3**: `ifprint` (Logical: `.TRUE.` for detailed printing).
*   **Line 4**: `scattering` (Integer: `0` for CRTA only, `1` for acoustic phonon scattering (RTA), `2` for acoustic + ionized impurity scattering (RTA)).
*   **Line 5 (general parameters)**:
    *   `efermi`: Initial Fermi level (eV).
    *   `estart`: Starting energy relative to `efermi` (eV).
    *   `de`: Energy step (eV).
    *   `nemu`: Number of energy points.
    *   `T`: Temperature (K).
    *   `nCBB(:)`: Number of conduction/valence bands for scissor operation (for each spin).
    *   `sis(:)`: Scissor energy shift (eV) (for each spin).
    *   `carriertotale`: Target total carrier concentration if `carrier` flag is true.
*   **Line 6 (if `scattering > 0`)**:
    *   `EdefVB(:)`: Deformation potential for valence bands (eV) (for each spin).
    *   `EdefCB(:)`: Deformation potential for conduction bands (eV) (for each spin).
    *   `elastic`: Elastic constant (GPa).
    *   `sigma`: Gaussian smearing width for energy (eV).
*   **Line 7 (if `scattering = 2`)**:
    *   `zion`: Charge state of the impurity.
    *   `nimpurity`: Impurity concentration (e.g., in 10^20 cm^-3).
    *   `n0`: Carrier concentration for screening (e.g., in 10^20 cm^-3).
    *   `epsilon_r`: Relative dielectric constant.

## Usage Examples

To run TransOpt 2.0, the following input files are generally required:

*   `TransOpt.input`: Contains all control parameters and calculation settings.
*   `EIGENVAL`: Eigenvalues from VASP.
*   `POSCAR`: Crystal structure.
*   `SYMMETRY`: Symmetry operations.
*   `OUTCAR`: VASP output file (used to check for SOC if `ifderivation` is true, and potentially for other parameters like dielectric constant if not specified in input).
*   **Either `GROUPVEC` or `GVEC`**:
    *   `GROUPVEC`: Contains group velocities (if `ifderivation = .FALSE.`).
    *   `GVEC`: Group velocities from derivation method (if `ifderivation = .TRUE.`).

**Expected output files:**
*   `CRTA-trace-e.txt`: Trace of transport tensors vs. energy (CRTA).
*   `CRTA-tensor-e.txt`: Full transport tensors vs. energy (CRTA).
*   `RTA-trace-e.txt`: Trace of transport tensors vs. energy (RTA, if `scattering > 0`).
*   `RTA-tensor-e.txt`: Full transport tensors vs. energy (RTA, if `scattering > 0`).
*   `TAU` (if `scattering > 0`): Calculated relaxation times on the full k-mesh.

## Dependencies and Interactions

*   **VASP**: Relies on VASP outputs for `EIGENVAL`, `POSCAR`, `OUTCAR`, and often `GROUPVEC`.
*   **Input File**: The primary control is through `TransOpt.input`.
*   **`derivation.f90`**: The `GVEC` file, if used, can be generated by the `derivation.f90` program.
