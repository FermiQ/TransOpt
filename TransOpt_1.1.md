# TransOpt 1.1 Documentation

## Overview

TransOpt 1.1 is a Fortran program designed to calculate electronic transport properties of materials. It utilizes Boltzmann transport theory, typically taking pre-calculated electronic band structures and group velocities (often from DFT software like VASP) as input. The program then computes various transport coefficients such as electrical conductivity, Seebeck coefficient, and electronic thermal conductivity.

## Key Components

The program consists of a main program (`TransOpt`) and several modules and subroutines:

*   **Program `TransOpt`**: This is the main driver of the code. It handles reading input files, orchestrating the calculation steps by calling various subroutines, and writing the final transport properties to output files.
*   **Module `params`**: Defines fundamental physical constants (e.g., Boltzmann constant, Planck constant, electron charge, electron mass) and numerical precision parameters (e.g., `dp` for double precision).
*   **Module `Fermifunction`**:
    *   `DFERMI(E, EF, T, spi)`: Calculates the derivative of the Fermi-Dirac distribution function with respect to energy.
    *   `FERMI(E, EF, T, spi)`: Calculates the Fermi-Dirac distribution function.
*   **Module `variable`**: Declares global variables related to k-points (coordinates `irkpt`, `kpt`; weights `irkwpt`), band energies (`irbandenergy`, `bandenergy`), crystal structure information (e.g., lattice vectors `vec`, cell volume `volume`, scaling factor `scales`), and run control parameters (e.g., number of k-points `nirk`, `nk`; number of bands `nband`; number of spins `nspin`).
*   **Module `ef`**:
    *   `getef(ferminorigin, fermaxorigin)`: A subroutine to determine the Fermi level (`efermi`) iteratively based on the total number of electrons (`totale`) and the density of states.
*   **Module `variables`**: Declares a wide range of global variables essential for the transport calculations. These include:
    *   Electronic structure arrays: group velocities (`irvk`, `vk`), relaxation times (`irtau`, `alltau`).
    *   Transport coefficients: conductivity (`cond`, `cond_tau`), Seebeck coefficient (`seebeck`, `seebeck_tau`), electronic thermal conductivity (`ke`, `ke_tau`), and their inverses or temporary calculation arrays.
    *   Input parameters: energy range for calculations (`estart`, `de`, `nemu`), temperature (`T`), scissor operation parameters (`nCBB`, `sis`), deformation potential (`Edef`), elastic constant (`elastic`), Gaussian smearing width (`sigma`).
    *   Control flags: `CRTAonly` (Constant Relaxation Time Approximation only), `carrier` (whether to calculate carrier concentration dependent properties), `readTAU` (flag to read pre-calculated TAU file).
*   **Subroutine `getTAU()`**: Calculates the momentum relaxation times (`irtau`). In version 1.1, this is primarily based on deformation potential (DP) theory for electron-acoustic phonon scattering. It considers parameters like deformation potential (`Edef`), elastic constant (`elastic`), and temperature (`T`).
*   **Subroutine `getdos()`**: Calculates the Density of States (DOS) and related energy-integrated quantities based on the band energies and Fermi level.
*   **Subroutine `gettrans()`**: This is a core calculation routine. It computes the tensors for electrical conductivity (`cond`, `cond_tau`), the Mueller matrix components (`mu`, `mu_tau`), and the kappa tensors (`kappa`, `kappa_tau`) by integrating products of group velocities, relaxation times, and Fermi-Dirac distribution derivatives over the Brillouin zone. It then derives the Seebeck coefficient and electronic thermal conductivity from these tensors.
*   **Subroutine `output()`**: Writes the calculated transport properties (conductivity, Seebeck coefficient, power factor, electronic thermal conductivity, Lorenz number) as a function of energy/chemical potential (`emu`) to various output files: `CRTA-trace-e.txt` (trace of tensors under CRTA), `CRTA-tensor-e.txt` (full tensors under CRTA), and if RTA is performed (`.not. CRTAonly`), `RTA-trace-e.txt` and `RTA-tensor-e.txt`.
*   **Subroutine `vkrotate(finalvk, originalvk, sym, vec)`**: Rotates a group velocity vector (`originalvk`) according to a given symmetry operation matrix (`sym`) and lattice vectors (`vec`) to obtain the rotated vector (`finalvk`).
*   **Subroutine `inverse(a, b)`**: Calculates the inverse (`b`) of a 3x3 matrix (`a`).
*   **Subroutine `matmulonethree(a, b, c)`**: Performs a multiplication of a 1x3 vector (`a`) with a 3x3 matrix (`b`) resulting in a 1x3 vector (`c`).
*   **Subroutine `matmulthreethree(a, b, c)`**: Performs a multiplication of two 3x3 matrices (`a` and `b`) resulting in a 3x3 matrix (`c`).

## Important Variables/Constants

Key input parameters that control the behavior of TransOpt 1.1 are typically read from `finale.input` and `control.in`:

*   **From `finale.input`**:
    *   `efermi`: Initial Fermi level (eV).
    *   `estart`: Starting energy relative to `efermi` for transport calculations (eV).
    *   `de`: Energy step for transport calculations (eV).
    *   `nemu`: Number of energy points to calculate.
    *   `T`: Temperature (K).
    *   `nCBB(:)`: Number of conduction/valence bands for scissor operation (for each spin).
    *   `sis(:)`: Scissor energy shift (eV) (for each spin).
    *   `carriertotale`: Target total carrier concentration if `carrier` flag is true.
    *   `Edef(:)`: Deformation potential (eV) (for each spin).
    *   `elastic`: Elastic constant (GPa).
    *   `sigma`: Gaussian smearing width for energy (eV).
*   **From `control.in`**:
    *   `ifderivation`: Logical flag, `.TRUE.` if using derivation method for group velocities (requires `GVEC` and `OUTCAR`), `.FALSE.` for momentum matrix method (requires `GROUPVEC`).
    *   `dimens(:)`: Logical array (3 elements) specifying which dimensions (x, y, z) to consider for transport.
    *   `ifprint`: Logical flag, `.TRUE.` to print detailed intermediate outputs like `EIGENVAL_full`, `klist`, `GROUPVEC_full`.

## Usage Examples

To run TransOpt 1.1, the following input files are generally required in the working directory:

*   **For momentum matrix method (default if `ifderivation` is `.FALSE.` or `control.in` is absent):**
    *   `GROUPVEC`: Contains group velocities.
    *   `EIGENVAL`: Eigenvalues from VASP.
    *   `POSCAR`: Crystal structure.
    *   `SYMMETRY`: Symmetry operations.
    *   `finale.input`: Main calculation parameters.
    *   `control.in` (optional): Controls derivation method, dimensions, and printing.
*   **For derivation method (`ifderivation` is `.TRUE.` in `control.in`):**
    *   `GVEC`: Group velocities (alternative to `GROUPVEC`, possibly from `derivation.f90`).
    *   `EIGENVAL`
    *   `POSCAR`
    *   `SYMMETRY`
    *   `finale.input`
    *   `control.in`
    *   `OUTCAR`: VASP output file (used to check for SOC if `ifderivation` is true).

**Expected output files:**
*   `CRTA-trace-e.txt`: Trace of transport tensors vs. energy (CRTA).
*   `CRTA-tensor-e.txt`: Full transport tensors vs. energy (CRTA).
*   `RTA-trace-e.txt`: Trace of transport tensors vs. energy (RTA, if not `CRTAonly`).
*   `RTA-tensor-e.txt`: Full transport tensors vs. energy (RTA, if not `CRTAonly`).
*   `TAU` (if generated): Calculated relaxation times.

## Dependencies and Interactions

*   **VASP**: TransOpt heavily relies on outputs from VASP (or compatible DFT codes) for `EIGENVAL`, `POSCAR`, `OUTCAR`, and often `GROUPVEC`.
*   **Input Files**: The behavior is controlled by `finale.input` and `control.in`. The format and content of these files are critical.
*   **`derivation.f90`**: The `GVEC` file, if used, can be generated by the `derivation.f90` program.
