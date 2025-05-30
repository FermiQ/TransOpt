# Derivation Program Documentation (`derivation.f90`)

## Overview

The Fortran program `derivation.f90` (containing a main program named `main`) is designed to calculate k-space derivatives of electronic band energies. These derivatives are used to determine group velocities and effective mass tensors. The outputs of this program, particularly the group velocities (`GVEC`), can serve as input for the main TransOpt transport calculation programs (e.g., `TransOpt_1.1.f90`, `TransOpt_2.0.f90`) when the `ifderivation` flag is enabled. Alternatively, the calculated properties can be used for direct analysis of band structure features.

## Key Components

The program consists of a main program (`main`) and several modules and subroutines:

*   **Program `main`**:
    *   Reads crystal structure information from `POSCAR`.
    *   Reads symmetry operations from `SYMMETRY`.
    *   Reads eigenvalues from `EIGENVAL`.
    *   Reads the Fermi level from `OUTCAR`.
    *   Performs k-point symmetrization to generate a full k-mesh.
    *   Sorts the k-points using the `quick_sort` subroutine.
    *   Calls the `derivation` subroutine to calculate first and second derivatives of energy with respect to k.
    *   Writes the calculated group velocities to `GVEC`.
    *   Writes the calculated effective mass tensors (inverse) to `EFMASS`.
*   **Module `params`**: Defines fundamental physical constants (e.g., Boltzmann constant, Planck constant, electron charge, electron mass) and numerical precision parameters.
*   **Module `variable`**: Declares global variables used within the program, such as temperature (`T`), Fermi energy (`efermi`), lattice vectors (`vec`), volume (`volume`), number of bands (`nbands`), and number of spins (`nspins`).
*   **Recursive Subroutine `quick_sort(k, left, right, corresponding)`**:
    *   Sorts an array of k-points (`k`) in three dimensions (primarily by z, then y, then x).
    *   Keeps track of the original indices using the `corresponding` array. This is used to ensure that eigenvalues are correctly mapped after k-points are reordered.
*   **Subroutine `derivation(Energy, x, y, z, lenth, rtvecb, corresponding, derva, dervb, dervc, dervx, dervy, dervz)`**:
    *   Calculates the first derivatives of energy (`Energy`) with respect to k in fractional coordinates (`derva`, `dervb`, `dervc`) and Cartesian coordinates (`dervx`, `dervy`, `dervz`) using finite difference methods.
    *   `x`, `y`, `z`: Number of k-points along each reciprocal lattice direction.
    *   `lenth`: Step sizes for finite differences in fractional coordinates.
    *   `rtvecb`: Transformation matrix from fractional to Cartesian coordinates for derivatives.
    *   The effective mass tensor components are calculated by further calls to this subroutine, passing previously calculated first derivatives (velocities) as input to get second derivatives.
*   **Subroutine `inverse(a)`**: Calculates the inverse of a 3x3 matrix `a` *in place*. Note: This is a different `inverse` subroutine than the one found in `TransOpt_X.X.f90` programs, as it takes only one matrix argument and modifies it directly.
*   **Subroutine `error()`**: A simple subroutine to stop the program execution, typically called when a required file is not found.

## Important Variables/Constants

*   **Input Data**: The program relies heavily on the content and format of:
    *   `POSCAR`: For lattice vectors and scale.
    *   `SYMMETRY`: For symmetry operations to unfold the irreducible k-points.
    *   `EIGENVAL`: For k-points in the irreducible Brillouin zone and their corresponding band energies.
    *   `OUTCAR`: Specifically to read the Fermi energy (`E-fermi` line).
*   **Internal Parameters**:
    *   `nkpts`: Number of k-points read from `EIGENVAL`.
    *   `nbands`: Number of bands read from `EIGENVAL`.
    *   `nspins`: Number of spins read from `EIGENVAL`.
    *   `x, y, z`: Dimensions of the generated full k-mesh.
    *   `fullkptcal`: Array holding the full k-mesh coordinates.
    *   `fulleigcal`: Array holding energies on the full k-mesh.
    *   `corresponding`: Array mapping sorted k-points back to their original order/properties.

## Usage Examples

To run the `derivation.f90` program (after compiling it), the following input files must be present in the working directory:

*   `EIGENVAL`: VASP output containing irreducible k-points and eigenvalues.
*   `POSCAR`: VASP input file defining the crystal structure.
*   `SYMMETRY`: VASP output containing symmetry operations.
*   `OUTCAR`: VASP main output file (used for Fermi level).

**Execution:**
Running the compiled executable (e.g., `./derivation_program`) will produce:

*   `GVEC`: Output file containing the calculated group velocities (dx, dy, dz components for each band and k-point) in units of m/s (after internal conversion from atomic units). The format includes k-point coordinates, band index, group velocity components, and magnitude.
*   `EFMASS`: Output file containing the inverse effective mass tensor components (m_xx, m_xy, m_xz, etc.) for each band and k-point. The values are scaled by (a/2pi)^2, where 'a' is likely related to lattice dimensions.
*   `klist_int` (intermediate): A list of k-points.

## Dependencies and Interactions

*   **VASP**: This program is highly dependent on VASP output files (`EIGENVAL`, `POSCAR`, `SYMMETRY`, `OUTCAR`).
*   **TransOpt Programs**: The `GVEC` file generated by `derivation.f90` can be used as an input to `TransOpt_1.1.f90` or `TransOpt_2.0.f90` (and their OMP versions) if the `ifderivation` flag in the TransOpt input (`control.in` or `TransOpt.input`) is set to `.TRUE.`. This provides an alternative to using the `GROUPVEC` file (momentum matrix elements) directly from VASP.
