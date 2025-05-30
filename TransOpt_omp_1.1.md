# TransOpt OMP 1.1 Documentation

## Overview

TransOpt OMP 1.1 is the OpenMP-parallelized version of the `TransOpt_1.1.f90` program. It is designed to calculate electronic transport properties of materials using Boltzmann transport theory and can leverage multi-core processors to speed up computationally intensive parts of the calculation. Like its serial counterpart, it uses pre-calculated electronic band structures and group velocities as input.

## Key Components

The core logic, modules, and most subroutines in TransOpt OMP 1.1 are identical to those in `TransOpt_1.1.f90`. Please refer to the `TransOpt_1.1.md` documentation for a detailed description of:

*   Program `TransOpt`
*   Module `params`
*   Module `Fermifunction`
*   Module `variable`
*   Module `ef`
*   Module `variables`
*   Subroutine `getdos`
*   Subroutine `gettrans`
*   Subroutine `output`
*   Subroutine `vkrotate`
*   Subroutine `inverse`
*   Subroutines `matmulonethree`, `matmulthreethree`

The primary difference lies in the **`getTAU` subroutine**:

*   **Subroutine `getTAU()`**: This subroutine, which calculates momentum relaxation times, has been parallelized using OpenMP directives. Specifically, the loops involved in calculating the scattering rates (which sum over k-points and bands) are parallelized. This can significantly reduce the wall time for this part of the calculation on systems with multiple CPU cores. The underlying physics and calculation method for relaxation time (deformation potential theory) remain the same as in `TransOpt_1.1.f90`.

## Important Variables/Constants

The important variables, constants, and input parameters read from `finale.input` and `control.in` are identical to those used in `TransOpt_1.1.f90`. Please refer to the "Important Variables/Constants" section in `TransOpt_1.1.md`.

## Usage Examples

The required input files (`GROUPVEC`, `EIGENVAL`, `POSCAR`, `SYMMETRY`, `finale.input`, `control.in`, `GVEC`, `OUTCAR`) and the expected output files (`CRTA-trace-e.txt`, `CRTA-tensor-e.txt`, `RTA-trace-e.txt`, `RTA-tensor-e.txt`, `TAU`) are the same as for `TransOpt_1.1.f90`.

Refer to the "Usage Examples" section in `TransOpt_1.1.md`.

The main difference in usage is that the compiled `TransOpt_omp_1.1` executable can benefit from setting OpenMP environment variables (e.g., `OMP_NUM_THREADS`) to control the number of threads used for parallel regions.

## Dependencies and Interactions

The dependencies and interactions with VASP outputs and input files (`finale.input`, `control.in`) are the same as for `TransOpt_1.1.f90`. Please refer to the "Dependencies and Interactions" section in `TransOpt_1.1.md`. The program must be compiled with an OpenMP-enabled Fortran compiler for the parallelization to be active.
