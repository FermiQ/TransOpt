# TransOpt OMP 2.0 Documentation

## Overview

TransOpt OMP 2.0 is the OpenMP-parallelized version of the `TransOpt_2.0.f90` program. It is designed for calculating electronic transport properties and includes advanced scattering mechanisms like ionized impurity scattering, along with streamlined input file management via `TransOpt.input`. This version can utilize multi-core processors to accelerate computationally intensive sections, particularly the calculation of relaxation times.

## Key Components

The core logic, modules, and most subroutines in TransOpt OMP 2.0 are identical to those in `TransOpt_2.0.f90`. For a detailed description of the following components, please refer to the `TransOpt_2.0.md` documentation:

*   Program `TransOpt`
*   Module `params`
*   Module `Fermifunction`
*   Module `variable`
*   Module `ef`
*   Module `variables` (including parameters for new scattering mechanisms and updated input handling)
*   Subroutine `getdos`
*   Subroutine `gettrans`
*   Subroutine `output`
*   Subroutine `vkrotate`
*   Subroutine `inverse`
*   Subroutines `matmulonethree`, `matmulthreethree`

The primary distinction of the OMP version is within the **`getTAU` subroutine**:

*   **Subroutine `getTAU()`**: This subroutine, responsible for calculating momentum relaxation times considering different scattering mechanisms (acoustic phonon, ionized impurity), has been parallelized using OpenMP directives. The loops involved in summing contributions to scattering rates over k-points and bands are parallelized. This is particularly beneficial for the `scattering=2` option (acoustic + ionized impurity), which involves a double loop over the full k-mesh. The underlying physics and calculation methods for the scattering mechanisms remain the same as in `TransOpt_2.0.f90`.

## Important Variables/Constants

The important variables, constants, and the structure of the `TransOpt.input` file (which controls calculation parameters, including scattering options) are identical to those used in `TransOpt_2.0.f90`. Please refer to the "Important Variables/Constants" section in `TransOpt_2.0.md`.

## Usage Examples

The required input files (`TransOpt.input`, `EIGENVAL`, `POSCAR`, `SYMMETRY`, `OUTCAR`, `GROUPVEC`/`GVEC`) and the expected output files (`CRTA-trace-e.txt`, `CRTA-tensor-e.txt`, `RTA-trace-e.txt`, `RTA-tensor-e.txt`, `TAU`) are the same as for `TransOpt_2.0.f90`.

Refer to the "Usage Examples" section in `TransOpt_2.0.md`.

When using `TransOpt_omp_2.0`, performance can be enhanced by setting OpenMP environment variables (e.g., `OMP_NUM_THREADS`) to control the number of threads utilized during parallel execution.

## Dependencies and Interactions

The dependencies and interactions with VASP outputs and the `TransOpt.input` file are the same as for `TransOpt_2.0.f90`. Please refer to the "Dependencies and Interactions" section in `TransOpt_2.0.md`. For the parallelization to be effective, the program must be compiled with an OpenMP-enabled Fortran compiler.
