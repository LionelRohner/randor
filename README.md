# randor

( ͡° ͜ʖ ͡°)

## A "Sammelsurium" of R Functions

This repository is a "Sammelsurium" – a wild mix of various R functions. It was created by merging several smaller, older repositories.

**Disclaimer:** Many of these functions are not optimized for performance and are primarily for educational or experimental purposes. They often serve as less efficient, plain R implementations of functions that are already available in other packages or even in base R. They were mostly written as a fun way to explore mathematical concepts.

---

## Function Categories

The functions in this repository are grouped into the following categories:

### 2D List Comprehension
- **File:** `R/2d_list_comprehension.R`
- **Description:** An attempt to emulate Python's 2D list comprehension in R. This is based on the `comprehenr` package.

### Date Functions
- **File:** `R/dates.R`
- **Description:** Contains helper functions for date calculations, such as calculating the age between two dates.

### Pi Estimation
- **File:** `R/estimate_pi.R`
- **Description:** A collection of functions to estimate the value of π using different methods, including empirical, resampling, and MCMC-like approaches.

### Integer Functions
- **File:** `R/integers.R`
- **Description:** A set of functions for various integer operations, such as extracting digits from a number, getting powers of ten, and concatenating numbers mathematically.

### Linear Algebra (LAFNAF)
- **File:** `R/lafnaf.R`
- **Description:** **L**inear **A**lgebra **F**unctions **T**hat **N**obody **A**sked **F**or. This is a collection of functions for linear algebra operations, originally written to verify exercises. It includes functions for matrix decomposition, finding inverses, and checking matrix properties.
- **Plotting Functions:** `R/plot.R` contains functions to visualize 2x2 matrix transformations.

### Prime Number Functions
- **File:** `R/primes.R`
- **Description:** Functions related to prime numbers, including the Sieve of Eratosthenes for finding primes up to a given number and a function to find circular primes.

### Matrix Derivatives
- **File:** `matrix_derivatives.R`
- **Description:** Derivate Using Linear Algebra. This is a fun project inspired by this video [Video](https://www.youtube.com/watch?v=TgKwz5Ikpc8) by 3 Blue 1 Brown.

The idea is that the polynomials of any degree can be described as a matrix-vector multiplication. More precisely, a matrix ($\mathbf{D}$) representing the differentiation of any terms of a polynomial is premultiplied by the vector ($\mathbf{p}$) describing the terms of the polynomial.

**Differentiation Matrix:**
```math
\mathbf{D}=\frac{\mathbf{d}}{\mathbf{dx}}=\begin{bmatrix} 0 & 1 & 0 & 0 & 0 & \cdots \\ 0 & 0 & 2 & 0 & 0 & \cdots \\ 0 & 0 & 0 & 3 & 0 & \cdots \\ 0 & 0 & 0 & 0 & 4 & \cdots \\ 0 & 0 & 0 & 0 & 0 & \cdots \\ \vdots & \vdots & \vdots & \vdots & \ddots \end{bmatrix}
```

#### Example

An example of a third degree polynomial.
$$
f(x) = 13+x+3x^2+4x^3
$$

The derivative of the polynomial:
$$
\frac{\mathbf{d}}{\mathbf{dx}}(13+x+3x^2+4x^3)=0+1+6x+12x^2
$$

In matrix notation, the coefficients corresponding to any term are represented in a vector form, where row number are the degrees of the terms. First row corresponds to the constant part of the polynomial.
$$
\vec{\mathbf{p}}= \begin{bmatrix}13 \\1 \\3 \\4 \\0 \\\vdots \end{bmatrix}
$$

Calculation of $\mathbf{D} \vec{p}$:
$$
\begin{bmatrix} 0 & 1 & 0 & 0 & 0 & \cdots \\ 0 & 0 & 2 & 0 & 0 & \cdots \\ 0 & 0 & 0 & 3 & 0 & \cdots \\ 0 & 0 & 0 & 0 & 4 & \cdots \\ 0 & 0 & 0 & 0 & 0 & \cdots \\ \vdots & \vdots & \vdots & \vdots & \ddots \end{bmatrix} \begin{bmatrix}13 \\1 \\3 \\4 \\0 \\\vdots \end{bmatrix} = \begin{bmatrix}1 \\6 \\12 \\0 \\0 \\\vdots \end{bmatrix} =\begin{bmatrix}\text{Constant} \\\text{1st Order Term} \\\text{2nd Order Term} \\\text{3rd Order Term}\\\text{4th Order Term} \\\vdots \end{bmatrix}
$$
