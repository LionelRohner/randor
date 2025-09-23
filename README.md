# randor
Random R functions for everyday use ( ͡° ͜ʖ ͡°)
# 2DListComprehension
An approach to emulate Python's 2D list comprehension in R. 

Based on comprehenr : https://cran.r-project.org/web/packages/comprehenr/index.html
# EstimatePi
Estimate π from uniformly drawn points within a square of length 1. π is estimated as the ratio of points within a unit circle within the square divided to all points in the square.

### Empirical Method

**The Idea:** 
Create uniformly distributed points between 0 and 1 and consider them as vectors pointing to this random coordinate. Then count the vectors that have a vector norm of less than 1, i.e. the points inside the unit circle. The ratio of points inside to points outside the unit circle is an approximation to π/4. The accuracy of the estimate depends on the number of uniform points generated. Below 1e6, the accuracy is quite poor.

### Resampling Method

**The Idea:** 
The idea is to avoid generating a large number of points to get an accurate approximation of pi. Instead, we compute rather imprecise estimates of the ratio of the points inside and outside the unit circle and generate a beta or gamma distribution of ratios. The parameter estimate of the beta distribution is based on the method-of-moments to find initial values for alpha and beta in ~ Beta(alpha, beta). Once the distribution is fixed, we sample from that distribution and take the mean of the distribution, which should be a good estimate of pi.

This method has many parameters, such as the number of points to calculate a ratio, the number of initial ratios to generate, and the number of ratios to draw from the newly created distribution, and so some optimization is required (grid search or something more intelligent?). But at the moment, this method is neither more accurate nor faster than the empirical method...

### MCMC-like Method

MCMC-like algorithm, but instead of the Hastings ratio I used the accuracy measure, which kind of defeats the purpose of the MCMC algorithm, since it is used when the true value is unknown. But heck, it's just for fun, right? A newer implementation has the Hastings ratio with a uniform proposal kernel (cancels out).

**The Idea:**
1.) Create a prior distribution (beta distribution of unit circle ratio (same as in the resampling method))
2.) Propose first move (start with mean of prior)
3.) Initiate chain
4.) Propose a move with uniform proposal kernel
5.) Compute hastings-ratio H, accept if H >= than Uniform([0,1]). Alternatively, Compute accuracy, accept move if accuracy is better else stay.
6.) End chain at maxIter.

# LAFNAF
<ins>L</ins>inear <ins>A</ins>lgebra <ins>F</ins>unctions That <ins>N</ins>obody <ins>A</ins>sked <ins>F</ins>or, But Here They Are.

Originally, I wrote these functions to verify some linear algebra exercises. As the number of functions grew, I created this repository. The functions are not efficient (it's plain R-code) nor have they been thoroughly tested. Besides, all these functions have been implemented more efficiently in other packages or even in base R. It is just a fun exercise for me to get a better understanding of linear algebra.

### To Do List:
* The **singular_value_decomposition** function does not work correctly as the eigenvectors produced by R have a random sign, thus the matrix product **PDQ^-1** (corresponds to **UΣV^-1**) does not reconstitute **A**. Solution: Implement sign-flip function from https://digital.library.unt.edu/ark:/67531/metadc900575/m2/1/high_res_d/920802.pdf.
* ~~Reduced row echelon forms for centering matrice, probably due to floating point errors. No, but it works now.~~
* ~~The **rref** function does not return the reduced row echelon form, since it does not set free variables, which are located above a pivot to zero. Solution: Repeat Gaussian Elimination for free variables.~~
* ~~The **generalized_inverse** function, I am still trying to find a way to find a nonsingular matrix in **A** without using the builtin function **qr**. Once rref works fine, this can be used to find rank r and consequetnly to find a submatrix by checking, which rxr submatrix has a determinant > 0.~~
* ~~Once **rref** works, rewrite **rank_Matrix**.~~

### Main Functions:

* **create_basis:** Create a set of linear independent vectors that span a real vector-space.
*  **is_pos_def:** Checks whether a matrix **A** is positive definite.
*  **mat_pow:** Calculate powers of some matrix **A**.
*  **canonical_form:** Compute the decomposition of a matrix **A** into **UDU^-1**, where **U** is the matrix of eigenvectors and the diagonal matrix **D**, the similar canonical form, containing the eigenvalues of **A**.
*  **fast_exp:** Fast exponentiation using the similar canonical form.
*  **lin_dep_Cautchy_Schwartz:** Checks for the Cautchy-Schwartz-Inequality between two vectors or between rows/cols of matrix to check for linear dependence.
*  **adjugate:** Creates an adjugate matrix of some matrix **A**.
*  **generalized_inverse:** Create generalized inverses of any matrix **A**. Needs more testing.
*  **check_Penrose_cond:** Check for the four Penrose condition for matrix inverses, i.e. A = AMA, M = MAM, AM = (AM)^T, MA = (MA)^T
*  **inverse:** Creates an inverse of a nonsingular matrix **A**.
*  **orthogonalize:** Create an orthogonal matrix from some matrix **A**. Used for my SVD implementation (computations of AA^T and A^TA).
*  **rank_matrix:** Compute rank of matrix.
*  **singular_value_decomposition:** Does not work yet, I need to solve the problem of the sign ambiguity of eigenvector calculations.
*  **find_zero_vectors:** Find zero-vectors in a matrix using vector norm.
*  **ref:** Reduce some matrix **A** to row echelon form. Used for **rank_Matrix**, because its quicker than **rref**.
*  **rref:** Reduce some matrix **A** to reduced row echelon form. Validated to some extent... Needs more testing.

### Plot Functions:
*  **plot_eigenvec:** Plots eigenvalues of a 2x2 matrix.
*  **plot_matrix_transformation:** Plots vectors **x** and **y** as in **Ax=y**, where **A** is 2x2.

### Auxiliary Functions:
* **compare_floats:** Compare equality of floats. Principle: Compare absolute difference of two floats to a tolerance value (default = 1e-06). 
*  **swap:** Swap rows in a some matrix **A**. Used in **rref**.
*  **add_to_bottom:** Add rows to the bottom of a matrix **A**. Used in **rref**.
*  **col_is_all_zero:** Check (TRUE/FALSE) whether a column is all zeros. Used in **rref**.
*  **gaussian_elimination:** Performs Gaussian elimination on non-zero rows of a matrix. Used in **rref**.
*  **swap_zero_vectors:** Combination of **find_Zero_Vectors** and **add_to_bottom** used to rearrange rows in **rref**.
*  **remove_parallel_vectors:** Remove parallel vectors (rows or cols) in a matrix based on the Cautchy Schwartz Equality. Used in **rref**.
### MatrixDerivatives
Derivate Using Linear Algebra. This is a fun project inspired by this video [Video](https://www.youtube.com/watch?v=TgKwz5Ikpc8) by 3 Blue 1 Brown.

[![purple-pi](https://img.shields.io/badge/Rendered%20with-Purple%20Pi-bd00ff?style=flat-square)](https://github.com/nschloe/purple-pi?activate) 

The idea is that the polynomials of any degree (e.g. ) can be described as a matrix-vector multiplication. More precisely, a matrix (**D**) representing the differentiation of any terms of a polynomial is premultiplied by the vector (**p**) describing the terms of the polynomial.

Differentiation Matrix :
<img src="https://latex.codecogs.com/svg.image?\textbf{D}=\frac{\textbf{d}}{\textbf{dx}}=\begin{bmatrix}&space;0&&space;&space;1&&space;&space;0&&space;&space;0&space;&&space;0&\cdots&space;\\&space;0&&space;&space;0&&space;&space;2&&space;&space;0&space;&&space;0&\cdots&space;\\&space;0&&space;&space;0&&space;&space;0&&space;&space;3&space;&&space;0&\cdots&space;\\&space;0&&space;&space;0&&space;&space;0&&space;&space;0&space;&&space;4&\cdots&space;\\&space;0&&space;&space;0&&space;&space;0&&space;&space;0&space;&&space;0&\cdots&space;\\&space;&space;\vdots&space;&&space;\vdots&space;&&space;\vdots&space;&&space;\vdots&space;&&space;\ddots&space;\end{bmatrix}" title="\textbf{D}=\frac{\textbf{d}}{\textbf{dx}}=\begin{bmatrix} 0& 1& 0& 0 & 0&\cdots \\ 0& 0& 2& 0 & 0&\cdots \\ 0& 0& 0& 3 & 0&\cdots \\ 0& 0& 0& 0 & 4&\cdots \\ 0& 0& 0& 0 & 0&\cdots \\ \vdots & \vdots & \vdots & \vdots & \ddots \end{bmatrix}" />

# Example

An example of a third degree polynomial.

<img src="https://latex.codecogs.com/svg.image?f(x)&space;=&space;13&plus;x&plus;3x^2&plus;4x^3" title="f(x) = 13+x+3x^2+4x^3" />

The derivative of the polynomial:

<img src="https://latex.codecogs.com/svg.image?\frac{\textbf{d}}{\textbf{dx}}(13&plus;x&plus;3x^2&plus;4x^3)=0&plus;1&plus;6x&plus;12x^2" title="\frac{\textbf{d}}{\textbf{dx}}(13+x+3x^2+4x^3)=0+1+6x+12x^2" />


In matrix notation, the coefficients corresponding to any term are represented in a vector form, where row number are the degrees of the terms. First row corresponds to the constant part of the polynomial.

<img src="https://latex.codecogs.com/svg.image?\vec{\textbf{p}}=&space;\begin{bmatrix}13&space;\\1&space;\\3&space;\\4&space;\\0&space;\\\vdots&space;\end{bmatrix}" title="\vec{\textbf{p}}= \begin{bmatrix}13 \\1 \\3 \\4 \\0 \\\vdots \end{bmatrix}" />

Calculation of <img src="https://latex.codecogs.com/svg.image?\textbf{D}&space;\vec{p}" title="\textbf{D} \vec{p}" />
In matrix notation, the coefficients corresponding to any term are represented in a vector form, where row number are the degrees of the terms. First row corresponds to the constant part of the polynomial.

<img src="https://latex.codecogs.com/svg.image?\vec{\textbf{p}}=&space;\begin{bmatrix}13&space;\\1&space;\\3&space;\\4&space;\\0&space;\\\vdots&space;\end{bmatrix}" title="\vec{\textbf{p}}= \begin{bmatrix}13 \\1 \\3 \\4 \\0 \\\vdots \end{bmatrix}" />

Calculation of <img src="https://latex.codecogs.com/svg.image?\textbf{D}&space;\vec{p}" title="\textbf{D} \vec{p}" />

<img src="https://latex.codecogs.com/svg.image?\begin{bmatrix}&space;0&&space;1&&space;0&&space;0&space;&&space;0&\cdots&space;\\&space;0&&space;0&&space;2&&space;0&space;&&space;0&\cdots&space;\\&space;0&&space;0&&space;0&&space;3&space;&&space;0&\cdots&space;\\&space;0&&space;0&&space;0&&space;0&space;&&space;4&\cdots&space;\\&space;0&&space;0&&space;0&&space;0&space;&&space;0&\cdots&space;\\&space;\vdots&space;&&space;\vdots&space;&&space;\vdots&space;&&space;\vdots&space;&&space;\ddots&space;\end{bmatrix}&space;\begin{bmatrix}13&space;\\1&space;\\3&space;\\4&space;\\0&space;\\\vdots&space;\end{bmatrix}&space;=&space;\begin{bmatrix}1&space;\\6&space;\\12&space;\\0&space;\\0&space;\\\vdots&space;\end{bmatrix}&space;=\begin{bmatrix}\text{Constant}&space;\\\text{1st&space;Order&space;Term}&space;\\\text{2nd&space;Order&space;Term}&space;\\\text{3rd&space;Order&space;Term}\\\text{4th&space;Order&space;Term}&space;\\\vdots&space;\end{bmatrix}" title="\begin{bmatrix} 0& 1& 0& 0 & 0&\cdots \\ 0& 0& 2& 0 & 0&\cdots \\ 0& 0& 0& 3 & 0&\cdots \\ 0& 0& 0& 0 & 4&\cdots \\ 0& 0& 0& 0 & 0&\cdots \\ \vdots & \vdots & \vdots & \vdots & \ddots \end{bmatrix} \begin{bmatrix}13 \\1 \\3 \\4 \\0 \\\vdots \end{bmatrix} = \begin{bmatrix}1 \\6 \\12 \\0 \\0 \\\vdots \end{bmatrix} =\begin{bmatrix}\text{Constant} \\\text{1st Order Term} \\\text{2nd Order Term} \\\text{3rd Order Term}\\\text{4th Order Term} \\\vdots \end{bmatrix}" />


