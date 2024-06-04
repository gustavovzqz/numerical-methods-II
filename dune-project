(lang dune 3.14)

(name numerical_methods)

(generate_opam_files true)

(source
 (github gustavovzqz/numerical-methods-II))

(authors "Gustavo Vazquez")

(maintainers "Gustavo Vazquez")

(license MIT)

(package
 (name numerical_methods)
 (synopsis "Implementations of Numerical Methods in OCaml")
 (description
  "A library providing implementations of fundamental numerical methods, with a focus on linear algebra. Developed for a university course, this library includes tools for matrix operations, solving linear systems, eigenvalue problems, and other essential numerical techniques. It serves as an educational resource for students and instructors in scientific computing and numerical analysis.")
 (depends ocaml dune)
 (tags
  (topics
   "numerical methods"
   "linear algebra"
   "scientific computing"
   "numerical analysis"
   "OCaml"
   "university project"
   "matrix operations"
   "eigenvalues")))
