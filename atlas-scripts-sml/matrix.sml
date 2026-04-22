use "atlas-scripts-sml/MatrixAT.sml";

(* 
  File: atlas-scripts-sml/matrix.sml

  Purpose
  - Compatibility shim matching the `matrix.at` module name used by Atlas scripts.
  - In this SML port, the actual implementation lives in `MatrixAT`.
*)
structure Matrix = MatrixAT
